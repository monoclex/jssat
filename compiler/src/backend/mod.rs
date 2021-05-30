use std::{borrow::Cow, collections::HashMap, u64};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    ir::{Constant, ExternalFunction, Function, TopLevelId, Type, IR},
    types::{TypeAnnotations, TypeId},
};

use self::runtime_glue::RuntimeGlue;

mod runtime_glue;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(ir: &IR, type_info: &TypeAnnotations) -> BuildArtifact {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("jssat");
    let glue = RuntimeGlue::new(&context, &module);

    let skeleton_compiler = SkeletonCompiler {
        ir,
        type_info,
        context: &context,
        // builder: &builder,
        module: &module,
        glue: &glue,
    };

    let skeleton_artifact = skeleton_compiler.compile();

    let backend_compiler = BackendCompiler {
        ir,
        type_info,
        context: &context,
        builder: &builder,
        module: &module,
        glue: &glue,
        functions: &skeleton_artifact.functions,
        globals: &skeleton_artifact.globals,
        types: &skeleton_artifact.types,
    };

    let backend_artifact = backend_compiler.compile();

    backend_artifact
}

struct SkeletonCompiler<'compilation, 'module> {
    ir: &'compilation IR,
    type_info: &'compilation TypeAnnotations,
    context: &'compilation Context,
    // builder: &'compilation Builder<'compilation>,
    module: &'module Module<'compilation>,
    glue: &'module RuntimeGlue<'compilation, 'module>,
}

struct SkeletonArtifact<'compilation> {
    // TODO: encapsulate the `HashMap<K, V>` structure of these types into something?
    // i ran into having to update all the type signatures when updating this, so
    // leaving this comment here to consider if i should put this into its own struct.
    types: HashMap<TypeId, AnyTypeEnum<'compilation>>,
    globals: HashMap<TopLevelId, GlobalValue<'compilation>>,
    functions: HashMap<TopLevelId, FunctionValue<'compilation>>,
}

impl<'c, 'm> SkeletonCompiler<'c, 'm> {
    pub fn compile(&self) -> SkeletonArtifact<'c> {
        let mut artifact = SkeletonArtifact {
            types: self.populate_types(),
            globals: HashMap::new(),
            functions: HashMap::new(),
        };

        for (&id, constant) in self.ir.constants.iter() {
            let global = self.populate_constant(id, constant);
            artifact.globals.insert(id, global);
        }

        for (&id, ext_func) in self.ir.external_functions.iter() {
            let llvm_fn = self.populate_external_function(id, ext_func, &artifact.types);
            artifact.functions.insert(id, llvm_fn);
        }

        for (&id, func) in self.ir.functions.iter() {
            let llvm_fn = self.populate_function(id, func, &artifact.types);
            artifact.functions.insert(id, llvm_fn);
        }

        artifact
    }

    // TODO: should this be in the skeleton compiler? as really, we need
    // full type info before making skeletons of `globals` and `functions`, but at that
    // point, it feels like a lot of boilerplate to have a `TypeCompiler` state.
    fn populate_types(&self) -> HashMap<TypeId, AnyTypeEnum<'c>> {
        let mut types = HashMap::new();

        for (&id, r#type) in self.type_info.type_map.iter() {
            let struct_type = self.create_type(id, r#type);
            types.insert(id, struct_type);
        }

        types
    }

    // TODO: see the todo for `populate_types`
    fn create_type(&self, _id: TypeId, r#type: &Type) -> AnyTypeEnum<'c> {
        match r#type {
            Type::Any => self.glue.type_value.as_any_type_enum(),
            Type::Void => self.context.void_type().as_any_type_enum(),
            Type::Runtime => self.glue.type_runtime.as_any_type_enum(),
            // TODO: analyze why we need to handle this case
            Type::Constant(_) => panic!("a constant should not be present in the `TypeId`s"),
        }

        // let name = format!(".{}", id);

        // let opaque_struct = self.context.opaque_struct_type(name.as_str());
    }

    fn populate_constant(&self, id: TopLevelId, constant: &Constant) -> GlobalValue<'c> {
        let name = (self.ir.debug_info.top_level_names.get(&id))
            .map(|s| s.as_ref())
            .unwrap_or("");

        let raw_const = self.make_constant_value(&constant);

        let global =
            self.module
                .add_global(raw_const.get_type(), Some(AddressSpace::Generic), name);

        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        global.set_constant(true);
        global.set_initializer(&raw_const);

        global
    }

    fn make_constant_value(&self, constant: &Constant) -> BasicValueEnum<'c> {
        match std::str::from_utf8(constant.payload.as_slice()) {
            Ok(str) => self
                .context
                .const_string(str.as_bytes(), false)
                .as_basic_value_enum(),
            Err(_) => {
                let byte_values = constant
                    .payload
                    .iter()
                    .map(|n| self.context.i8_type().const_int(*n as u64, false))
                    .collect::<Vec<_>>();

                self.context
                    .i8_type()
                    .const_array(byte_values.as_slice())
                    .as_basic_value_enum()
            }
        }
    }

    // TODO: somehow unify `populate_external_function` and `populate_function`, as they are very similar
    fn populate_external_function(
        &self,
        id: TopLevelId,
        _ext_func: &ExternalFunction,
        types: &HashMap<TypeId, AnyTypeEnum<'c>>,
    ) -> FunctionValue<'c> {
        let name = (self.ir.debug_info.top_level_names.get(&id))
            .map(|n| Cow::Borrowed(n.as_ref()))
            .unwrap_or_else(|| Cow::Owned(format!(".{}", id.value())));

        let annotations = (self.type_info.external_functions.get(&id))
            .expect("expected type information for external function");

        // TODO: this should really be encapsulated (repeated in this code quite a bit)
        // or perhaps we could include the type annotations with the external function in the first place?
        //
        // we pass along the `ext_func` but do nothing with it, it should really have the types in it
        let llvm_ret = types
            .get(&annotations.return_type)
            .expect("Expected `AnyTypeEnum` present for `TypeId`");

        let llvm_params = (annotations.parameter_annotations.iter())
            .map(|id| {
                types
                    .get(id)
                    .expect("Expected `AnyTypeEnum` present for `TypeId`")
            })
            .map(|a| LLVMAnyWrapper(a).coerce_basic())
            .collect::<Vec<_>>();

        let fn_type =
            LLVMAnyWrapper(&llvm_ret.as_any_type_enum()).fn_type(llvm_params.as_slice(), false);

        // TODO: review if `ExternalWeak` is right
        self.module
            .add_function(&name, fn_type, Some(Linkage::ExternalWeak))
    }

    fn populate_function(
        &self,
        id: TopLevelId,
        func: &Function,
        types: &HashMap<TypeId, AnyTypeEnum<'c>>,
    ) -> FunctionValue<'c> {
        let name = (self.ir.debug_info.top_level_names.get(&id))
            .map(|n| Cow::Borrowed(n.as_ref()))
            .unwrap_or_else(|| Cow::Owned(format!(".{}", id.value())));

        let annotations = (self.type_info.function_annotations.get(&id))
            .expect("expected type information for function");

        // TODO: this should really be encapsulated (repeated in this code quite a bit)
        // or perhaps we could include the type annotations with the external function in the first place?
        //
        // we pass along the `ext_func` but do nothing with it, it should really have the types in it
        let llvm_ret = types
            .get(&annotations.return_annotation)
            .expect("Expected `AnyTypeEnum` present for `TypeId`");

        let mut llvm_params = (annotations.parameter_annotations.iter())
            .map(|id| {
                types
                    .get(id)
                    .expect("Expected `AnyTypeEnum` present for `TypeId`")
            })
            .map(|a| LLVMAnyWrapper(a).coerce_basic())
            .collect::<Vec<_>>();

        // all JSSAT functions have an implicit runtime parameter except for main
        let needs_runtime_parameter = !func.is_main;
        if needs_runtime_parameter {
            // TODO: the `types` parameter should be responsible for converting
            // `Type::Any` -> <runtime type>
            llvm_params.insert(0, self.glue.type_runtime.as_basic_type_enum());
        }

        let fn_type =
            LLVMAnyWrapper(&llvm_ret.as_any_type_enum()).fn_type(llvm_params.as_slice(), false);

        // TODO: review if `ExternalWeak` is right
        self.module
            .add_function(&name, fn_type, Some(Linkage::ExternalWeak))
    }
}

struct BackendCompiler<'compilation, 'module> {
    ir: &'compilation IR,
    type_info: &'compilation TypeAnnotations,
    context: &'compilation Context,
    builder: &'compilation Builder<'compilation>,
    module: &'module Module<'compilation>,
    glue: &'module RuntimeGlue<'compilation, 'module>,
    types: &'module HashMap<TypeId, AnyTypeEnum<'module>>,
    globals: &'module HashMap<TopLevelId, GlobalValue<'module>>,
    functions: &'module HashMap<TopLevelId, FunctionValue<'module>>,
}

impl BackendCompiler<'_, '_> {
    pub fn compile(&self) -> BuildArtifact {
        self.compile_llvm()
    }

    fn compile_llvm(&self) -> BuildArtifact {
        Target::initialize_all(&Default::default());
        let target_triple = TargetMachine::get_default_triple();

        let target = Target::from_triple(&target_triple).unwrap();

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .expect("couldn't make target machine");

        let text_buff = self.module.print_to_string().to_string();

        let obj_buff = target_machine
            .write_to_memory_buffer(&self.module, FileType::Object)
            .expect("couldn't compile to assembly");

        BuildArtifact {
            llvm_ir: text_buff.to_string(),
            obj: obj_buff.as_slice().to_vec(),
        }
    }
}

struct TypeCompiler<'compilation, 'module, 'types> {
    ir: &'compilation IR,
    type_info: &'compilation TypeAnnotations,
    context: &'compilation Context,
    builder: &'compilation Builder<'compilation>,
    module: &'module Module<'compilation>,
    glue: &'module RuntimeGlue<'compilation, 'module>,
    types: &'types HashMap<TypeId, StructType<'compilation>>,
}

impl<'c, 'm, 't> TypeCompiler<'c, 'm, 't> {}

struct LLVMAnyWrapper<'usage, 'ctx>(&'usage AnyTypeEnum<'ctx>);

impl<'c> LLVMAnyWrapper<'_, 'c> {
    pub fn fn_type(
        &self,
        param_types: &[BasicTypeEnum<'c>],
        is_var_args: bool,
    ) -> FunctionType<'c> {
        match self.0 {
            AnyTypeEnum::ArrayType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::FloatType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::FunctionType(t) => t
                .ptr_type(AddressSpace::Generic)
                .fn_type(param_types, is_var_args),
            AnyTypeEnum::IntType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::PointerType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::StructType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::VectorType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::VoidType(t) => t.fn_type(param_types, is_var_args),
        }
    }

    pub fn coerce_basic(&self) -> BasicTypeEnum<'c> {
        match self.0 {
            AnyTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::FunctionType(t) => {
                panic!("cannot coerce `FunctionType` to variant of `BasicTypeEnum`")
            }
            AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::StructType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::VectorType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::VoidType(t) => {
                panic!("cannot coerce `VoidType` to variant of `BasicTypeEnum`")
            }
        }
    }
}
// pub fn build(ir: &IR, type_info: &TypeAnnotations) -> BuildArtifact {
//     let mut types = HashMap::new();

//     let context = Context::create();
//     let builder = context.create_builder();
//     let llvm_module = context.create_module("jssat");
//     let glue = RuntimeGlue::new(&context, &llvm_module);

//     let mut all_fns = HashMap::new();
//     all_fns.extend(ext_fns.clone());
//     all_fns.extend(fns.clone());

//     let (ir_main_fn_id, _) = ir.functions.iter().find(|(_, f)| f.is_main).unwrap();
//     let llvm_main_fn = fns.get(&ir_main_fn_id).unwrap();

//     // TODO: remove the massive amount of nesting?
//     for i in ir.functions.iter() {
//         let fn_types = type_info.function_annotations.get(&i.id).unwrap();
//         let llvm_fn = fns.get(&i.id).unwrap();
//         let mut registers: HashMap<RegisterId, BasicValueEnum> = HashMap::new();

//         for (idx, param) in i.parameters.iter().enumerate() {
//             // all fuynctions have the first parameter being the runtime, except for main
//             let offset = if i.is_main { 0 } else { 1 };
//             registers.insert(
//                 param.register,
//                 llvm_fn.get_nth_param((idx + offset) as u32).unwrap(),
//             );
//         }

//         let mut last_block = None;

//         let runtime_param = match i.is_main {
//             false => llvm_fn.get_nth_param(0).unwrap(),
//             true => {
//                 // main function needs to setup the runtime
//                 let bb = context.append_basic_block(*llvm_fn, "runtime_init");
//                 builder.position_at_end(bb);

//                 last_block = Some(bb);

//                 println!("-> runtime");
//                 let runtime = builder.build_call(glue.fn_jssatrt_runtime_new, &[], "runtime");
//                 runtime.try_as_basic_value().unwrap_left()
//             }
//         };

//         for block in i.body.blocks.iter() {
//             let basic_block = context.append_basic_block(*llvm_fn, "");

//             if let Some(last) = last_block {
//                 builder.build_unconditional_branch(basic_block);
//             }

//             builder.position_at_end(basic_block);

//             for instruction in block.instructions.iter() {
//                 // TODO: extract this into its own function?
//                 match instruction {
//                     Instruction::Call(result, callable, args) => {
//                         enum Either<A, B> {
//                             Left(A),
//                             Right(B),
//                         }

//                         let fn_sig = match callable {
//                             Callable::GlobalFunction(id) => {
//                                 match (
//                                     type_info.function_annotations.get(id),
//                                     type_info.external_functions.get(id),
//                                 ) {
//                                     (Some(l), None) => Either::Left(l),
//                                     (None, Some(r)) => Either::Right(r),
//                                     _ => panic!(),
//                                 }
//                             }
//                             _ => todo!(),
//                         };
//                         let function = match callable {
//                             Callable::GlobalFunction(id) => all_fns.get(id).unwrap(),
//                             _ => todo!(),
//                         };

//                         let args = args
//                             .iter()
//                             .enumerate()
//                             .map(|(idx, v)| {
//                                 (
//                                     idx,
//                                     match v {
//                                         Value::Register(register) => (
//                                             *registers.get(register).unwrap(),
//                                             Either::Left(
//                                                 fn_types
//                                                     .register_annotations
//                                                     .get(register)
//                                                     .unwrap(),
//                                             ),
//                                         ),
//                                         Value::Constant(constant) => {
//                                             println!(
//                                                 "gettign constant: {:?}",
//                                                 constants.get(constant).unwrap()
//                                             );
//                                             (
//                                                 constants
//                                                     .get(constant)
//                                                     .unwrap()
//                                                     .as_basic_value_enum(),
//                                                 Either::Right(Type::List(
//                                                     Box::new(Type::Integer(8)),
//                                                     Some(
//                                                         ir.constants
//                                                             .iter()
//                                                             .find(|c| c.id == *constant)
//                                                             .unwrap()
//                                                             .payload
//                                                             .len(),
//                                                     ),
//                                                 )),
//                                             )
//                                         }
//                                         _ => {
//                                             // `;` for intellisense
//                                             todo!();
//                                         }
//                                     },
//                                 )
//                             })
//                             .map(|(idx, (value, kind))| {
//                                 // TODO: pull this into its own function
//                                 // type conversions
//                                 let kind = match &kind {
//                                     Either::Left(id) => type_info.type_map.get(id).unwrap(),
//                                     Either::Right(t) => t,
//                                 };

//                                 let param_type = match fn_sig {
//                                     Either::Left(l) => l.parameter_annotations.get(idx).unwrap(),
//                                     Either::Right(r) => r.parameter_annotations.get(idx).unwrap(),
//                                 };

//                                 // TODO: **proper** code to convert from arbitrary `kind` -> `param_type`
//                                 // for now this is just some hacky workarounds
//                                 let value = match kind {
//                                     Type::List(box Type::Integer(8), Some(size)) => {
//                                         // trying to turn a constant into a `Type::Any`
//                                         // `in_bounds_gep` works on `ArrayValue`s too
//                                         println!("converting: {:?}", value);
//                                         let p = match value {
//                                             BasicValueEnum::PointerValue(p) => p,
//                                             _ => panic!(),
//                                         };
//                                         println!("dealing with: {:?}", p);

//                                         // TODO: what am i doing
//                                         let gep = unsafe {
//                                             p.const_in_bounds_gep(&[
//                                                 context.i64_type().const_int(0, false),
//                                                 // context.i64_type().const_int(0, false),
//                                             ])
//                                         };
//                                         println!("gep -> {:?}", gep);
//                                         let gep = builder.build_bitcast(
//                                             gep,
//                                             context.i8_type().ptr_type(AddressSpace::Generic),
//                                             "",
//                                         );
//                                         println!("--> gep after {:?}", gep);

//                                         let args = &[
//                                             runtime_param,
//                                             gep.into(),
//                                             // TODO: use platform dependent size
//                                             context
//                                                 .i64_type()
//                                                 .const_int(*size as u64, false)
//                                                 .into(),
//                                         ];
//                                         println!(
//                                             "-> cast\n\tcall {:?}\n\tfn {:?}",
//                                             args, glue.fn_jssatrt_constant_new
//                                         );
//                                         let value = builder.build_call(
//                                             glue.fn_jssatrt_constant_new,
//                                             args,
//                                             "",
//                                         );

//                                         value.try_as_basic_value().unwrap_left()
//                                     }
//                                     _ => value,
//                                 };

//                                 value
//                             });

//                         let args_with_runtime = std::iter::once(runtime_param)
//                             .chain(args)
//                             .collect::<Vec<_>>();

//                         println!(
//                             "-> call {:?} fn {:?}",
//                             &args_with_runtime,
//                             &function.get_params()
//                         );
//                         let value = builder.build_call(*function, args_with_runtime.as_slice(), "");

//                         if let Some(result) = result {
//                             registers.insert(*result, value.try_as_basic_value().unwrap_left());
//                         }
//                     }
//                     Instruction::Ret(value) => {
//                         match value {
//                             Some(v) => {
//                                 let callsite_value = registers.get(v).unwrap();
//                                 let basic_value = callsite_value;
//                                 builder.build_return(Some(basic_value))
//                             }
//                             _ => builder.build_return(None),
//                         };
//                     }
//                     _ => todo!(),
//                 };
//             }

//             last_block = Some(basic_block);
//         }
//     }

//     compile(llvm_module)
// }
