use std::{collections::HashMap, convert::TryInto, u64};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{AnyType, ArrayType, BasicTypeEnum, IntType, StructType, VectorType},
    values::{
        ArrayValue, BasicValue, BasicValueEnum, FunctionValue, GlobalValue, IntValue, PointerValue,
        VectorValue,
    },
    AddressSpace, OptimizationLevel,
};

use crate::{
    ir::{
        Callable, Constant, GlobalVariable, Instruction, RegisterId, TopLevelId, Type, Value, IR,
    },
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
        builder: &builder,
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
    builder: &'compilation Builder<'compilation>,
    module: &'module Module<'compilation>,
    glue: &'module RuntimeGlue<'compilation, 'module>,
}

struct SkeletonArtifact<'module> {
    types: HashMap<TypeId, StructType<'module>>,
    globals: HashMap<TopLevelId, GlobalValue<'module>>,
    functions: HashMap<TopLevelId, FunctionValue<'module>>,
}

impl<'c, 'm> SkeletonCompiler<'c, 'm> {
    pub fn compile(&self) -> SkeletonArtifact<'m> {
        todo!()
    }
}

struct BackendCompiler<'compilation, 'module> {
    ir: &'compilation IR,
    type_info: &'compilation TypeAnnotations,
    context: &'compilation Context,
    builder: &'compilation Builder<'compilation>,
    module: &'module Module<'compilation>,
    glue: &'module RuntimeGlue<'compilation, 'module>,
    types: &'module HashMap<TypeId, StructType<'module>>,
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

// fn compile(llvm_module: inkwell::module::Module) -> BuildArtifact {
//     let text_buff = llvm_module.print_to_string().to_string();

//     // TODO: return a lambda to compile? this would prevent the hardcoded message since
//     // we'd just call the lambda to compile. the reasoning behind wanting a lambda is because
//     // i segfaulted once during the compilation to machine code step
//     #[cfg(debug_assertions)]
//     println!("-> LLVM IR: {}", text_buff);

//     Target::initialize_all(&Default::default());

//     let target_triple = TargetMachine::get_default_triple();

//     let target = Target::from_triple(&target_triple).unwrap();

//     let target_machine = target
//         .create_target_machine(
//             &target_triple,
//             "generic",
//             "",
//             OptimizationLevel::Aggressive,
//             RelocMode::PIC,
//             CodeModel::Default,
//         )
//         .expect("couldn't make target machine");

//     let obj_buff = target_machine
//         .write_to_memory_buffer(&llvm_module, FileType::Object)
//         .expect("couldn't compile to assembly");

//     BuildArtifact {
//         llvm_ir: text_buff.to_string(),
//         obj: obj_buff.as_slice().to_vec(),
//     }
// }

// pub fn build(ir: &IR, type_info: &TypeAnnotations) -> BuildArtifact {
//     let mut types = HashMap::new();

//     let context = Context::create();
//     let builder = context.create_builder();
//     let llvm_module = context.create_module("jssat");
//     let glue = RuntimeGlue::new(&context, &llvm_module);

//     build_struct_types(type_info, &context, &glue, &mut types);
//     let ext_fns = declare_external_functions(ir, &context, type_info, &glue, &types, &llvm_module);
//     let fns = declare_functions(
//         ir,
//         type_info,
//         &context,
//         &glue,
//         &types,
//         &llvm_module,
//         &builder,
//     );
//     let constants = {
//         let mut constants = HashMap::new();

//         for (id, c) in ir.constants.iter() {
//             let raw_const = context.i8_type().const_array(
//                 c.payload
//                     .iter()
//                     .map(|n| context.i8_type().const_int(*n as u64, false))
//                     .collect::<Vec<_>>()
//                     .as_slice(),
//             );
//             let global = llvm_module.add_global(
//                 context.i8_type().array_type(c.payload.len() as u32),
//                 Some(AddressSpace::Generic),
//                 "",
//             );
//             // copy C
//             global.set_linkage(Linkage::Private);
//             global.set_unnamed_addr(true);
//             global.set_constant(true);
//             global.set_initializer(&raw_const);

//             constants.insert(id, global);
//         }

//         constants
//     };

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

// fn build_struct_types<'c>(
//     type_info: &TypeAnnotations,
//     context: &'c Context,
//     glue: &RuntimeGlue<'c, '_>,
//     types: &mut HashMap<crate::types::TypeId, inkwell::types::PointerType<'c>>,
// ) {
//     for (key, kind) in type_info.type_map.iter() {
//         assert!(matches!(kind, Type::Any));
//         types.insert(*key, glue.type_value);
//     }
// }

// fn declare_external_functions<'c, 'm>(
//     ir: &IR,
//     context: &'c Context,
//     type_info: &TypeAnnotations,
//     glue: &RuntimeGlue<'c, 'm>,
//     types: &'c HashMap<crate::types::TypeId, inkwell::types::PointerType>,
//     llvm_module: &inkwell::module::Module<'c>,
// ) -> HashMap<TopLevelId, FunctionValue<'c>> {
//     let mut fns = HashMap::new();

//     for function in ir.external_functions.iter() {
//         let name = ir
//             .debug_info
//             .top_level_names
//             .get(&function.id)
//             .map(|n| format!("{}", n))
//             .unwrap_or(format!(".{}", function.id.value()));

//         let raw_fn_params = type_info
//             .external_functions
//             .get(&function.id)
//             .unwrap()
//             .parameter_annotations
//             .iter()
//             .map(|id| types.get(id).unwrap())
//             .map(|t| BasicTypeEnum::PointerType(*t));

//         let fn_type = context.void_type().fn_type(
//             std::iter::once(BasicTypeEnum::PointerType(
//                 glue.type_runtime_value.ptr_type(AddressSpace::Generic),
//             ))
//             .chain(raw_fn_params)
//             .collect::<Vec<_>>()
//             .as_slice(),
//             false,
//         );

//         // TODO: review if `ExternalWeak` is right
//         let llvm_fn = llvm_module.add_function(name.as_str(), fn_type, Some(Linkage::ExternalWeak));
//         fns.insert(function.id, llvm_fn);
//     }

//     fns
// }

// fn declare_functions<'c, 'm>(
//     ir: &IR,
//     type_info: &TypeAnnotations,
//     context: &'c Context,
//     glue: &RuntimeGlue<'c, 'm>,
//     types: &HashMap<crate::types::TypeId, inkwell::types::PointerType<'c>>,
//     llvm_module: &inkwell::module::Module<'c>,
//     builder: &inkwell::builder::Builder<'c>,
// ) -> HashMap<TopLevelId, FunctionValue<'c>> {
//     let mut fns = HashMap::new();

//     for function in ir.functions.iter() {
//         let name = ir
//             .debug_info
//             .top_level_names
//             .get(&function.id)
//             .map(|n| format!("{}", n))
//             .unwrap_or(format!(".{}", function.id.value()));

//         let annotations = type_info.function_annotations.get(&function.id).unwrap();

//         let raw_fn_params = function
//             .parameters
//             .iter()
//             .map(|p| annotations.register_annotations.get(&p.register).unwrap())
//             .map(|id| types.get(id).unwrap())
//             .map(|t| BasicTypeEnum::PointerType(*t));

//         let mut fn_params = std::iter::once(BasicTypeEnum::PointerType(
//             glue.type_runtime_value.ptr_type(AddressSpace::Generic),
//         ))
//         .chain(raw_fn_params)
//         .collect::<Vec<_>>();

//         // all parameters have an implicit `runtime` parameter except main
//         if function.is_main {
//             fn_params.remove(0);
//         }

//         let fn_type = context.void_type().fn_type(fn_params.as_slice(), false);

//         let llvm_fn = match function.is_main {
//             true => llvm_module.add_function("main", fn_type, Some(Linkage::External)),
//             false => llvm_module.add_function(name.as_str(), fn_type, None),
//         };

//         fns.insert(function.id, llvm_fn);
//     }

//     fns
// }
