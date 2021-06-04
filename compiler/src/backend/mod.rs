use std::{borrow::Cow, collections::HashMap, num::NonZeroUsize, u64};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue},
    AddressSpace, OptimizationLevel,
};

use crate::frontend::types::ir::TypeAnnotations;

use self::{llvm::compiler::BackendCompiler, runtime_glue::RuntimeGlue, skeleton::ir::*};

pub mod llvm;
mod runtime_glue;
pub mod skeleton;

use crate::id::*;
use skeleton::compiler::{SkeletonArtifact, SkeletonCompiler};

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(ir: &IR, type_info: &TypeManager) -> BuildArtifact {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("jssat");
    let glue = RuntimeGlue::new(&context, &module);
    let mut monomorphizer = LLVMMonomorphizer::new(&type_info, &glue);

    let mut skeleton_compiler = SkeletonCompiler {
        ir,
        type_info,
        monomorphizer: &mut monomorphizer,
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
    };

    let backend_artifact = backend_compiler.compile();

    backend_artifact
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

//     // TxODO: remove the massive amount of nesting?
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
//                 // TxODO: extract this into its own function?
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
//                                 // TxODO: pull this into its own function
//                                 // type conversions
//                                 let kind = match &kind {
//                                     Either::Left(id) => type_info.type_map.get(id).unwrap(),
//                                     Either::Right(t) => t,
//                                 };

//                                 let param_type = match fn_sig {
//                                     Either::Left(l) => l.parameter_annotations.get(idx).unwrap(),
//                                     Either::Right(r) => r.parameter_annotations.get(idx).unwrap(),
//                                 };

//                                 // TxODO: **proper** code to convert from arbitrary `kind` -> `param_type`
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

//                                         // TxODO: what am i doing
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
//                                             // TxODO: use platform dependent size
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
