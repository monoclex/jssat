use inkwell::context::Context;

use crate::frontend::{ir::IR, type_annotater::TypeAnnotations};

// use self::{llvm::compiler::BackendCompiler, runtime_glue::RuntimeGlue, skeleton::ir::*};

pub mod llvm;
mod runtime_glue;
pub mod skeleton;

// use skeleton::compiler::SkeletonCompiler;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(ir: &IR, annotations: &TypeAnnotations) -> BuildArtifact {
    let backend_ir = skeleton::translate(ir, annotations);
    llvm::compile(backend_ir)

    // let context = Context::create();
    // let builder = context.create_builder();
    // let module = context.create_module("jssat");
    // let glue = RuntimeGlue::new(&context, &module);
    // let mut monomorphizer = LLVMMonomorphizer::new(&type_info, &glue);

    // let mut skeleton_compiler = SkeletonCompiler {
    //     ir,
    //     type_info,
    //     monomorphizer: &mut monomorphizer,
    //     context: &context,
    //     // builder: &builder,
    //     module: &module,
    //     glue: &glue,
    // };

    // let skeleton_artifact = skeleton_compiler.compile();

    // let backend_compiler = BackendCompiler {
    //     ir,
    //     type_info,
    //     context: &context,
    //     builder: &builder,
    //     module: &module,
    //     glue: &glue,
    //     functions: &skeleton_artifact.functions,
    //     globals: &skeleton_artifact.globals,
    // };

    // let backend_artifact = backend_compiler.compile();

    // backend_artifact
}
