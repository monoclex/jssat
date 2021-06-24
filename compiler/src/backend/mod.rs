use crate::frontend::{ir::IR, type_annotater::TypeAnnotations};

pub mod llvm;
pub mod skeleton;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(ir: &IR, annotations: &TypeAnnotations) -> BuildArtifact {
    let backend_ir = skeleton::translate(ir, annotations);
    llvm::compile(backend_ir)
}
