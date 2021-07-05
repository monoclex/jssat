use crate::frontend::{assembler::Program};

pub mod llvm;
pub mod skeleton;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(_program: Program) -> BuildArtifact {
    todo!()
    // let backend_ir = skeleton::translate(ir, annotations);
    // llvm::compile(backend_ir)
}
