use crate::frontend::assembler::Program;

pub mod llvm;
pub mod skeleton;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(program: Program) -> BuildArtifact {
    let backend_ir = skeleton::translate(program);
    llvm::compile(backend_ir)
}
