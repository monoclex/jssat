use crate::frontend::{assembler::Program, ir::IR, type_annotater::SymbolicEngine};

pub mod llvm;
pub mod skeleton;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(program: Program) -> BuildArtifact {
    todo!()
    // let backend_ir = skeleton::translate(ir, annotations);
    // llvm::compile(backend_ir)
}
