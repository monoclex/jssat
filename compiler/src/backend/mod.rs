pub mod llvm;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn compile(_program: ()) -> BuildArtifact {
    todo!()
    // let backend_ir = skeleton::translate(program);
    // llvm::compile(backend_ir)
}
