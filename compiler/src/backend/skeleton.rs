use crate::backend::llvm::BackendIR;
use crate::frontend::{ir::IR, type_annotater::TypeAnnotations};

pub fn translate<'ir>(ir: &'ir IR, annotations: &TypeAnnotations) -> BackendIR<'ir> {
    todo!()
}
