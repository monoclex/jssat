use crate::backend::skeleton::ir::{self as s, TypeManager};
use crate::frontend::js::ir as j;

use super::types::ir::TypeAnnotations;

pub struct GlueArtifact(pub s::IR, pub s::TypeManager);

pub fn glue(ir: j::IR, type_annotations: TypeAnnotations) -> GlueArtifact {
    // let type_manager = TypeManager::new();
    todo!()
}
