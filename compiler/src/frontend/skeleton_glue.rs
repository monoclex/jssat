use crate::backend::skeleton::ir as s;
use crate::frontend::js::ir as j;

use super::types::ir::TypeAnnotations;

pub struct GlueArtifact<'c>(pub s::IR, pub s::TypeManager<'c>);

pub fn glue<'c>(ir: j::IR, type_annotations: TypeAnnotations) -> GlueArtifact<'c> {
    todo!()
}
