use std::collections::HashMap;

use super::ir::{TypeAnnotations, TypesArtifact};
use crate::frontend::js::ir::IR;

pub fn compile(ir: &IR) -> TypesArtifact {
    let mut annotations = TypeAnnotations {
        functions: HashMap::new(),
    };

    for (_, function) in ir.functions.iter() {}

    TypesArtifact { annotations }
}
