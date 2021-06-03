use std::collections::HashMap;

use crate::id::*;

#[derive(Debug, Clone)]
pub struct TypesArtifact {
    pub annotations: TypeAnnotations,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotations {
    pub functions: HashMap<TopLevelId, FunctionContext>,
}

#[derive(Debug, Clone)]
pub struct FunctionContext {
    pub return_type: TypeId,
    pub registers: HashMap<RegisterId, TypeId>,
    pub types: TypeManager,
}

#[derive(Debug, Clone)]
pub struct TypeManager {
    pub(super) types: HashMap<TypeId, Type>,
}

impl TypeManager {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn get(&self, id: &TypeId) -> &Type {
        self.types.get(id).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Any,
    // TODO: generic types, union types, sum types, dependent types, fun fun yum yum!
    Void,
    // Never,
}
