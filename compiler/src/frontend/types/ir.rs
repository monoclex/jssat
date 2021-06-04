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

// TODO: separate out TypeManager responsibility
// currently, TypeManager = TypeBuilder + TypeContainer
//
// e.g. we don't need `free_id` or `inserted_types` when in storage
#[derive(Debug, Clone)]
pub struct TypeManager {
    free_id: TypeId,
    rev_types: HashMap<Type, TypeId>,
    pub(super) types: HashMap<TypeId, Type>,
}

impl TypeManager {
    pub fn new() -> Self {
        Self {
            free_id: TypeId::new(),
            rev_types: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn id(&mut self, r#type: &Type) -> TypeId {
        if let Some(id) = self.rev_types.get(&r#type) {
            *id
        } else {
            let id = self.free_id;
            self.free_id = self.free_id.next();

            self.rev_types.insert(r#type.clone(), id);
            self.types.insert(id, r#type.clone());
            id
        }
    }

    // TODO: this method's entire existence is a hack
    pub fn id_cache(&self, r#type: &Type) -> TypeId {
        if let Some(id) = self.rev_types.get(&r#type) {
            *id
        } else {
            panic!("expected cached id for {:?}", r#type);
        }
    }

    pub fn get(&self, id: &TypeId) -> &Type {
        self.types.get(id).unwrap()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Runtime,
    // TODO: generic types, union types, sum types, dependent types, fun fun yum yum!
    Void,
    // Never,
}
