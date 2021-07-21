//! Provides helpers to turn invocatinos of functions into unique IDs.

use std::sync::{Arc, Mutex};

use rustc_hash::FxHashMap;

use crate::{id::*, poor_hashmap::PoorMap};

use super::types::TypeBag;

#[derive(Default)]
pub struct UniqueFnId {
    id_gen: Counter<FunctionId<SymbolicCtx>>,
    fns: FxHashMap<FunctionId<LiftedCtx>, PoorMap<TypeBag, FunctionId<SymbolicCtx>>>,
}

pub struct UniqueFnIdShared(pub Arc<Mutex<UniqueFnId>>);

impl UniqueFnIdShared {
    pub fn id_of(&self, fn_id: FunctionId<LiftedCtx>, types: &TypeBag) -> FunctionId<SymbolicCtx> {
        let mut fns = self.0.lock().unwrap();
        fns.id_of(fn_id, types)
    }
}

impl UniqueFnId {
    pub fn id_of(
        &mut self,
        fn_id: FunctionId<LiftedCtx>,
        types: &TypeBag,
    ) -> FunctionId<SymbolicCtx> {
        let poor_map = self.fns.entry(fn_id).or_insert_with(Default::default);

        match poor_map.get(types) {
            Some(id) => *id,
            None => {
                let id = self.id_gen.next();
                poor_map.insert(types.clone(), id);
                id
            }
        }
    }
}
