//! Provides helpers to turn invocatinos of functions into unique IDs.

use std::sync::{Arc, Mutex};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{id::*, poor_hashmap::PoorMap};

use super::types::TypeBag;

#[derive(Default)]
pub struct UniqueFnId {
    id_gen: Counter<FunctionId<SymbolicCtx>>,
    fns: FxHashMap<FunctionId<LiftedCtx>, PoorMap<TypeBag, FunctionId<SymbolicCtx>>>,
    symb_to_lifted: FxHashMap<FunctionId<SymbolicCtx>, FunctionId<LiftedCtx>>,
    entry_fns: FxHashSet<FunctionId<SymbolicCtx>>,
}

#[derive(Clone)]
pub struct UniqueFnIdShared(pub Arc<Mutex<UniqueFnId>>);

impl UniqueFnIdShared {
    pub fn id_of(
        &self,
        fn_id: FunctionId<LiftedCtx>,
        types: TypeBag,
        is_entry_fn: bool,
    ) -> FunctionId<SymbolicCtx> {
        let mut me = self.0.try_lock().expect("should be contentionless");
        me.id_of(fn_id, types, is_entry_fn)
    }

    pub fn is_entry_fn(&self, id: FunctionId<SymbolicCtx>) -> bool {
        let mut me = self.0.try_lock().expect("should be contentionless");
        me.is_entry_fn(id)
    }

    pub fn types_of(&self, id: FunctionId<SymbolicCtx>) -> (FunctionId<LiftedCtx>, TypeBag) {
        let me = self.0.try_lock().expect("should be contentionless");
        let lifted_id = *me.symb_to_lifted.get(&id).unwrap();
        let poor_map = me.fns.get(&lifted_id).unwrap();

        for (bag, other_id) in poor_map.iter() {
            if *other_id == id {
                return (lifted_id, bag.clone());
            }
        }

        panic!("couldn't find types")
    }
}

impl UniqueFnId {
    pub fn id_of(
        &mut self,
        fn_id: FunctionId<LiftedCtx>,
        types: TypeBag,
        is_entry_fn: bool,
    ) -> FunctionId<SymbolicCtx> {
        let poor_map = self.fns.entry(fn_id).or_insert_with(Default::default);

        let id = match poor_map.get(&types) {
            Some(id) => *id,
            None => {
                let id = self.id_gen.next();
                poor_map.insert(types.clone(), id);
                self.symb_to_lifted.insert(id, fn_id);
                id
            }
        };

        if is_entry_fn {
            self.entry_fns.insert(id);
        }

        id
    }

    pub fn is_entry_fn(&self, id: FunctionId<SymbolicCtx>) -> bool {
        self.entry_fns.contains(&id)
    }
}
