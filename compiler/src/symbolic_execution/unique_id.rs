//! Provides helpers to turn invocatinos of functions into unique IDs.

use std::sync::{Arc, Mutex};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{collections::PoorMap, id::*, symbolic_execution::types::InstIdx};

use super::types::TypeBag;

#[derive(Default)]
pub struct UniqueFnId {
    id_gen: FunctionId<SymbolicCtx>,
    fns: FxHashMap<FunctionId<LiftedCtx>, PoorMap<TypeBag, FunctionId<SymbolicCtx>>>,
    symb_to_lifted: FxHashMap<FunctionId<SymbolicCtx>, FunctionId<LiftedCtx>>,
    entry_fns: FxHashSet<FunctionId<SymbolicCtx>>,
}

pub struct UniqueFnIdShared(pub Arc<Mutex<UniqueFnId>>);

impl Clone for UniqueFnIdShared {
    fn clone(&self) -> Self {
        let arc = self.0.clone();
        Self(arc)
    }
}

impl UniqueFnIdShared {
    pub fn id_of_immut(
        &self,
        fn_id: FunctionId<LiftedCtx>,
        types: TypeBag,
        is_entry_fn: bool,
    ) -> Option<FunctionId<SymbolicCtx>> {
        let me = self.0.try_lock().expect("should be contentionless");
        me.id_of_immut(fn_id, types, is_entry_fn)
    }

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
        let me = self.0.try_lock().expect("should be contentionless");
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
    pub fn id_of_immut(
        &self,
        fn_id: FunctionId<LiftedCtx>,
        types: TypeBag,
        is_entry_fn: bool,
    ) -> Option<FunctionId<SymbolicCtx>> {
        let poor_map = self.fns.get(&fn_id)?;
        poor_map.get(&types).cloned()
    }

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
                let id = self.id_gen.next_and_mut();
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
