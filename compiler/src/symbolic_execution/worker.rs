//! The file where the actual symbolic execution work gets done

use crate::id::*;
use crate::lifted::LiftedProgram;

use super::{
    graph_system::{System, Worker},
    unique_id::UniqueFnIdShared,
};

pub struct SymbWorker<'program> {
    pub program: &'program LiftedProgram,
    pub id: <Self as Worker>::Id,
    pub fn_ids: UniqueFnIdShared,
}

impl<'p> Worker for SymbWorker<'p> {
    type Id = FunctionId<SymbolicCtx>;

    // TODO: the result of a worker should be a return type like Never
    // or something idk. maybe it's fine to leave it as self and hope that
    // callers only use the return type and not any other values
    type Result = Self;

    fn work(mut self, system: &impl System<Self>) -> Self::Result {
        self
    }
}
