use std::sync::Arc;
use std::sync::Mutex;

use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;

use crate::id::*;
use crate::lifted::LiftedProgram;

use self::graph_system::{Bogusable, GraphSystem, System, Worker, WorkerFactory};
use self::types::TypeBag;
use self::unique_id::{UniqueFnId, UniqueFnIdShared};
use self::worker::SymbWorker;

pub mod graph_system;
pub mod types;
pub mod unique_id;
pub mod worker;

pub fn execute(program: &LiftedProgram) {
    let mut fn_ids = UniqueFnId::default();
    let entry_fn_id = fn_ids.id_of(program.entrypoint, &TypeBag::default());

    let factory = SymbFactory {
        program,
        fn_ids: Arc::new(Mutex::new(fn_ids)),
    };

    let system = GraphSystem::new(factory);
    {
        system.spawn(entry_fn_id);
    }

    let results = system.try_into_results().expect("system should be dead");

    todo!()
}

struct SymbFactory<'program> {
    program: &'program LiftedProgram,
    fn_ids: Arc<Mutex<UniqueFnId>>,
}

impl<'p> WorkerFactory for SymbFactory<'p> {
    type Worker = SymbWorker<'p>;

    fn make(&mut self, id: <Self::Worker as Worker>::Id) -> Self::Worker {
        Self::Worker {
            program: self.program,
            id,
            fn_ids: UniqueFnIdShared(self.fn_ids.clone()),
        }
    }
}

impl Bogusable for SymbWorker<'_> {
    fn bogus() -> Self {
        panic!()
    }
}
