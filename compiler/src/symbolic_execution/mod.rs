use std::sync::Arc;
use std::sync::Mutex;

use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;

use crate::id::*;
use crate::lifted::LiftedProgram;
use crate::symbolic_execution::graph_system::GraphSystem;
use crate::symbolic_execution::graph_system::System;

use self::graph_system::Bogusable;
use self::graph_system::Worker;
use self::graph_system::WorkerFactory;
use self::types::TypeBag;

pub mod graph_system;
pub mod types;

#[test]
fn awesome() {
    let system = GraphSystem::new(SymbFactory);
    let k = system.spawn(0);
    println!("result: {}", *k);
    panic!();
}

pub fn execute(program: &LiftedProgram) {
    todo!()
}

struct SymbFactory;

impl WorkerFactory for SymbFactory {
    type Worker = SymbWorker;

    fn make(&mut self, id: usize) -> Self::Worker {
        println!("making worker {}", id);
        SymbWorker(id)
    }
}

struct SymbWorker(usize);

impl Worker for SymbWorker {
    type Id = usize;

    type Result = usize;

    fn work(&mut self, system: &impl graph_system::System<Self>) -> Self::Result {
        println!("worker {} is working", self.0);

        if self.0 == 9 {
            return 60;
        }

        *system.spawn(self.0 + 1) + 1
    }
}

impl Bogusable for usize {
    fn bogus() -> Self {
        panic!()
    }
}
