use std::sync::Arc;
use std::sync::Mutex;
use std::sync::TryLockError;

use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;

use crate::id::*;
use crate::lifted::LiftedProgram;
use crate::symbolic_execution::graph_system::SuperUnsafeCell;

use self::graph_system::{Bogusable, GraphSystem, System, Worker, WorkerFactory};
use self::types::ReturnType;
use self::types::TypeBag;
use self::unique_id::{UniqueFnId, UniqueFnIdShared};
use self::worker::CurrentInstruction;
use self::worker::SymbWorker;

pub mod graph_system;
pub mod types;
pub mod unique_id;
pub mod worker;

pub fn execute(program: &'static LiftedProgram) {
    let mut fn_ids = UniqueFnId::default();
    let entry_fn_id = fn_ids.id_of(program.entrypoint, TypeBag::default());

    let fn_ids_shared = UniqueFnIdShared(Arc::new(Mutex::new(fn_ids)));

    let factory = SymbFactory {
        program,
        fn_ids: fn_ids_shared.clone(),
    };

    let system = GraphSystem::new(factory);

    let orig_hook = std::panic::take_hook();
    system.set_panic_hook(Box::new(move |panic_info, callstack| {
        // #[forbid(panic)]
        // WE CANNOT PANIC IN THIS CODE!

        println!();
        println!("=== ERROR DURING SYMBOLIC EXECUTION ===");
        orig_hook(panic_info);
        println!();

        println!("callstack:");
        for (func, worker) in callstack {
            // TODO: we don't actually need mutable access, just immutable access
            let w: &mut SymbWorker = worker.raw_get();

            println!("  Worker {}:", w.id);

            let looking_up = match w.types.looking_up.try_lock() {
                Ok(g) => g,
                Err(TryLockError::Poisoned(g)) => g.into_inner(),
                Err(_) => {
                    println!(
                        "couldn't unlock `looking_up` (this is an err) for {:?}",
                        w.id
                    );
                    continue;
                }
            };

            // print `n` instructions before and after the instruction we're on
            let n = 5;

            match w.inst_on {
                CurrentInstruction::None => {
                    println!("| was not found working on any instruction")
                }
                CurrentInstruction::Sequential(inst) => {
                    let find_inst_idx = w
                        .func
                        .instructions
                        .iter()
                        .enumerate()
                        .find(|(_, i)| std::ptr::eq(*i, inst));

                    let idx = match find_inst_idx {
                        Some((idx, _)) => idx,
                        None => {
                            println!("cannot print code of worker");
                            continue;
                        }
                    };

                    let inst_range = (idx.saturating_sub(n))..(idx.saturating_add(n));

                    for (i, inst) in w
                        .func
                        .instructions
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| inst_range.contains(i))
                    {
                        match i == idx {
                            true => println!("> {}", inst.as_display()),
                            false => println!("| {}", inst.as_display()),
                        };
                    }
                }
                CurrentInstruction::ControlFlow(inst) => {
                    println!("> {}", inst.as_display());
                }
            };
        }
    }));

    {
        system.spawn(entry_fn_id);
    }

    let results = system.try_into_results().expect("system should be dead");

    drop(std::panic::take_hook());

    todo!()
}

struct SymbFactory<'program> {
    program: &'program LiftedProgram,
    fn_ids: UniqueFnIdShared,
}

impl<'p> WorkerFactory for SymbFactory<'p> {
    type Worker = SymbWorker<'p>;

    fn make(&mut self, id: <Self::Worker as Worker>::Id) -> Self::Worker {
        let (lifted_id, types) = self.fn_ids.types_of(id);

        Self::Worker {
            program: self.program,
            func: self.program.functions.get(&lifted_id).unwrap(),
            id,
            fn_ids: self.fn_ids.clone(),
            types,
            inst_on: CurrentInstruction::None,
            // placeholder value
            return_type: ReturnType::Never,
        }
    }
}

impl Bogusable for SymbWorker<'_> {
    fn bogus() -> Self {
        panic!()
    }
}
