use std::sync::Arc;
use std::sync::Mutex;
use std::sync::TryLockError;

use crate::frontend::assembler;
use crate::id::*;
use crate::lifted::LiftedProgram;
use crate::retag::ExtFnPassRetagger;
use crate::retag::ExtFnRetagger;

use self::graph_system::{Bogusable, GraphSystem, System, Worker, WorkerFactory};
use self::types::ReturnType;
use self::types::TypeBag;
use self::unique_id::{UniqueFnId, UniqueFnIdShared};
use self::worker::CurrentInstruction;
use self::worker::SymbWorker;

pub mod assembler_glue;
pub mod graph_system;
pub mod types;
pub mod unique_id;
pub mod worker;

pub fn execute(program: &'static LiftedProgram) -> assembler::Program {
    let mut asm_ext_map = ExtFnPassRetagger::default();
    for (id, _) in program.external_functions.iter() {
        asm_ext_map.retag_new(*id);
    }

    let mut fn_ids = UniqueFnId::default();
    let entry_fn_id = fn_ids.id_of(program.entrypoint, TypeBag::default(), true);

    let fn_ids_shared = UniqueFnIdShared(Arc::new(Mutex::new(fn_ids)));

    let factory = SymbFactory {
        program,
        fn_ids: fn_ids_shared,
        asm_ext_map: Arc::new(asm_ext_map),
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
        for (_, worker) in callstack {
            // TODO: we don't actually need mutable access, just immutable access
            let w: &mut SymbWorker = unsafe { worker.raw_get() };

            println!();
            println!("Worker {}:", w.id);

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

            match *looking_up {
                types::LookingUp::Nothing => {}
                types::LookingUp::ShapeKey(key) => {
                    println!("- was looking up type of field key: {:?}", key)
                }
                types::LookingUp::Register(r) => {
                    println!("- was looking up type of register: %{}", r)
                }
                types::LookingUp::Constant(c) => {
                    println!("- was looking up value of constant: {}", c)
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
                    // TODO: don't copy code
                    let idx = w.func.instructions.len();
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

    assembler_glue::glue(entry_fn_id, &program, results)
}

struct SymbFactory<'program> {
    program: &'program LiftedProgram,
    fn_ids: UniqueFnIdShared,
    asm_ext_map: Arc<ExtFnPassRetagger<LiftedCtx, AssemblerCtx>>,
}

impl<'p> WorkerFactory for SymbFactory<'p> {
    type Worker = SymbWorker<'p>;

    fn make(&mut self, id: <Self::Worker as Worker>::Id) -> Self::Worker {
        let (lifted_id, types) = self.fn_ids.types_of(id);

        SymbWorker {
            program: self.program,
            func: self.program.functions.get(&lifted_id).unwrap(),
            id,
            fn_ids: self.fn_ids.clone(),
            types,
            inst_on: CurrentInstruction::None,
            // placeholder value
            return_type: ReturnType::Never,
            is_entry_fn: self.fn_ids.is_entry_fn(id),
            asm_ext_map: self.asm_ext_map.clone(),
        }
    }
}

impl Bogusable for SymbWorker<'_> {
    fn bogus() -> Self {
        panic!()
    }
}
