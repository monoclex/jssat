use std::sync::Arc;
use std::sync::Mutex;
use std::sync::TryLockError;

use crate::id::*;
use crate::isa;
use crate::lifted::LiftedProgram;
use crate::retag::ExtFnPassRetagger;
use crate::retag::ExtFnRetagger;

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
        println!("! note: at this time, type information is inaccurate");
        println!("! as it only reflects the most current state of abstract interpretation");
        println!();

        println!("callstack:");
        for (_, worker) in callstack {
            // TODO: we don't actually need mutable access, just immutable access
            let w: &mut SymbWorker = unsafe { worker.raw_get() };

            println!();
            println!(
                "Worker {} (IR: @{}.${}):",
                w.id, w.func.ir_fn_id, w.func.ir_blk_id
            );

            if w.never_infected {
                println!("- was `never` infected");
            }

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
            let n = std::env::var("JSSAT_VIEW")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(7);

            let idx = match w.inst_on {
                CurrentInstruction::None => {
                    println!(
                        "was not found working on any instruction (maybe panicked on params?)"
                    );
                    None
                }
                CurrentInstruction::Sequential(inst) => {
                    let find_inst_idx = w
                        .func
                        .instructions
                        .iter()
                        .enumerate()
                        .find(|(_, i)| std::ptr::eq(*i, inst));

                    find_inst_idx.map(|(idx, _)| idx)
                }
                // one past last instruction idx == control flow
                CurrentInstruction::ControlFlow(_) => Some(w.func.instructions.len()),
                CurrentInstruction::Completed => {
                    println!("was found completed");
                    None
                }
            };

            let inst_range = match idx {
                Some(idx) => idx.saturating_sub(n)..idx.saturating_add(n),
                None => 0..n,
            };

            println!("fn @{}({})", w.id, isa::Registers(&w.func.parameters));

            if !inst_range.contains(&0) {
                println!("| ...");
            }

            for (i, inst) in w
                .func
                .instructions
                .iter()
                .enumerate()
                .filter(|(i, _)| inst_range.contains(i))
            {
                match Some(i) == idx {
                    true => println!("> {}", inst.as_display()),
                    false => println!("| {}", inst.as_display()),
                };

                // TODO: setup a `DisplayContext` for self referential structs
                if let Some(reg) = inst.assigned_to() {
                    println!("# %{} : {}", reg, w.types.display(reg))
                }

                for reg in inst.used_registers() {
                    println!("# %{} : {}", reg, w.types.display(reg))
                }
            }

            let last_idx = w.func.instructions.len();
            if inst_range.contains(&last_idx) {
                let inst = &w.func.end;
                match Some(last_idx) == idx {
                    true => println!("> {}", inst.as_display()),
                    false => println!("| {}", inst.as_display()),
                };
            } else {
                println!("| ...");
            }
        }
    }));

    {
        system.spawn(entry_fn_id);
    }

    let results = system.try_into_results().expect("system should be dead");

    drop(std::panic::take_hook());

    // assembler_glue::glue(entry_fn_id, &program, results)
    todo!()
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
            never_infected: false,
        }
    }
}

impl Bogusable for SymbWorker<'_> {
    fn bogus() -> Self {
        panic!()
    }
}
