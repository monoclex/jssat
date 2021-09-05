use std::any::Any;
use std::cell::UnsafeCell;
use std::panic::PanicInfo;
use std::sync::Arc;
use std::sync::Mutex;

use crate::id::*;
use crate::isa;
use crate::lifted::LiftedProgram;
use crate::retag::ExtFnPassRetagger;
use crate::retag::ExtFnRetagger;
use crate::symbolic_execution::graph_system::SuperUnsafeCell;

use self::graph_system::{ComputeGraphSys, System, Worker, WorkerFactory};
use self::types::ReturnType;
use self::types::TypeBag;
use self::unique_id::{UniqueFnId, UniqueFnIdShared};
use self::worker::CurrentInstruction;
use self::worker::SymbWorker;

pub mod graph_system;
pub mod type_computations;
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

    let system = ComputeGraphSys::new(factory);

    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!();
        println!("=== ERROR DURING SYMBOLIC EXECUTION ===");
        default_hook(info);
    }));

    let result = std::panic::catch_unwind(|| {
        system.spawn(entry_fn_id);
    });

    drop(std::panic::take_hook());

    // replace hook to make it known that there was a panic during panic execution
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!();
        println!("--- ERROR DURING PRINTING SYMBOLIC EXECUTION TRACE ---");
        default_hook(info);
    }));

    match result {
        Ok(_) => {}
        Err(e) => handle_panic(system, e),
    };

    drop(std::panic::take_hook());

    let _results = system.try_into_results().expect("system should be dead");

    // assembler_glue::glue(entry_fn_id, &program, results)
    todo!()
}

fn handle_panic<'p>(
    system: ComputeGraphSys<SymbWorker<'p>, SymbFactory<'p>>,
    panic_payload: Box<dyn Any + Send>,
) -> ! {
    println!();
    println!("! note: at this time, type information is inaccurate");
    println!("! as it only reflects the most current state of abstract interpretation");
    println!();

    println!("callstack:");
    let callstack = system.load_callstack_within_panic();

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

        match w.types.looking_up() {
            types::LookingUpStatus::Nothing => {}
            types::LookingUpStatus::RecordKey(key) => {
                println!("- was looking up type of field key: {:?}", key)
            }
            types::LookingUpStatus::Register(r) => {
                println!("- was looking up type of register: %{}", r)
            }
            types::LookingUpStatus::Constant(c) => {
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
                println!("was not found working on any instruction (maybe panicked on params?)");
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
