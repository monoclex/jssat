use std::any::Any;
use std::cell::UnsafeCell;
use std::panic::PanicInfo;
use std::sync::Arc;
use std::sync::Mutex;

use rustc_hash::FxHashMap;

use crate::id::*;
use crate::isa;
use crate::lifted::LiftedProgram;
use crate::retag::ExtFnPassRetagger;
use crate::retag::ExtFnRetagger;
use crate::symbolic_execution::graph_system::SuperUnsafeCell;
use crate::symbolic_execution::types::InstIdx;

use self::graph_system::{ComputeGraphSys, System, Worker, WorkerFactory};
use self::types::RegisterType;
use self::types::ReturnType;
use self::types::TypeBag;
use self::unique_id::{UniqueFnId, UniqueFnIdShared};
use self::worker::CurrentInstruction;
use self::worker::SymbWorker;
use self::worker::WorkerResults;

pub mod graph_system;
pub mod type_computations;
pub mod types;
pub mod unique_id;
pub mod worker;

pub struct Engine<'a> {
    program: &'a LiftedProgram,
    fn_ids: UniqueFnIdShared,
    system: ComputeGraphSys<SymbWorker<'a>, SymbFactory<'a>>,
}

pub fn execute(program: &LiftedProgram) -> SystemRun {
    let engine = make_system(program);
    system_run(engine, program.entrypoint, |_| Vec::new())
}

pub fn make_system(program: &LiftedProgram) -> Engine {
    let mut asm_ext_map = ExtFnPassRetagger::default();
    for (id, _) in program.external_functions.iter() {
        asm_ext_map.retag_new(*id);
    }

    let fn_ids = UniqueFnId::default();
    let fn_ids_shared = UniqueFnIdShared(Arc::new(Mutex::new(fn_ids)));

    let factory = SymbFactory {
        program,
        fn_ids: fn_ids_shared.clone(),
        asm_ext_map: Arc::new(asm_ext_map),
    };

    let system = ComputeGraphSys::new(factory);

    Engine {
        program,
        fn_ids: fn_ids_shared,
        system,
    }
}

#[derive(Clone)]
pub struct SystemRun<'a> {
    pub program: &'a LiftedProgram,
    pub entry_fn: FunctionId<SymbolicCtx>,
    pub results: FxHashMap<FunctionId<SymbolicCtx>, WorkerResults>,
}

pub fn system_run(
    engine: Engine,
    fn_id: FunctionId<LiftedCtx>,
    args: impl FnOnce(&mut TypeBag) -> Vec<RegisterType>,
) -> SystemRun {
    let mut types = TypeBag::default();
    let args = args(&mut types);

    let program_fn = engine.program.functions.get(&fn_id).unwrap();

    debug_assert_eq!(args.len(), program_fn.parameters.len());
    for (fn_reg, arg_typ) in program_fn.parameters.iter().zip(args) {
        types.assign_type(*fn_reg, arg_typ);
    }

    let engine_fn_id = engine.fn_ids.id_of(fn_id, types, false);

    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!();
        println!("=== ERROR DURING SYMBOLIC EXECUTION ===");
        default_hook(info);
    }));

    let result = std::panic::catch_unwind(|| {
        engine.system.spawn(engine_fn_id);
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
        Err(e) => {
            handle_panic(engine.system, e);
            drop(std::panic::take_hook());
            panic!("panic handler ran");
        }
    };

    drop(std::panic::take_hook());

    let results = engine
        .system
        .try_into_results()
        .expect("system should be dead");

    SystemRun {
        program: engine.program,
        entry_fn: engine_fn_id,
        results,
    }
}

fn handle_panic<'p>(
    system: ComputeGraphSys<SymbWorker<'p>, SymbFactory<'p>>,
    panic_payload: Box<dyn Any + Send>,
) {
    println!();
    println!("! note: at this time, type information is inaccurate");
    println!("! as it only reflects the most current state of abstract interpretation");
    println!();

    println!("callstack:");
    let callstack = system.load_callstack_within_panic();

    // print `nw` workers before quitting
    let nw = std::env::var("JSSAT_VIEW_WORKERS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(usize::MAX);

    for (_, worker) in callstack.into_iter().take(nw) {
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
        let n = std::env::var("JSSAT_VIEW_LINES")
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

            let inst_idx = InstIdx::Inst(i);

            // TODO: setup a `DisplayContext` for self referential structs
            if let Some(reg) = inst.assigned_to() {
                println!("# %{} : {}", reg, w.types.display(reg, inst_idx))
            }

            for reg in inst.used_registers() {
                println!("# %{} : {}", reg, w.types.display(reg, inst_idx))
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
