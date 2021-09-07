use derive_more::{Deref, DerefMut};
use rustc_hash::FxHashMap;
use tinyvec::TinyVec;

use crate::{
    id::{AssemblerCtx, FunctionId, LiftedCtx, RegisterId, SymbolicCtx},
    lifted::LiftedProgram,
    retag::{
        CnstPassRetagger, CnstRetagger, ExtFnPassRetagger, ExtFnRetagger, FnPassRetagger,
        FnRetagger, RegPassRetagger, RegRetagger,
    },
    symbolic_execution::{
        types::{InstIdx, TypeBag},
        unique_id::UniqueFnIdShared,
        worker::WorkerResults,
        SystemRun,
    },
    UnwrapNone,
};

use super::{Block, EndInstruction, Instruction, TypedProgram};

/// Given a run of a program after symbolic execution, this will produce a
/// version of the program free of the original `LiftedProgram`, allowing for
/// optimizations to be made on a per-function basis as necessary.
pub fn type_program(system_run: SystemRun) -> TypedProgram {
    let SystemRun {
        program,
        entry_fn,
        fn_ids,
        results,
    } = system_run;

    let mut function_id_mapper = FnPassRetagger::default();
    function_id_mapper.ignore_checks();
    let entrypoint = function_id_mapper.retag_new(entry_fn);

    let mut external_function_id_mapper = ExtFnPassRetagger::default();
    let mut external_functions = FxHashMap::default();

    for (id, external_function) in program.external_functions.iter() {
        let id = external_function_id_mapper.retag_new(*id);
        external_functions
            .insert(id, external_function.clone())
            .expect_free();
    }

    let mut constant_id_mapper = CnstPassRetagger::default();
    let mut constants = FxHashMap::default();

    for (id, constant) in program.constants.iter() {
        let id = constant_id_mapper.retag_new(*id);
        constants.insert(id, constant.clone()).expect_free();
    }

    let mut functions = FxHashMap::default();

    let mut explore_queue = vec![entry_fn];

    let mut fn_typer = FnTyperFactory {
        program,
        fn_ids: &fn_ids,
        results: &results,
        fn_id_mapper: &mut function_id_mapper,
        explore_queue: &mut explore_queue,
    };

    while let Some(id) = fn_typer.explore_queue.pop() {
        let (id, function) = fn_typer.process(id);
        functions.insert(id, function).expect_free();
    }

    TypedProgram {
        entrypoint,
        external_functions,
        constants,
        functions,
    }
}

struct FnTyperFactory<'a> {
    program: &'a LiftedProgram,
    fn_ids: &'a UniqueFnIdShared,
    results: &'a FxHashMap<FunctionId<SymbolicCtx>, WorkerResults>,
    fn_id_mapper: &'a mut FnPassRetagger<SymbolicCtx, AssemblerCtx>,
    explore_queue: &'a mut Vec<FunctionId<SymbolicCtx>>,
}

impl<'a> FnTyperFactory<'a> {
    pub fn process(
        &mut self,
        id: FunctionId<SymbolicCtx>,
    ) -> (FunctionId<AssemblerCtx>, Block<TypeBag>) {
        let function_id = self.fn_id_mapper.retag_new(id);
        let results = self.results.get(&id).unwrap();
        let (lifted_fn_id, _) = self.fn_ids.types_of(id);
        let lifted_fn = self.program.functions.get(&lifted_fn_id).unwrap();
        let reg_retagger = RegPassRetagger::default();

        let mut fn_typer = FnTyper {
            factory: self,
            id,
            function_id,
            results,
            lifted_fn_id,
            lifted_fn,
            reg_retagger,
        };

        let block = fn_typer.process();

        (function_id, block)
    }
}

#[derive(Deref, DerefMut)]
struct FnTyper<'resource, 'factory> {
    #[deref]
    #[deref_mut]
    factory: &'factory mut FnTyperFactory<'resource>,
    id: FunctionId<SymbolicCtx>,
    function_id: FunctionId<AssemblerCtx>,
    results: &'resource WorkerResults,
    lifted_fn_id: FunctionId<LiftedCtx>,
    lifted_fn: &'resource crate::lifted::Function,
    reg_retagger: RegPassRetagger<LiftedCtx, AssemblerCtx>,
}

impl<'r> FnTyper<'r, '_> {
    pub fn process(&mut self) -> Block<TypeBag> {
        let parameters = (self.lifted_fn.parameters.iter())
            .map(|r| self.reg_retagger.retag_new(*r))
            .collect();

        let instructions = (self.lifted_fn.instructions.iter().enumerate())
            .map(|(idx, inst)| self.process_inst(inst, InstIdx::Inst(idx)))
            .collect();

        let end = self.process_end(&self.lifted_fn.end);

        let type_info = self.results.types.clone();

        Block {
            parameters,
            instructions,
            end,
            type_info,
        }
    }

    fn process_inst(
        &mut self,
        instruction: &crate::frontend::ir::Instruction<LiftedCtx, LiftedCtx>,
        inst_idx: InstIdx,
    ) -> Instruction {
        use crate::frontend::ir;

        match instruction {
            ir::Instruction::Comment(i) => Instruction::Comment(*i),
            ir::Instruction::NewRecord(i) => {
                Instruction::NewRecord(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::RecordGet(_) => todo!(),
            ir::Instruction::RecordSet(_) => todo!(),
            ir::Instruction::RecordHasKey(_) => todo!(),
            ir::Instruction::GetFnPtr(_) => todo!(),
            ir::Instruction::CallStatic(_) => todo!(),
            ir::Instruction::CallExtern(_) => todo!(),
            ir::Instruction::CallVirt(_) => todo!(),
            ir::Instruction::MakeTrivial(_) => todo!(),
            ir::Instruction::MakeBytes(_) => todo!(),
            ir::Instruction::MakeInteger(_) => todo!(),
            ir::Instruction::MakeBoolean(_) => todo!(),
            ir::Instruction::BinOp(_) => todo!(),
            ir::Instruction::Negate(_) => todo!(),
            ir::Instruction::Generalize(_) => todo!(),
        }
    }

    fn process_end(&mut self, end_inst: &crate::lifted::EndInstruction) -> EndInstruction {
        todo!()
    }
}
