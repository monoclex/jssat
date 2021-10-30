use std::collections::VecDeque;

use derive_more::{Deref, DerefMut};
use rustc_hash::{FxHashMap, FxHashSet};
use tinyvec::TinyVec;

use crate::{
    id::{AssemblerCtx, FunctionId, LiftedCtx, RegisterId, SymbolicCtx},
    isa::{BlockJump, Call, Comment, Jump, JumpIf, Make, Noop},
    lifted::LiftedProgram,
    retag::{
        CnstPassRetagger, CnstRetagger, ExtFnPassRetagger, ExtFnRetagger, FnPassRetagger,
        FnRetagger, RegPassRetagger, RegRetagger,
    },
    symbolic_execution::{
        types::{InstIdx, RegisterType, TypeBag},
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

    let mut explore_queue = VecDeque::from(vec![entry_fn]);

    let mut fn_typer = FnTyperFactory {
        program,
        fn_ids: &fn_ids,
        all_results: &results,
        fn_id_mapper: &mut function_id_mapper,
        ext_fn_id_mapper: &mut external_function_id_mapper,
        constant_id_mapper: &constant_id_mapper,
        explore_queue: &mut explore_queue,
    };

    let mut completed_functions = FxHashSet::default();
    while let Some(id) = fn_typer.explore_queue.pop_front() {
        if !completed_functions.insert(id) {
            // we've already handled this function
            continue;
        }

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
    all_results: &'a FxHashMap<FunctionId<SymbolicCtx>, WorkerResults>,
    fn_id_mapper: &'a mut FnPassRetagger<SymbolicCtx, AssemblerCtx>,
    ext_fn_id_mapper: &'a mut ExtFnPassRetagger<LiftedCtx, AssemblerCtx>,
    constant_id_mapper: &'a CnstPassRetagger<LiftedCtx, AssemblerCtx>,
    explore_queue: &'a mut VecDeque<FunctionId<SymbolicCtx>>,
}

impl<'a> FnTyperFactory<'a> {
    pub fn process(
        &mut self,
        id: FunctionId<SymbolicCtx>,
    ) -> (FunctionId<AssemblerCtx>, Block<AssemblerCtx>) {
        let function_id = self.fn_id_mapper.retag_new(id);
        let results = self.all_results.get(&id).unwrap();
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
    pub fn process(&mut self) -> Block<AssemblerCtx> {
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
    ) -> Instruction<AssemblerCtx> {
        use crate::frontend::ir;

        match instruction {
            ir::Instruction::Comment(i) => Instruction::Comment(*i),
            ir::Instruction::NewRecord(i) => {
                Instruction::NewRecord(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::RecordGet(i) => {
                Instruction::RecordGet(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::RecordSet(i) => {
                Instruction::RecordSet(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::RecordHasKey(i) => {
                Instruction::RecordHasKey(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::GetFnPtr(i) => Instruction::GetFnPtr(Make {
                result: self.reg_retagger.retag_new(i.result),
                item: i.item,
            }),
            ir::Instruction::CallStatic(i) => {
                // TODO: abstract out SymbWorker::exec_types and use their same logic
                // TODO: this logic is also present in mapping a BlockJump, need to consolidaate
                // the logic
                let prev = inst_idx.back().unwrap();
                let target_id =
                    self.lookup_symbolic_fn_id_from_invocation(i.calling, &i.args, prev);

                Instruction::CallStatic(Call {
                    result: i.result.map(|r| self.reg_retagger.retag_new(r)),
                    calling: self.fn_id_mapper.retag_old(target_id),
                    args: i
                        .args
                        .iter()
                        .map(|r| self.reg_retagger.retag_old(*r))
                        .collect(),
                })
            }
            ir::Instruction::CallExtern(i) => Instruction::CallExtern(Call {
                result: i.result.map(|r| self.reg_retagger.retag_new(r)),
                calling: self.ext_fn_id_mapper.retag_old(i.calling),
                args: i
                    .args
                    .iter()
                    .map(|r| self.reg_retagger.retag_old(*r))
                    .collect(),
            }),
            ir::Instruction::CallVirt(i) => {
                // TODO: no support for unions of fnptrs yet
                // SANITY: this is fine as SSA form prevents registers from being set and we
                // should only have fnpts stored in the register id
                let fn_id = self.results.types.get_fnptr(i.calling);
                let prev = inst_idx.back().unwrap();
                let target_id = self.lookup_symbolic_fn_id_from_invocation(fn_id, &i.args, prev);

                // TODO: once callvirt supports calling unions,
                // replace this instruction with a different one maybe? maybe some kind of
                // callvirt instruction? one that would handle the idea of "here
                // are the unions of functions we may call bla bla bla"
                Instruction::CallStatic(Call {
                    result: i.result.map(|r| self.reg_retagger.retag_new(r)),
                    calling: self.fn_id_mapper.retag_old(target_id),
                    args: i
                        .args
                        .iter()
                        .map(|r| self.reg_retagger.retag_old(*r))
                        .collect(),
                })
            }
            ir::Instruction::MakeTrivial(i) => {
                Instruction::MakeTrivial(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::MakeBytes(i) => {
                let const_retagger = self.constant_id_mapper;
                let reg_retagger = &mut self.reg_retagger;
                Instruction::MakeBytes(i.retag(reg_retagger, const_retagger))
            }
            ir::Instruction::MakeInteger(i) => {
                Instruction::MakeInteger(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::MakeBoolean(i) => {
                Instruction::MakeBoolean(i.retag(&mut self.reg_retagger))
            }
            ir::Instruction::BinOp(i) => Instruction::BinOp(i.retag(&mut self.reg_retagger)),
            ir::Instruction::Negate(i) => Instruction::Negate(i.retag(&mut self.reg_retagger)),
            ir::Instruction::Generalize(_) => todo!(),
            ir::Instruction::Assert(_) => todo!(),
            ir::Instruction::IsType(_) => todo!(),
        }
    }

    fn process_end(
        &mut self,
        end_inst: &crate::lifted::EndInstruction,
    ) -> EndInstruction<AssemblerCtx, FunctionId<AssemblerCtx>> {
        match end_inst {
            crate::lifted::EndInstruction::Jump(i) => {
                EndInstruction::Jump(Jump(self.translate_blockjump(&i.0)))
            }
            crate::lifted::EndInstruction::JumpIf(i) => {
                if let RegisterType::Bool(b) = self.results.types.get(i.condition) {
                    EndInstruction::Jump(Jump(self.translate_blockjump(i.take_path(b))))
                } else {
                    panic!("TODO: handle both paths")
                }
            }
            crate::lifted::EndInstruction::Return(i) => {
                EndInstruction::Return(i.retag(&self.reg_retagger))
            }
        }
    }

    fn translate_blockjump(
        &mut self,
        b: &BlockJump<FunctionId<LiftedCtx>, LiftedCtx>,
    ) -> BlockJump<FunctionId<AssemblerCtx>, AssemblerCtx> {
        let BlockJump(symb_id, args) = b;

        let prev = InstIdx::from_inst_len(self.lifted_fn.instructions.len());
        let target_id = self.lookup_symbolic_fn_id_from_invocation(*symb_id, args, prev);

        let target_fn_id = self.fn_id_mapper.retag_new(target_id);
        let target_args = args
            .iter()
            .map(|r| self.reg_retagger.retag_old(*r))
            .collect();

        BlockJump(target_fn_id, target_args)
    }

    fn lookup_symbolic_fn_id_from_invocation(
        &mut self,
        fn_id: FunctionId<LiftedCtx>,
        args: &[RegisterId<LiftedCtx>],
        up_until: InstIdx,
    ) -> FunctionId<SymbolicCtx> {
        let target_fn = self.program.functions.get(&fn_id).unwrap();

        let subset = self
            .results
            .types
            .subset_immut(args, &target_fn.parameters, up_until);

        let target_id = self
            .fn_ids
            .id_of_immut(fn_id, subset.child(), false)
            .unwrap();

        self.factory.explore_queue.push_back(target_id);

        target_id
    }
}
