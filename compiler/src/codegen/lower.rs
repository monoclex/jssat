use std::hash::Hash;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::codegen::Block;
use crate::id::{AssemblerCtx, BlockId, FunctionId, LowerCtx};
use crate::isa::{self, BlockJump};
use crate::retag::{
    BlkMapRetagger, BlkRetagger, CnstPassRetagger, ExtFnPassRetagger, FnMapRetagger,
    FnPassRetagger, FnRetagger, RegMapRetagger, RegRetagger,
};
use crate::symbolic_execution::types::InstIdx;

use super::{Function, Instruction, Program, TypedProgram};

pub fn lower(program: TypedProgram) -> Program {
    let mut lowerer = Lowerer::new(&program);
    lowerer.explore_queue.enqueue(program.entrypoint);

    let functions = lowerer.lower();
    let entrypoint = lowerer.id_of_explored(program.entrypoint);

    Program {
        entrypoint,
        external_functions: program.external_functions,
        constants: program.constants,
        functions,
    }
}

struct Lowerer<'a> {
    typed_program: &'a TypedProgram,
    fn_id_mapper: FnMapRetagger<AssemblerCtx, LowerCtx>,
    explore_queue: ExploreQueue<FunctionId<AssemblerCtx>>,
}

impl<'a> Lowerer<'a> {
    fn new(typed_program: &'a TypedProgram) -> Self {
        Lowerer {
            typed_program,
            fn_id_mapper: Default::default(),
            explore_queue: Default::default(),
        }
    }

    fn id_of_explored(&self, id: FunctionId<AssemblerCtx>) -> FunctionId<LowerCtx> {
        self.fn_id_mapper.retag_old(id)
    }

    fn lower(&mut self) -> FxHashMap<FunctionId<LowerCtx>, Function<LowerCtx>> {
        // first, we need to explore all the functions
        // this is so we can use `fn_id_mapper`'s `retag_old` in our code
        // otherwise when retagging `CallStatic` instructions for functions we haven't visited,
        // we'd be unable to retag those instructions

        let pre_explore = self.explore_queue.clone();
        while let Some(typed_program_fn_id) = self.explore_queue.next() {
            self.fn_id_mapper.retag_new(typed_program_fn_id); // important
            self.explore_fn(typed_program_fn_id);
        }

        // now map the functions since all functions have been added to `fn_id_mapper`

        self.explore_queue = pre_explore;
        let mut functions = FxHashMap::default();

        while let Some(typed_program_fn_id) = self.explore_queue.next() {
            let function_id = self.fn_id_mapper.retag_old(typed_program_fn_id);
            let function = self.map_fn(typed_program_fn_id);

            functions.insert(function_id, function);
        }

        functions
    }

    /// Explores a function's blocks and adds functions to visit to the `explore_queue`.
    fn explore_fn(&mut self, typed_program_fn_id: FunctionId<AssemblerCtx>) {
        let mut block_list = ExploreQueue::default();
        block_list.enqueue(typed_program_fn_id);

        while let Some(block_id) = block_list.next() {
            let block = self.typed_program.functions.get(&block_id).unwrap();

            for inst in block.instructions.iter() {
                if let Instruction::CallStatic(inst) = inst {
                    self.explore_queue.enqueue(inst.calling);
                }
            }

            // TODO: deduplicate this?
            match &block.end {
                super::EndInstruction::Unreachable(_) | super::EndInstruction::Return(_) => {}
                super::EndInstruction::Jump(inst) => {
                    block_list.enqueue(inst.0 .0);
                }
                super::EndInstruction::JumpIf(inst) => {
                    block_list.enqueue(inst.if_so.0);
                    block_list.enqueue(inst.other.0);
                }
            };
        }
    }

    /// Takes a lifted function, and produces a lowered function.
    fn map_fn(&mut self, typed_program_fn_id: FunctionId<AssemblerCtx>) -> Function<LowerCtx> {
        // every lifted function has their own unique id
        // we want to map these into lowered function block ids, which are local to a lowered function
        // so since every lifted function id is unique, we can map them all into a local block id
        let mut block_id_mapper = BlkMapRetagger::<AssemblerCtx, LowerCtx>::default();

        // we want to fill `block_id_mapper` with the mapped IDs
        // this is because when mapping an `EndInstruction`, we need to `retag_new` the ID at the end
        // but also need to `retag_new` at the beginning for `typed_program_fn_id`
        let mut block_list = ExploreQueue::default();
        block_list.enqueue(typed_program_fn_id);

        while let Some(block_id) = block_list.next() {
            block_id_mapper.retag_new(block_id.convert());

            let block = self.typed_program.functions.get(&block_id).unwrap();

            use super::EndInstruction::*;

            // TODO: deduplicate this?
            match &block.end {
                Unreachable(_) | Return(_) => {}
                Jump(inst) => {
                    block_list.enqueue(inst.0 .0);
                }
                JumpIf(inst) => {
                    block_list.enqueue(inst.if_so.0);
                    block_list.enqueue(inst.other.0);
                }
            };
        }

        // a lowered function is composed of a bunch of blocks
        // these blocks are the individual lifted functions we explore
        let mut blocks = FxHashMap::default();

        // a lifted function starts at a single block, and jump/jumpifs to other blocks
        // we want to explore all of them
        let mut block_list = ExploreQueue::default();
        block_list.enqueue(typed_program_fn_id);

        // a lifted function has its own set of registers that start from 0. since a lowered function
        // is composed of multiple lifted functions, we use a regmapretagger for each lifted function, and
        // then create a new regmapretagger with the same counter as the previous one, so that new registers
        // don't overlap with old ones
        let mut prev_reg_retagger = RegMapRetagger::default();

        // we don't change or modify constants or external functions, so we'll just set up stubs here
        let mut const_retagger = CnstPassRetagger::default();
        const_retagger.ignore_checks();

        let mut ext_retagger = ExtFnPassRetagger::default();
        ext_retagger.ignore_checks();

        let mut fnptr_retagger = FnPassRetagger::default();
        fnptr_retagger.ignore_checks();

        while let Some(block_id) = block_list.next() {
            let block = self.typed_program.functions.get(&block_id).unwrap();

            let mut reg_retagger = RegMapRetagger::new_with_count(prev_reg_retagger);

            let mut parameters = vec![];

            for parameter in block.parameters.iter() {
                parameters.push(reg_retagger.retag_new(*parameter));
            }

            let mut instructions = vec![];

            // enqueue up possible functions to examine
            for inst in block.instructions.iter() {
                use Instruction::*;

                let new_inst = match inst.clone() {
                    Noop(i) => Noop(i),
                    Comment(i) => Comment(i),
                    NewRecord(i) => NewRecord(i.retag(&mut reg_retagger)),
                    RecordGet(i) => RecordGet(i.retag(&mut reg_retagger)),
                    RecordSet(i) => RecordSet(i.retag(&mut reg_retagger)),
                    RecordHasKey(i) => RecordHasKey(i.retag(&mut reg_retagger)),
                    CallStatic(i) => CallStatic({
                        // explore this function later
                        self.explore_queue.enqueue(i.calling);

                        i.retag(&mut reg_retagger, &self.fn_id_mapper)
                    }),
                    CallExtern(i) => CallExtern(i.retag(&mut reg_retagger, &ext_retagger)),
                    CallVirt(_) => panic!("virtual instructions not supported"),
                    // CallVirt(i.retag(&mut reg_retagger)),
                    GetFnPtr(i) => GetFnPtr(i.retag(&mut reg_retagger, &fnptr_retagger)),
                    MakeTrivial(i) => MakeTrivial(i.retag(&mut reg_retagger)),
                    MakeBytes(i) => MakeBytes(i.retag(&mut reg_retagger, &const_retagger)),
                    MakeInteger(i) => MakeInteger(i.retag(&mut reg_retagger)),
                    MakeBoolean(i) => MakeBoolean(i.retag(&mut reg_retagger)),
                    BinOp(i) => BinOp(i.retag(&mut reg_retagger)),
                    Negate(i) => Negate(i.retag(&mut reg_retagger)),
                };
                instructions.push(new_inst);
            }

            // enqueue up blocks to travel to
            // TODO: deduplicate this
            use super::EndInstruction::*;

            let end = match block.end.clone() {
                Unreachable(i) => Unreachable(i),
                Return(i) => Return(i.retag(&reg_retagger)),
                Jump(inst) => {
                    let BlockJump(target, args) = inst.0;
                    block_list.enqueue(target);
                    Jump(isa::Jump(BlockJump(
                        block_id_mapper.retag_old(target.convert()),
                        args.into_iter()
                            .map(|r| reg_retagger.retag_old(r))
                            .collect(),
                    )))
                }
                JumpIf(inst) => {
                    let BlockJump(target, args) = inst.if_so;
                    let if_so = BlockJump(
                        block_id_mapper.retag_old(target.convert()),
                        args.into_iter()
                            .map(|r| reg_retagger.retag_old(r))
                            .collect(),
                    );

                    let BlockJump(target, args) = inst.other;
                    let other = BlockJump(
                        block_id_mapper.retag_old(target.convert()),
                        args.into_iter()
                            .map(|r| reg_retagger.retag_old(r))
                            .collect(),
                    );

                    block_list.enqueue(inst.if_so.0);
                    block_list.enqueue(inst.other.0);
                    JumpIf(isa::JumpIf {
                        condition: reg_retagger.retag_old(inst.condition),
                        if_so,
                        other,
                    })
                }
            };

            blocks.insert(
                block_id_mapper.retag_old(block_id.convert()),
                Block {
                    parameters,
                    instructions,
                    end,
                    type_info: block.type_info.clone(),
                },
            );

            prev_reg_retagger = reg_retagger;
        }

        Function {
            blocks,
            entry: block_id_mapper.retag_old(typed_program_fn_id.convert()),
        }
    }
}

#[derive(Default, Clone)]
struct ExploreQueue<T> {
    todo: Vec<T>,
    todone: FxHashSet<T>,
}

impl<T: Copy + Hash + Eq> ExploreQueue<T> {
    fn enqueue(&mut self, next: T) {
        if !self.todone.contains(&next) && !self.todo.contains(&next) {
            self.todo.push(next);
        }
    }

    fn next(&mut self) -> Option<T> {
        let element = self.todo.pop()?;
        let could_insert = self.todone.insert(element);
        assert!(could_insert);
        Some(element)
    }
}
