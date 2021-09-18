use std::hash::Hash;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::id::{AssemblerCtx, BlockId, FunctionId, LowerCtx};
use crate::retag::{BlkMapRetagger, BlkRetagger, FnMapRetagger, FnRetagger};

use super::{Function, Instruction, Program, TypedProgram};

pub fn lower(program: TypedProgram) -> Program {
    let mut lowerer = Lowerer::new(&program);
    lowerer.explore_queue.enqueue(program.entrypoint);

    let functions = lowerer.explore();
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

    fn explore(&mut self) -> FxHashMap<FunctionId<LowerCtx>, Function<LowerCtx>> {
        let mut functions = FxHashMap::default();

        while let Some(typed_program_fn_id) = self.explore_queue.next() {
            let function_id = self.fn_id_mapper.retag_new(typed_program_fn_id);
            let function = self.explore_fn(typed_program_fn_id);

            functions.insert(function_id, function);
        }

        functions
    }

    /// Explores a lifted function, and produces a lowered function.
    fn explore_fn(&mut self, typed_program_fn_id: FunctionId<AssemblerCtx>) -> Function<LowerCtx> {
        // a lowered function is composed of a bunch of blocks
        // these blocks are the individual lifted functions we explore
        let mut blocks = FxHashMap::default();

        // every lifted function has their own unique id
        // we want to map these into lowered function block ids, which are local to a lowered function
        // so since every lifted function id is unique, we can map them all into a local block id
        let mut block_id_mapper = BlkMapRetagger::<AssemblerCtx, LowerCtx>::default();

        // a lifted function starts at a single block, and jump/jumpifs to other blocks
        // we want to explore all of them
        let mut block_list = ExploreQueue::default();
        block_list.enqueue(typed_program_fn_id);

        while let Some(block_id) = block_list.next() {
            let typed_program_block = (self.typed_program.functions)
                .get(&block_id)
                .expect("function id should exist");

            let block_id_datatype_to_function_id_datatype =
                block_id.convert::<BlockId<AssemblerCtx>>();
            let block_id = block_id_mapper.retag_new(block_id_datatype_to_function_id_datatype);

            // enqueue up possible functions to examine
            for inst in typed_program_block.instructions.iter() {
                if let Instruction::CallStatic(inst) = inst {
                    self.explore_queue.enqueue(inst.calling);
                    continue;
                }

                if let Instruction::CallVirt(_) = inst {
                    panic!("virtual instructions not supported");
                }
            }

            // enqueue up blocks to travel to
            match &typed_program_block.end {
                super::EndInstruction::Unreachable(_) | super::EndInstruction::Return(_) => {}
                super::EndInstruction::Jump(inst) => {
                    block_list.enqueue(inst.0 .0);
                }
                super::EndInstruction::JumpIf(inst) => {
                    block_list.enqueue(inst.if_so.0);
                    block_list.enqueue(inst.other.0);
                }
            };

            blocks.insert(block_id, todo!());
        }

        Function {
            blocks,
            entry: todo!(),
        }
    }
}

#[derive(Default)]
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
