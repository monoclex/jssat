use std::sync::Arc;
use std::{marker::PhantomData, sync::atomic::AtomicUsize};

use rustc_hash::FxHashMap;

use crate::frontend::ir::*;
use crate::{id::*, name::DebugName};

pub struct ProgramBuilder {
    entrypoint: Option<FunctionId>,
    constants: FxHashMap<ConstantId, Constant>,
    external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    functions: FxHashMap<FunctionId, Function>,
    gen_constant_id: Counter<ConstantId>,
    gen_external_function_id: Counter<ExternalFunctionId>,
    gen_function_id: Counter<FunctionId>,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        ProgramBuilder {
            entrypoint: None,
            constants: FxHashMap::default(),
            external_functions: FxHashMap::default(),
            functions: FxHashMap::default(),
            gen_constant_id: Counter::new(),
            gen_external_function_id: Counter::new(),
            gen_function_id: Counter::new(),
        }
    }

    pub fn finish(self) -> IR {
        let entrypoint = self.entrypoint.expect("expected an entrypoint function! mark a function as an entrypoing using fn_builder.mark_entrypoint()");

        IR {
            entrypoint,
            constants: self.constants,
            external_functions: self.external_functions,
            functions: self.functions,
        }
    }

    pub fn constant(&mut self, name: DebugName, payload: Vec<u8>) -> ConstantId {
        let id = self.gen_constant_id.next();

        self.constants.insert(id, Constant { name, payload });

        id
    }

    pub fn external_function(
        &mut self,
        name: String,
        parameters: Vec<FFIValueType>,
        return_type: FFIReturnType,
    ) -> ExternalFunctionId {
        let id = self.gen_external_function_id.next();

        self.external_functions.insert(
            id,
            ExternalFunction {
                name,
                parameters,
                return_type,
            },
        );

        id
    }

    pub fn start_function(&self, name: DebugName) -> FunctionBuilder {
        let id = self.gen_function_id.next();

        FunctionBuilder {
            id,
            marked_main: false,
            name,
            parameters: vec![],
            entry_block: None,
            blocks: FxHashMap::default(),
            gen_block_id: Counter::new(),
            gen_register_id: Arc::new(Counter::new()),
        }
    }

    pub fn end_function(&mut self, builder: FunctionBuilder) {
        let builder_id = builder.id;
        let (is_main, func) = builder.finish();
        self.functions.insert(builder_id, func);

        match (is_main, self.entrypoint) {
            (false, _) => {}
            (true, Some(id)) => panic!(
                "a function is already main! ({} -> {})",
                id,
                self.functions.get(&id).unwrap().name
            ),
            (true, None) => self.entrypoint = Some(builder_id),
        };
    }
}

pub struct FunctionBuilder {
    pub id: FunctionId,
    marked_main: bool,
    name: DebugName,
    parameters: Vec<Parameter>,
    entry_block: Option<BlockId>,
    blocks: FxHashMap<BlockId, FunctionBlock>,
    gen_block_id: Counter<BlockId>,
    gen_register_id: Arc<Counter<RegisterId>>,
}

impl FunctionBuilder {
    fn finish(self) -> (bool, Function) {
        (
            self.marked_main,
            Function {
                name: self.name,
                parameters: self.parameters,
                entry_block: self
                    .entry_block
                    .expect("expected a block to be marked as the entry block"),
                blocks: self.blocks,
            },
        )
    }

    pub fn mark_main(&mut self) {
        if self.parameters.len() > 0 {
            panic!("cannot mark method with parameters as main");
        }

        self.marked_main = true;
    }

    pub fn parameter(&mut self, name: DebugName) -> RegisterId {
        if self.marked_main {
            panic!("cannot add parameters to main method");
        }

        let register = self.gen_register_id.next();

        self.parameters.push(Parameter { name, register });

        register
    }

    pub fn start_block(&self) -> BlockBuilder {
        let id = self.gen_block_id.next();

        BlockBuilder {
            id,
            is_entry: false,
            instructions: vec![],
            gen_register_id: self.gen_register_id.clone(),
        }
    }

    pub fn end_block(&mut self, block: FinishedBlockBuilder) {
        match (self.entry_block, block.is_entry) {
            (None, true) => self.entry_block = Some(block.id),
            (_, false) => {}
            (_, _) => panic!("unexpected state of entrypoint-ness"),
        };

        self.blocks.insert(block.id, block.finish());
    }
}

pub struct BlockBuilder {
    pub id: BlockId,
    is_entry: bool,
    instructions: Vec<Instruction>,
    gen_register_id: Arc<Counter<RegisterId>>,
}

impl BlockBuilder {
    pub fn mark_entrypoint(&mut self) {
        self.is_entry = true;
    }

    pub fn get_runtime(&mut self) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions.push(Instruction::GetRuntime(result));
        result
    }

    pub fn make_string(&mut self, payload: ConstantId) -> RegisterId {
        let result = self.gen_register_id.next();
        self.instructions
            .push(Instruction::MakeString(result, payload));
        result
    }

    pub fn call_external_function(
        &mut self,
        external_function: ExternalFunctionId,
        values: Vec<RegisterId>,
    ) {
        self.instructions.push(Instruction::Call(
            None,
            Callable::External(external_function),
            values,
        ));
    }

    pub fn call_external_function_with_result(
        &mut self,
        external_function: ExternalFunctionId,
        values: Vec<RegisterId>,
    ) -> RegisterId {
        let result = self.gen_register_id.next();

        self.instructions.push(Instruction::Call(
            Some(result),
            Callable::External(external_function),
            values,
        ));

        result
    }

    pub fn ret(self, value: Option<RegisterId>) -> FinishedBlockBuilder {
        self.finish(ControlFlowInstruction::Ret(value))
    }

    fn finish(self, end: ControlFlowInstruction) -> FinishedBlockBuilder {
        FinishedBlockBuilder {
            id: self.id,
            is_entry: self.is_entry,
            instructions: self.instructions,
            end,
        }
    }
}

pub struct FinishedBlockBuilder {
    pub id: BlockId,
    is_entry: bool,
    instructions: Vec<Instruction>,
    end: ControlFlowInstruction,
}

impl FinishedBlockBuilder {
    fn finish(self) -> FunctionBlock {
        FunctionBlock {
            instructions: self.instructions,
            end: self.end,
        }
    }
}

struct Counter<I> {
    current: AtomicUsize,
    phantom: PhantomData<I>,
}

impl<I: IdCompat> Counter<I> {
    pub fn new() -> Self {
        Counter {
            current: AtomicUsize::new(1),
            phantom: PhantomData::default(),
        }
    }

    pub fn next(&self) -> I {
        I::new_with_value(
            self.current
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        )
    }
}
