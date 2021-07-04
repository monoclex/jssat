//! Assembles together the informatino from the original IR, and type_annotator
//! passes into a fully conherent IR, which will then be passed to `skeleton`.

use std::collections::VecDeque;

use rustc_hash::FxHashMap;

use super::{
    conv_only_bb::Block as BBlock,
    ir,
    ir::{FFIValueType, IR},
    type_annotater::{BlockKey, ExplorationBranch, SymbolicEngine, ValueType},
};
use crate::id::{self, *};

#[derive(Clone, Debug)]
pub struct Program {
    constants: FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>,
    external_functions: FxHashMap<ExternalFunctionId<AssemblerCtx>, ExternalFunction>,
    functions: FxHashMap<FunctionId<AssemblerCtx>, Function>,
}

#[derive(Clone, Debug)]
pub struct ExternalFunction {
    name: String,
    parameters: Vec<Type>,
    returns: ReturnType,
}

#[derive(Clone, Debug)]
pub struct Function {
    entry_block: BlockId<AssemblerCtx>,
    blocks: FxHashMap<BlockId<AssemblerCtx>, Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    parameters: Vec<Parameter>,
    instructions: Vec<Instruction>,
    end: EndInstruction,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    typ: Type,
    // TODO: maybe parameters should implicitly get the register accoridng to
    // their index? it makes sense not to do this in the other IRs because of
    // mangling parameters, but here we have pretty much all the information
    // necessary to craft a final product
    register: RegisterId<AssemblerCtx>,
}

#[derive(Clone, Debug)]
pub enum Instruction {}

#[derive(Clone, Debug)]
pub enum EndInstruction {
    T,
}

#[derive(Clone, Debug)]
pub enum ReturnType {
    Void,
    Value(Type),
}

#[derive(Clone, Debug)]
pub enum Type {
    FFI(FFIValueType),
    Val(ValueType),
}

pub fn assemble(ir: IR, blocks: Vec<BBlock>, engine: SymbolicEngine) -> Program {
    Assembler::new(ir, blocks, engine).assemble()
}

fn map_ext_fn(exernal_function: ir::ExternalFunction) -> ExternalFunction {
    ExternalFunction {
        name: exernal_function.name,
        parameters: exernal_function
            .parameters
            .into_iter()
            .map(ir::FFIValueType::to_type)
            .collect(),
        returns: exernal_function.return_type.to_type(),
    }
}

impl ir::FFIReturnType {
    fn to_type(self) -> ReturnType {
        match self {
            ir::FFIReturnType::Void => ReturnType::Void,
            ir::FFIReturnType::Value(v) => ReturnType::Value(v.to_type()),
        }
    }
}

impl ir::FFIValueType {
    fn to_type(self) -> Type {
        Type::FFI(self)
    }
}

struct Assembler {
    ir: IR,
    blocks: Vec<BBlock>,
    engine: SymbolicEngine,
    //
    constants: FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>,
    constant_id_gen: Counter<ConstantId<AssemblerCtx>>,
    external_functions: FxHashMap<ExternalFunctionId<AssemblerCtx>, ExternalFunction>,
    functions: FxHashMap<FunctionId<AssemblerCtx>, Function>,
    //
    function_ids: FxHashMap<(FunctionId<NoContext>, BlockId<NoContext>), FunctionId<AssemblerCtx>>,
}

impl Assembler {
    pub fn new(mut ir: IR, blocks: Vec<BBlock>, engine: SymbolicEngine) -> Self {
        let external_functions = (ir.external_functions.into_iter())
            .map(|(k, v)| (k.map_context::<AssemblerCtx>(), map_ext_fn(v)))
            .collect::<FxHashMap<_, _>>();

        // hack so we can still have a owned `ir` everywhere
        ir.external_functions = Default::default();

        Self {
            ir,
            blocks,
            engine,
            constants: Default::default(),
            constant_id_gen: Default::default(),
            external_functions,
            functions: Default::default(),
            function_ids: Default::default(),
        }
    }

    pub fn assemble(mut self) -> Program {
        self.assign_fn_ids();

        let mut built_fns = Vec::new();

        // go through every function in the program and assemble it
        for (fn_id, entry_blk, args, cntrl_flw) in self
            .engine
            .executions
            .all_fn_invocations()
            .filter(|(fn_id, blk_id, _, _)| {
                // only allow blocks that are the entry block
                self.ir.functions.get(fn_id).unwrap().entry_block == *blk_id
            })
        {
            // at this point, we will have only function ids, their entry blocks,
            // and the pairs of arguments passed to the function to invoke
            let fn_assembler =
                FnAssembler::new(&self, fn_id, entry_blk, args.clone(), cntrl_flw.key());

            let assembled_fn = fn_assembler.assemble();
            built_fns.push((fn_id, entry_blk, assembled_fn));
        }

        for (fn_id, blk_id, func) in built_fns {
            self.functions.insert(self.get_fn_id(fn_id, blk_id), func);
        }

        Program {
            constants: self.constants,
            external_functions: self.external_functions,
            functions: self.functions,
        }
    }

    fn get_fn_id(
        &self,
        fn_id: FunctionId<NoContext>,
        blk_id: BlockId<NoContext>,
    ) -> FunctionId<AssemblerCtx> {
        *self.function_ids.get(&(fn_id, blk_id)).unwrap()
    }

    fn assign_fn_ids(&mut self) {
        let id_gen = Counter::new();

        let functions = &self.ir.functions;
        for (fn_id, entry_blk, _, _) in
            self.engine
                .executions
                .all_fn_invocations()
                .filter(|(fn_id, blk_id, _, _)| {
                    // only allow blocks that are the entry block
                    functions.get(fn_id).unwrap().entry_block == *blk_id
                })
        {
            self.function_ids.insert((fn_id, entry_blk), id_gen.next());
        }
    }

    fn find_block(&self, fn_id: FunctionId, blk_id: BlockId) -> &BBlock {
        self.blocks
            .iter()
            .filter(|b| b.derived_from == (fn_id, blk_id))
            .next()
            .unwrap()
    }
}

struct FnAssembler<'duration> {
    assembler: &'duration Assembler,
    function_id: FunctionId<NoContext>,
    entry_block: BlockId<NoContext>,
    invocation_args: Vec<ValueType>,
    block_key: BlockKey,
    block_id_gen: Counter<BlockId<AssemblerCtx>>,
    block_id_map: FxHashMap<BlockId<NoContext>, BlockId<AssemblerCtx>>,
}

impl<'d> FnAssembler<'d> {
    pub fn new(
        assembler: &'d Assembler,
        function_id: FunctionId<NoContext>,
        entry_block: BlockId<NoContext>,
        invocation_args: Vec<ValueType>,
        block_key: BlockKey,
    ) -> Self {
        Self {
            assembler,
            function_id,
            entry_block,
            invocation_args,
            block_key,
            block_id_gen: Counter::new(),
            block_id_map: Default::default(),
        }
    }

    pub fn assemble(mut self) -> Function {
        let mut blocks = FxHashMap::default();

        let mut blocks_to_assemble = VecDeque::new();
        blocks_to_assemble.push_back((
            self.entry_block,
            std::mem::replace(&mut self.invocation_args, Vec::new()),
        ));

        let typed_fn = (self.assembler.engine.typed_blocks)
            .get(&self.block_key)
            .unwrap();

        while let Some((block, args)) = blocks_to_assemble.pop_front() {
            let id = self.id_of(block);

            let (branch, register_types) = typed_fn.find(&block, &args);
            let block_src = self.assembler.find_block(self.block_key.function, block);

            let parameters = (args.iter())
                .zip(block_src.parameters.iter())
                // we can ignore parameters with constant values, as we will
                // always know their value
                .filter(|(typ, _)| !typ.is_const())
                .map(|(typ, register)| Parameter {
                    typ: typ.to_type(),
                    register: register.map_context::<AssemblerCtx>(),
                })
                .collect::<Vec<_>>();

            let instructions = Vec::new();
            let end = EndInstruction::T;

            let block = Block {
                parameters,
                instructions,
                end,
            };

            blocks.insert(id, block);
        }

        Function {
            entry_block: self.id_of(self.entry_block),
            blocks,
        }
    }

    fn id_of(&mut self, blk_id: BlockId<NoContext>) -> BlockId<AssemblerCtx> {
        let id_gen = &self.block_id_gen;
        *(self.block_id_map)
            .entry(blk_id)
            .or_insert_with(|| id_gen.next())
    }
}

impl ValueType {
    pub fn to_type(&self) -> Type {
        Type::Val(self.clone())
    }

    pub fn is_const(&self) -> bool {
        match self {
            ValueType::Any
            | ValueType::Runtime
            | ValueType::String
            | ValueType::Number
            | ValueType::BytePointer
            | ValueType::Pointer(_)
            | ValueType::Word
            | ValueType::Boolean => false,
            ValueType::ExactNumber(_) | ValueType::ExactString(_) | ValueType::Bool(_) => true,
        }
    }
}
