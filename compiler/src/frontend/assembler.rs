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
use crate::id::*;

#[derive(Clone, Debug)]
pub struct Program {
    constants: FxHashMap<ConstantId, Vec<u8>>,
    external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    functions: FxHashMap<FunctionId, Function>,
}

#[derive(Clone, Debug)]
pub struct ExternalFunction {
    name: String,
    parameters: Vec<Type>,
    returns: ReturnType,
}

#[derive(Clone, Debug)]
pub struct Function {
    entry_block: BlockId,
    blocks: FxHashMap<BlockId, Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    parameters: Vec<Parameter>,
    instructions: Vec<Instruction>,
    end: EndInstruction,
}

#[derive(Clone, Debug)]
pub struct Parameter {
    kind: Type,
    // TODO: maybe parameters should implicitly get the register accoridng to
    // their index? it makes sense not to do this in the other IRs because of
    // mangling parameters, but here we have pretty much all the information
    // necessary to craft a final product
    register: RegisterId,
}

#[derive(Clone, Debug)]
pub enum Instruction {}

#[derive(Clone, Debug)]
pub enum EndInstruction {}

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
    constants: FxHashMap<ConstantId, Vec<u8>>,
    constant_id_gen: Counter<ConstantId>,
    external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    functions: FxHashMap<FunctionId, Function>,
}

impl Assembler {
    pub fn new(mut ir: IR, blocks: Vec<BBlock>, engine: SymbolicEngine) -> Self {
        let external_functions = (ir.external_functions.into_iter())
            .map(|(k, v)| (k, map_ext_fn(v)))
            .collect::<FxHashMap<_, _>>();

        // hack so we can still have a owned `ir` everywhere
        ir.external_functions = Default::default();

        Self {
            constants: Default::default(),
            constant_id_gen: Default::default(),
            external_functions,
            functions: Default::default(),
            ir,
            blocks,
            engine,
        }
    }

    pub fn assemble(mut self) -> Program {
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
            let fn_assembler = FnAssembler::new(
                &self,
                fn_id,
                entry_blk,
                args.iter().map(ValueType::to_type).collect(),
                cntrl_flw.key(),
            );

            let assembled_fn = fn_assembler.assemble();

            todo!("add the assembled function into our list of functions");
        }

        Program {
            constants: self.constants,
            external_functions: self.external_functions,
            functions: self.functions,
        }
    }
}

struct FnAssembler<'duration> {
    assembler: &'duration Assembler,
    function_id: FunctionId,
    entry_block: BlockId,
    invocation_args: Vec<Type>,
    block_key: BlockKey,
}

impl<'d> FnAssembler<'d> {
    pub fn new(
        assembler: &'d Assembler,
        function_id: FunctionId,
        entry_block: BlockId,
        invocation_args: Vec<Type>,
        block_key: BlockKey,
    ) -> Self {
        Self {
            assembler,
            function_id,
            entry_block,
            invocation_args,
            block_key,
        }
    }

    pub fn assemble(self) -> AssembledFn {
        todo!()
    }
}

pub struct AssembledFn {}

impl ValueType {
    pub fn to_type(&self) -> Type {
        Type::Val(self.clone())
    }
}
