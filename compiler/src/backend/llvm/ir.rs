use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, GlobalValue},
};

use crate::{id::*, name::DebugName};
use std::collections::HashMap;

pub struct IR<'ctx> {
    pub constants: HashMap<ConstantId, Constant<'ctx>>,
    pub external_functions: HashMap<ExternalFunctionId, ExternalFunction<'ctx>>,
    pub functions: HashMap<FunctionId, Function<'ctx>>,
    // TODO: pass this as part of the state given to the LLVMCompiler
    // pub internal_slots: HashMap<InternalSlotId, Box<str>>,
}

pub struct Constant<'ctx> {
    pub llvm: GlobalValue<'ctx>,
    pub name: DebugName,
    pub payload: Vec<u8>,
}

pub struct ExternalFunction<'ctx> {
    pub llvm: FunctionValue<'ctx>,
    pub name: String,
    pub parameters: Vec<TypeId>,
    pub return_type: PossibleType,
}

pub struct Function<'ctx> {
    pub llvm: FunctionValue<'ctx>,
    pub name: DebugName,
    pub parameters: Vec<RegisterId>,
    pub return_type: PossibleType,
    pub type_mapping: HashMap<RegisterId, TypeId>,
    pub entry_block: BlockId,
    pub blocks: HashMap<BlockId, Block<'ctx>>,
}

pub struct Block<'ctx> {
    pub llvm_block: BasicBlock<'ctx>,
    pub instructions: Vec<Instruction>,
    pub end_flow: InstructionFlow,
}

/// Handles interning and solving of types (TODO: is that a good desc?)
pub struct TypeManager {
    pub types: HashMap<TypeId, ValueType>,
}

/// Valid types for values
pub enum ValueType {
    /// Useful to box a value into the largest possible idea of what it may be.
    /// Primarily used during prototyping, and is only really useful if our
    /// type system is too immature to detect exact usage of something.
    Any,
    /// Annotated on external functions to signal that they accept a `Runtime`
    /// parameter. All JSSAT functions implicitly have a `Runtime` parameter.
    Runtime,
}

/// Value types for everything (values + fn return type)
pub enum PossibleType {
    Void,
    /// Index into a `TypeManager::types` -> `ValueType`
    Value(TypeId),
}

pub enum Instruction {
    LoadGlobal(RegisterId /*=*/, TopLevelId),
    SaveGlobal(TopLevelId /*=*/, RegisterId),
    RecordGet(RegisterId /*=*/, RegisterId, RecordKey),
    RecordSet(RegisterId, RecordKey, Value),
    RefIsEmpty(RegisterId /*=*/, RegisterId),
    RefDeref(RegisterId /*=*/, RegisterId),
    MakePrimitive {
        result: RegisterId, /*=*/
        strategy: GarbageCollectionStrategy,
        primitive_kind: PrimitiveKind,
    },
    // GcMakeRegion(RegisterId /*=*/),
    // GcEndRegion(RegisterId),
    // GcTracingMarkRoot(RegisterId),
    GcTracingUnmarkRoot(RegisterId),
    Call(Option<RegisterId> /*=*/, Callable, Vec<Value>),
}

#[derive(Debug)]
pub struct BlockImpliesRegister {
    pub block: BlockId,
    pub implies: RegisterId,
}

pub enum InstructionFlow {
    Phi(RegisterId /*=*/, Vec<BlockImpliesRegister>),
    Jmp(BlockId),
    JmpIf(BlockImpliesRegister, BlockId),
    Ret(Option<RegisterId>),
}

#[derive(Debug)]
pub enum Callable {
    GlobalFunction(TopLevelId),
    LocalFunction(RegisterId),
}

#[derive(Debug)]
pub enum RecordKey {
    /// An ECMAScript internal slot. `[[str]]`
    InternalSlot(InternalSlotId),
    Register(RegisterId),
    // Constant(TopLevelId),
}

#[derive(Debug)]
pub enum Value {
    Runtime,
    Register(RegisterId),
    Constant(TopLevelId),
    Number(f64),
}

#[derive(Debug)]
pub enum PrimitiveKind {
    Record,
    List,
}

#[derive(Debug)]
pub enum GarbageCollectionStrategy {
    Tracing,
    // Region(RegisterId),
}
