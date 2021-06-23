use inkwell::types::{BasicType, BasicTypeEnum};

use crate::{backend::runtime_glue::RuntimeGlue, id::*, name::DebugName};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct IR {
    pub constants: HashMap<ConstantId, Constant>,
    pub functions: HashMap<FunctionId, Function>,
    pub external_functions: HashMap<ExternalFunctionId, ExternalFunction>,
    pub entry_function: TopLevelId,
    // TODO: store this as part of the state passed to the skeleton compiler
    // pub internal_slots: HashMap<InternalSlotId, Box<str>>,
}

#[derive(Clone, Debug)]
pub struct Constant {
    pub name: DebugName,
    pub payload: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct ExternalFunction {
    pub name: String,
    pub parameters: Vec<TypeId>,
    pub return_type: PossibleType,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: DebugName,
    pub parameter_types: Vec<RegisterId>,
    pub return_type: PossibleType,
    pub type_mapping: HashMap<RegisterId, TypeId>,
    pub entry_block: BlockId,
    pub blocks: HashMap<BlockId, Block>,
}

#[derive(Clone, Debug)]
pub enum FunctionKind {
    Entrypoint,
    External,
    Code,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub end_flow: InstructionFlow,
}

/// Handles interning and solving of types (TODO: is that a good desc?)
#[derive(Clone, Debug)]
pub struct TypeManager {
    pub types: HashMap<TypeId, ValueType>,
}

pub struct LLVMMonomorphizer<'type_manager, 'context, 'module, 'glue> {
    type_manager: &'type_manager TypeManager,
    llvm_types: HashMap<TypeId, BasicTypeEnum<'context>>,
    runtime_glue: &'glue RuntimeGlue<'context, 'module>,
}

impl<'t, 'c, 'm, 'g> LLVMMonomorphizer<'t, 'c, 'm, 'g> {
    pub fn new(type_manager: &'t TypeManager, runtime_glue: &'g RuntimeGlue<'c, 'm>) -> Self {
        Self {
            type_manager,
            llvm_types: HashMap::new(),
            runtime_glue,
        }
    }

    pub fn llvm_type(&mut self, id: TypeId) -> BasicTypeEnum<'c> {
        if let Some(llvm_type) = self.llvm_types.get(&id) {
            return *llvm_type;
        }

        let value_type = self
            .type_manager
            .types
            .get(&id)
            .expect("expected typeid -> valuetype mapping");

        let llvm_type = match value_type {
            ValueType::Any => self.runtime_glue.type_value.as_basic_type_enum(),
            ValueType::Runtime => self.runtime_glue.type_runtime.as_basic_type_enum(),
        };

        self.llvm_types.insert(id, llvm_type);
        *self.llvm_types.get(&id).unwrap()
    }
}

/// Valid types for values
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
#[derive(Clone, Debug)]
pub enum PossibleType {
    Void,
    /// Index into a `TypeManager::types` -> `ValueType`
    Value(TypeId),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct BlockImpliesRegister {
    pub block: BlockId,
    pub implies: RegisterId,
}

#[derive(Clone, Debug)]
pub enum InstructionFlow {
    Phi(RegisterId /*=*/, Vec<BlockImpliesRegister>),
    Jmp(BlockId),
    JmpIf(BlockImpliesRegister, BlockId),
    Ret(Option<RegisterId>),
}

#[derive(Clone, Debug)]
pub enum Callable {
    GlobalFunction(TopLevelId),
    LocalFunction(RegisterId),
}

#[derive(Clone, Debug)]
pub enum RecordKey {
    /// An ECMAScript internal slot. `[[str]]`
    InternalSlot(InternalSlotId),
    Register(RegisterId),
    // Constant(TopLevelId),
}

#[derive(Clone, Debug)]
pub enum Value {
    Runtime,
    Register(RegisterId),
    Constant(TopLevelId),
    Number(f64),
}

#[derive(Clone, Debug)]
pub enum PrimitiveKind {
    Record,
    List,
}

#[derive(Clone, Debug)]
pub enum GarbageCollectionStrategy {
    Tracing,
    // Region(RegisterId),
}
