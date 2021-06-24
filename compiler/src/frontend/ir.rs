use rustc_hash::FxHashMap;

use crate::{id::*, name::DebugName};

pub type ControlFlowGraph = petgraph::graph::DiGraph<BlockId, (), usize>;
pub type ValueFlowGraph = petgraph::graph::DiGraph<RegisterId, (), usize>;

#[derive(Debug)]
pub struct IR {
    pub entrypoint: FunctionId,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub functions: FxHashMap<FunctionId, Function>,
    // TODO: `pub data: Vec<DataDeclaration>`
    // ^ todo: remember why (i think this is only applicable after type analysis)
}

#[derive(Debug)]
pub struct Constant {
    pub name: DebugName,
    pub payload: Vec<u8>,
}

#[derive(Debug)]
pub struct ExternalFunction {
    pub name: String,
    pub parameters: Vec<FFIValueType>,
    pub return_type: FFIReturnType,
}

#[derive(Debug)]
pub enum FFIValueType {
    /// Useful to box a value into the largest possible idea of what it may be.
    /// Primarily used during prototyping, and is only really useful if our
    /// type system is too immature to detect exact usage of something.
    Any,
    /// Annotated on external functions to signal that they accept a `Runtime`
    /// parameter. All JSSAT functions implicitly have a `Runtime` parameter.
    Runtime,
}

#[derive(Debug)]
pub enum FFIReturnType {
    Void,
    Value(FFIValueType),
}

#[derive(Debug)]
pub struct Function {
    pub name: DebugName,
    pub parameters: Vec<Parameter>,
    // pub control_flow: ControlFlowGraph,
    // pub register_flow: ValueFlowGraph,
    pub entry_block: BlockId,
    pub blocks: FxHashMap<BlockId, FunctionBlock>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: DebugName,
    pub register: RegisterId,
}

#[derive(Debug, Clone)]
pub struct FunctionBlock {
    pub instructions: Vec<Instruction>,
    pub end: ControlFlowInstruction,
}

#[derive(Debug)]
pub struct BlockImpliesRegister {
    pub block: BlockId,
    pub implies: RegisterId,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // RecordGet(RegisterId /*=*/, RegisterId, RecordKey),
    // RecordSet(RegisterId, RecordKey, Value),
    // RefIsEmpty(RegisterId /*=*/, RegisterId),
    // RefDeref(RegisterId /*=*/, RegisterId),
    // MakePrimitive(RegisterId /*=*/, PrimtiveCreationDetails),
    // FAR FUTURE: GcMakeRegion(RegisterId /*=*/),
    // FAR FUTURE: GcEndRegion(RegisterId),
    // FAR FUTURE: GcTracingMarkRoot(RegisterId),
    // FAR FUTURE: GcTracingUnmarkRoot(RegisterId),
    Call(Option<RegisterId> /*=*/, Callable, Vec<RegisterId>),
    // Phi(RegisterId /*=*/, Vec<BlockImpliesRegister>),
    /// # `GetRuntime`
    ///
    /// Stores a pointer to the JSSAT Runtime in the specified register.
    GetRuntime(RegisterId),
    /// # `MakeString`
    ///
    /// Will instantiate a string, using the constant referenced as payload for
    /// the value of the string. JS strings are UTF-16, so it is expected that
    /// the constant referenced is a valid UTF-16 string.
    MakeString(RegisterId, ConstantId),
}

#[derive(Debug, Clone)]
pub enum ControlFlowInstruction {
    // Jmp(BlockId),
    // JmpIf(BlockImpliesRegister, BlockId),
    Ret(Option<RegisterId>),
}

impl Instruction {
    pub fn assigned_to(&self) -> Option<RegisterId> {
        match self {
            // Self::LoadGlobal(to, _)
            // | Self::RecordGet(to, _, _)
            // | Self::RefIsEmpty(to, _)
            // | Self::RefDeref(to, _)
            // | Self::MakePrimitive(to, _)
            // | Self::GcMakeRegion(to)
            // | Self::Phi(to, _) => Some(*to),
            Self::Call(to, _, _) => *to,
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    External(ExternalFunctionId),
    Static(FunctionId),
    /// virtual just means fn pointer
    Virtual(RegisterId),
}

#[derive(Debug)]
pub enum RecordKey {
    /// An ECMAScript internal slot. `[[str]]`
    InternalSlot(&'static str),
    Register(RegisterId),
    Constant(TopLevelId),
}

#[derive(Debug)]
pub enum Value {
    Runtime,
    Register(RegisterId),
    Constant(ConstantId),
    Number(f64),
}

#[derive(Debug)]
pub enum PrimtiveCreationDetails {
    Record,
    List,
}

impl IR {
    pub fn new() -> Self {
        IR {
            entrypoint: FunctionId::new(),
            constants: FxHashMap::default(),
            external_functions: FxHashMap::default(),
            functions: FxHashMap::default(),
        }
    }
}
