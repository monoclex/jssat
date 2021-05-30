// use petgraph::Directed;
use std::{collections::HashMap, num::NonZeroUsize, sync::Arc};

// mod formatting;
pub mod builder;

// pub type ControlFlowGraph = petgraph::graph::DiGraph<BlockId, (), usize>;
// pub type ValueFlowGraph = petgraph::graph::DiGraph<RegisterId, (), usize>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegisterId(NonZeroUsize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(NonZeroUsize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TopLevelId(NonZeroUsize);

#[derive(Debug)]
pub struct IR {
    pub constants: HashMap<TopLevelId, Constant>,
    pub global_variables: HashMap<TopLevelId, GlobalVariable>,
    pub external_functions: HashMap<TopLevelId, ExternalFunction>,
    pub functions: HashMap<TopLevelId, Function>,
    // TODO: `pub data: Vec<DataDeclaration>`
    pub debug_info: IRDebugInfo,
}

#[derive(Debug)]
pub struct IRDebugInfo {
    pub top_level_names: HashMap<TopLevelId, Box<str>>,
    // TODO: information in source about where generated from, or what opt
    // passes have occurred
}

#[derive(Debug)]
pub struct Constant {
    pub payload: Vec<u8>,
}

#[derive(Debug)]
pub struct GlobalVariable {}

#[derive(Debug)]
pub struct ExternalFunction {
    pub parameters: Vec<TypedParameter>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct TypedParameter {
    pub kind: Type,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<Parameter>,
    pub body: FunctionBody,
    pub debug_info: FunctionDebugInfo,
    pub is_main: bool,
}

#[derive(Debug)]
pub struct FunctionDebugInfo {
    pub register_names: HashMap<RegisterId, Box<str>>,
}

#[derive(Debug)]
pub struct Parameter {
    pub register: RegisterId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Useful to box a value into the largest possible idea of what it may be.
    /// Primarily used during prototyping, and is only really useful if our
    /// type system is too immature to detect exact usage of something.
    Any,
    /// Annotated on external functions to signal that they accept a `Runtime`
    /// parameter. All JSSAT functions implicitly have a `Runtime` parameter.
    Runtime,
    /// Used to annotate functions that do not return anything.
    Void,
    // TODO: why is this used?
    Constant(Vec<u8>),
    // List(Box<Type>, Option<usize>),
    // TODO: do we only accomodate i64 integers or more?
    // Integer(usize),
}

#[derive(Debug)]
pub struct FunctionBody {
    pub blocks: Vec<FunctionBlock>,
    pub debug_info: FunctionBodyDebugInfo,
}

#[derive(Debug)]
pub struct FunctionBodyDebugInfo {
    pub block_names: HashMap<BlockId, Box<str>>,
}

#[derive(Debug)]
pub struct FunctionBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct BlockImpliesRegister {
    pub block: BlockId,
    pub implies: RegisterId,
}

#[derive(Debug)]
pub enum Instruction {
    ECMAScriptAbstractOperation(ECMAScriptAbstractOperation),
    LoadGlobal(RegisterId /*=*/, TopLevelId),
    SaveGlobal(TopLevelId /*=*/, RegisterId),
    RecordGet(RegisterId /*=*/, RegisterId, RecordKey),
    RecordSet(RegisterId, RecordKey, Value),
    RefIsEmpty(RegisterId /*=*/, RegisterId),
    RefDeref(RegisterId /*=*/, RegisterId),
    MakePrimitive(RegisterId /*=*/, PrimtiveCreationDetails),
    GcMakeRegion(RegisterId /*=*/),
    GcEndRegion(RegisterId),
    GcTracingMarkRoot(RegisterId),
    GcTracingUnmarkRoot(RegisterId),
    Call(Option<RegisterId> /*=*/, Callable, Vec<Value>),
    Phi(RegisterId /*=*/, Vec<BlockImpliesRegister>),
    Jmp(BlockId),
    JmpIf(BlockImpliesRegister, BlockId),
    Ret(Option<RegisterId>),
}

impl Instruction {
    pub fn assigned_to(&self) -> Option<RegisterId> {
        match self {
            Self::LoadGlobal(to, _)
            | Self::RecordGet(to, _, _)
            | Self::RefIsEmpty(to, _)
            | Self::RefDeref(to, _)
            | Self::MakePrimitive(to, _)
            | Self::GcMakeRegion(to)
            | Self::Phi(to, _) => Some(*to),
            Self::Call(to, _, _) => *to,
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Callable {
    GlobalFunction(TopLevelId),
    LocalFunction(RegisterId),
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
    Register(RegisterId),
    Constant(TopLevelId),
    Number(f64),
}

#[derive(Debug)]
pub enum PrimtiveCreationDetails {
    Record(GarbageCollectionStrategy),
    List(GarbageCollectionStrategy),
}

#[derive(Debug)]
pub enum GarbageCollectionStrategy {
    Tracing,
    Region(RegisterId),
}

#[derive(Debug)]
pub enum ECMAScriptAbstractOperation {
    InitializeHostDefinedRealm(RegisterId /*=*/),
    CreateRealm(RegisterId /*=*/),
    // CreateIntrinsics(RegisterId),
    // CreateBuiltinFunction(TopLevelId),
}

impl IR {
    pub fn new() -> Self {
        IR {
            constants: HashMap::new(),
            global_variables: HashMap::new(),
            external_functions: HashMap::new(),
            functions: HashMap::new(),
            debug_info: IRDebugInfo::new(),
        }
    }
}

impl IRDebugInfo {
    pub fn new() -> Self {
        IRDebugInfo {
            top_level_names: HashMap::new(),
        }
    }
}

impl RegisterId {
    pub fn first() -> Self {
        Self(NonZeroUsize::new(1).unwrap())
    }

    pub fn next(&self) -> Self {
        Self(NonZeroUsize::new(self.0.get() + 1).unwrap())
    }

    pub fn value(&self) -> usize {
        self.0.get()
    }
}

impl BlockId {
    pub fn first() -> Self {
        Self(NonZeroUsize::new(1).unwrap())
    }

    pub fn next(&self) -> Self {
        Self(NonZeroUsize::new(self.0.get() + 1).unwrap())
    }

    pub fn value(&self) -> usize {
        self.0.get()
    }
}

impl TopLevelId {
    pub fn first() -> Self {
        Self(NonZeroUsize::new(1).unwrap())
    }

    pub fn next(&self) -> Self {
        Self(NonZeroUsize::new(self.0.get() + 1).unwrap())
    }

    pub fn value(&self) -> usize {
        self.0.get()
    }
}
