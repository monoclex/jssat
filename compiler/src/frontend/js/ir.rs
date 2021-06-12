// use petgraph::Directed;
use crate::{
    id::*,
    name::{DebugName, Name},
};
use std::collections::HashMap;

// pub type ControlFlowGraph = petgraph::graph::DiGraph<BlockId, (), usize>;
// pub type ValueFlowGraph = petgraph::graph::DiGraph<RegisterId, (), usize>;

#[derive(Debug)]
pub struct IR {
    pub constants: HashMap<TopLevelId, Constant>,
    pub global_variables: HashMap<TopLevelId, GlobalVariable>,
    pub external_functions: HashMap<TopLevelId, ExternalFunction>,
    pub functions: HashMap<TopLevelId, Function>,
    // TODO: `pub data: Vec<DataDeclaration>`
}

#[derive(Debug)]
pub struct Constant {
    pub name: DebugName,
    pub payload: Vec<u8>,
}

#[derive(Debug)]
pub struct GlobalVariable {
    pub name: DebugName,
}

#[derive(Debug)]
pub struct ExternalFunction {
    pub name: Name,
    pub parameters: Vec<TypedParameter>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct TypedParameter {
    pub kind: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: DebugName,
    pub parameters: Vec<Parameter>,
    pub body: FunctionBody,
    pub debug_info: FunctionDebugInfo,
    pub is_main: bool,
}

#[derive(Debug)]
pub struct FunctionDebugInfo {
    pub register_names: HashMap<RegisterId, DebugName>,
}

#[derive(Debug)]
pub struct Parameter {
    pub register: RegisterId,
}

// TODO: have some kind of `SolvedType` versus `UnsolvedType` for types that
// utilize generics and other things that would be both:
//
// - hard for the backend to understand (the backend can only understand
//   "concrete" types, like solved types)
// - maybe not possible to derive `Hash` for (speculation on my part)
//
// in addition, we'd need an `InternedType` which has inner `TypeId`s nested
// for references to other types. or perhaps we should just do that from the
// getgo?
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    // TODO: bring back these types once we have a more mature type system
    // List(Box<Type>, Option<usize>),
    // Integer(usize),
}

#[derive(Debug)]
pub struct FunctionBody {
    pub blocks: Vec<FunctionBlock>,
    pub debug_info: FunctionBodyDebugInfo,
}

#[derive(Debug)]
pub struct FunctionBodyDebugInfo {
    pub block_names: HashMap<BlockId, DebugName>,
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
    // ECMAScriptAbstractOperation(ECMAScriptAbstractOperation),
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
    Runtime,
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
        }
    }
}
