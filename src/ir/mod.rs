// use petgraph::Directed;
use std::{collections::HashMap, num::NonZeroUsize, sync::Arc};

// mod formatting;

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
    constants: Vec<Constant>,
    global_variables: Vec<GlobalVariable>,
    external_functions: Vec<ExternalFunction>,
    functions: Vec<Function>,
    // TODO: `pub data: Vec<DataDeclaration>`
    debug_info: IRDebugInfo,
}

#[derive(Debug)]
pub struct IRDebugInfo {
    top_level_names: HashMap<TopLevelId, Box<str>>,
}

#[derive(Debug)]
pub struct Constant {
    id: TopLevelId,
    payload: Vec<u8>,
}

#[derive(Debug)]
pub struct GlobalVariable {
    id: TopLevelId,
}

#[derive(Debug)]
pub struct ExternalFunction {
    id: TopLevelId,
    parameters: Vec<TypedParameter>,
}

#[derive(Debug)]
pub struct TypedParameter {
    register: RegisterId,
    kind: Type,
}

#[derive(Debug)]
pub struct Function {
    id: TopLevelId,
    arguments: Vec<Parameter>,
    body: FunctionBody,
}

#[derive(Debug)]
pub struct Parameter {
    register: RegisterId,
}

#[derive(Debug)]
pub enum Type {
    Any,
}

#[derive(Debug)]
pub struct FunctionBody {
    blocks: Vec<FunctionBlock>,
}

#[derive(Debug)]
pub struct FunctionBlock {
    id: BlockId,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum TypeRecord {
    Todo,
}

#[derive(Debug)]
pub struct BlockImpliesRegister {
    block: BlockId,
    implies: RegisterId,
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
    Call(Option<RegisterId> /*=*/, Callable),
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
