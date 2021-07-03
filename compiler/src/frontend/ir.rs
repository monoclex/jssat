use rustc_hash::FxHashMap;

use crate::{id::*, name::DebugName};

#[derive(Debug)]
pub struct IR {
    pub entrypoint: FunctionId,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub functions: FxHashMap<FunctionId, Function>,
}

#[derive(Debug)]
pub struct Constant {
    pub name: DebugName,
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct ExternalFunction {
    pub name: String,
    pub parameters: Vec<FFIValueType>,
    pub return_type: FFIReturnType,
}

#[derive(Debug, Clone)]
pub enum FFIValueType {
    /// Useful to box a value into the largest possible idea of what it may be.
    /// Primarily used during prototyping, and is only really useful if our
    /// type system is too immature to detect exact usage of something.
    Any,
    /// Annotated on external functions to signal that they accept a `Runtime`
    /// parameter. All JSSAT functions implicitly have a `Runtime` parameter.
    Runtime,
    /// Pointer to a contiguous amount of bytes, `*const u8` or `void*`
    BytePointer,
    /// Pointer to bits of some size.
    /// Pointer(2) would be `i2*`, Pointer(16) would be `i16*`.
    Pointer(u16),
    /// A value dependent on the machine's size - akin to `usize`, or `size_t`.
    /// Either 32 bits or 64 bits.
    Word,
    /// A parameter of the [`jssatrt::string::String`] type.
    String,
}

#[derive(Debug, Clone)]
pub enum FFIReturnType {
    Void,
    Value(FFIValueType),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: DebugName,
    pub parameters: Vec<Parameter>,
    // require that the entry block has 0 parameters
    pub entry_block: BlockId,
    pub blocks: FxHashMap<BlockId, FunctionBlock>,
    // pub control_flow: ControlFlowGraph,
    // pub register_flow: ValueFlowGraph,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: DebugName,
    pub register: RegisterId,
}

#[derive(Debug, Clone)]
pub struct FunctionBlock {
    pub parameters: Vec<RegisterId>,
    pub instructions: Vec<Instruction>,
    pub end: ControlFlowInstruction,
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
    /// # [`Instruction::Unreachable`]
    ///
    /// Indicates that the executing code path will never reach this instruction.
    /// It is undefined behavior for code to reach an Unreachable instruction.
    /// This is used to implement functions that recurse an unknown amount of
    /// times.
    Unreachable,
    MakeNumber(RegisterId, f64),
    CompareLessThan(RegisterId, RegisterId, RegisterId),
    Add(RegisterId, RegisterId, RegisterId),
}

#[derive(Debug, Clone)]
pub struct BasicBlockJump(pub BlockId, pub Vec<RegisterId>);

#[derive(Debug, Clone)]
pub enum ControlFlowInstruction {
    Jmp(BasicBlockJump),
    JmpIf {
        condition: RegisterId,
        true_path: BasicBlockJump,
        false_path: BasicBlockJump,
    },
    Ret(Option<RegisterId>),
}

#[derive(Debug, Clone)]
pub enum Callable {
    External(ExternalFunctionId),
    Static(FunctionId),
    // virtual just means fn pointer
    // Virtual(RegisterId),
}

// pub enum RecordKey {
//     /// An ECMAScript internal slot. `[[str]]`
//     InternalSlot(&'static str),
//     Register(RegisterId),
//     Constant(TopLevelId),
// }

impl Instruction {
    pub fn assigned_to(&self) -> Option<RegisterId> {
        match self {
            Instruction::Call(result, _, _) => *result,
            Instruction::Unreachable => None,
            Instruction::GetRuntime(result)
            | Instruction::MakeString(result, _)
            | Instruction::MakeNumber(result, _)
            | Instruction::CompareLessThan(result, _, _)
            | Instruction::Add(result, _, _) => Some(*result),
        }
    }

    pub fn used_registers(&self) -> Vec<RegisterId> {
        match self {
            Instruction::Call(_, _, params) => params.clone(),
            Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
                vec![*lhs, *rhs]
            }
            Instruction::Unreachable
            | Instruction::GetRuntime(_)
            | Instruction::MakeString(_, _)
            | Instruction::MakeNumber(_, _) => Vec::new(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            Instruction::Call(_, _, params) => params.iter_mut().collect(),
            Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
                vec![lhs, rhs]
            }
            Instruction::GetRuntime(_)
            | Instruction::MakeString(_, _)
            | Instruction::Unreachable
            | Instruction::MakeNumber(_, _) => Vec::new(),
        }
    }
}

impl ControlFlowInstruction {
    pub fn used_registers(&self) -> Option<RegisterId> {
        match self {
            ControlFlowInstruction::Jmp(_) => None,
            ControlFlowInstruction::JmpIf { condition, .. } => Some(*condition),
            ControlFlowInstruction::Ret(register) => *register,
        }
    }

    pub fn used_registers_mut(&mut self) -> Option<&mut RegisterId> {
        match self {
            ControlFlowInstruction::Jmp(_) => None,
            ControlFlowInstruction::JmpIf { condition, .. } => Some(condition),
            ControlFlowInstruction::Ret(register) => register.as_mut(),
        }
    }

    pub fn children(&self) -> Vec<&BasicBlockJump> {
        match self {
            ControlFlowInstruction::Jmp(block) => vec![block],
            ControlFlowInstruction::JmpIf {
                true_path,
                false_path,
                ..
            } => vec![true_path, false_path],
            ControlFlowInstruction::Ret(_) => Vec::new(),
        }
    }

    pub fn children_mut(&mut self) -> Vec<&mut BasicBlockJump> {
        match self {
            ControlFlowInstruction::Jmp(block) => vec![block],
            ControlFlowInstruction::JmpIf {
                true_path,
                false_path,
                ..
            } => vec![true_path, false_path],
            ControlFlowInstruction::Ret(_) => Vec::new(),
        }
    }
}
