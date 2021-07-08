use std::hash::Hash;

use rustc_hash::FxHashMap;

use crate::name::DebugName;

use crate::id::{ContextTag, IrCtx};
type BlockId = crate::id::BlockId<IrCtx>;
type FunctionId = crate::id::FunctionId<IrCtx>;
type ConstantId = crate::id::ConstantId<IrCtx>;
use crate::id::RegisterId;
type PlainRegisterId = RegisterId<IrCtx>;
type ExternalFunctionId = crate::id::ExternalFunctionId<IrCtx>;

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
    /// A parameter of the [`jssatrt::string::String`] type.
    String,
    /// Pointer to bits of some size.
    /// Pointer(2) would be `i2*`, Pointer(16) would be `i16*`.
    Pointer(u16),
    /// A value dependent on the machine's size - akin to `usize`, or `size_t`.
    /// Either 32 bits or 64 bits.
    Word,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Returns<T> {
    Value(T),
    Void,
}

pub type FFIReturnType = Returns<FFIValueType>;

// #[derive(Debug, Clone)]
// pub enum FFIReturnType {
//     Void,
//     Value(FFIValueType),
// }

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
    pub register: PlainRegisterId,
}

#[derive(Debug, Clone)]
pub struct FunctionBlock {
    pub parameters: Vec<PlainRegisterId>,
    pub instructions: Vec<Instruction>,
    pub end: ControlFlowInstruction,
}

#[derive(Debug, Clone)]
pub enum Instruction<C = crate::id::IrCtx> {
    // RecordGet(RegisterId /*=*/, RegisterId, RecordKey),
    // RecordSet(RegisterId, RecordKey, Value),
    // RefIsEmpty(RegisterId /*=*/, RegisterId),
    // RefDeref(RegisterId /*=*/, RegisterId),
    // MakePrimitive(RegisterId /*=*/, PrimtiveCreationDetails),
    // FAR FUTURE: GcMakeRegion(RegisterId /*=*/),
    // FAR FUTURE: GcEndRegion(RegisterId),
    // FAR FUTURE: GcTracingMarkRoot(RegisterId),
    // FAR FUTURE: GcTracingUnmarkRoot(RegisterId),
    Call(
        Option<RegisterId<C>>, /*=*/
        Callable,
        Vec<RegisterId<C>>,
    ),
    // Phi(RegisterId /*=*/, Vec<BlockImpliesRegister>),
    /// # `GetRuntime`
    ///
    /// Stores a pointer to the JSSAT Runtime in the specified register.
    GetRuntime(RegisterId<C>),
    /// # `MakeString`
    ///
    /// Will instantiate a string, using the constant referenced as payload for
    /// the value of the string. JS strings are UTF-16, so it is expected that
    /// the constant referenced is a valid UTF-16 string.
    // TODO: the conv_bb_block phase doesn't mutate constants, so they're still
    // in the old constant phase. is this valid?
    MakeString(RegisterId<C>, crate::id::ConstantId<C>),
    // /// # [`Instruction::Unreachable`]
    // ///
    // /// Indicates that the executing code path will never reach this instruction.
    // /// It is undefined behavior for code to reach an Unreachable instruction.
    // /// This is used to implement functions that recurse an unknown amount of
    // /// times.
    // Unreachable,
    MakeInteger(RegisterId<C>, i64),
    CompareLessThan(RegisterId<C>, RegisterId<C>, RegisterId<C>),
    Add(RegisterId<C>, RegisterId<C>, RegisterId<C>),
}

#[derive(Debug, Clone)]
pub struct BasicBlockJump<C = IrCtx, Path = IrCtx>(
    pub crate::id::BlockId<Path>,
    pub Vec<RegisterId<C>>,
);

#[derive(Debug, Clone)]
pub enum ControlFlowInstruction<Ctx = IrCtx, Path = IrCtx> {
    Jmp(BasicBlockJump<Ctx, Path>),
    JmpIf {
        condition: RegisterId<Ctx>,
        true_path: BasicBlockJump<Ctx, Path>,
        false_path: BasicBlockJump<Ctx, Path>,
    },
    Ret(Option<RegisterId<Ctx>>),
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

impl<C: ContextTag> Instruction<C> {
    // this should turn into a no-op lol
    pub fn map_context<C2: ContextTag>(self) -> Instruction<C2> {
        match self {
            Instruction::Call(result, callable, args) => Instruction::Call(
                result.map(|r| r.map_context::<C2>()),
                callable,
                args.into_iter().map(|r| r.map_context::<C2>()).collect(),
            ),
            Instruction::GetRuntime(r) => Instruction::GetRuntime(r.map_context::<C2>()),
            Instruction::MakeString(r, s) => {
                Instruction::MakeString(r.map_context::<C2>(), s.map_context::<C2>())
            }
            Instruction::MakeInteger(r, n) => Instruction::MakeInteger(r.map_context::<C2>(), n),
            Instruction::CompareLessThan(r, l, rhs) => Instruction::CompareLessThan(
                r.map_context::<C2>(),
                l.map_context::<C2>(),
                rhs.map_context::<C2>(),
            ),
            Instruction::Add(r, l, rh) => Instruction::Add(
                r.map_context::<C2>(),
                l.map_context::<C2>(),
                rh.map_context::<C2>(),
            ),
        }
    }
}

impl<CO: ContextTag, PO: ContextTag> ControlFlowInstruction<CO, PO> {
    // this should turn into a no-op lol
    pub fn map_context<CD: ContextTag, PD: ContextTag>(self) -> ControlFlowInstruction<CD, PD> {
        match self {
            ControlFlowInstruction::Jmp(BasicBlockJump(p, a)) => {
                ControlFlowInstruction::Jmp(BasicBlockJump(
                    p.map_context::<PD>(),
                    a.into_iter().map(|r| r.map_context::<CD>()).collect(),
                ))
            }
            ControlFlowInstruction::JmpIf {
                condition,
                true_path,
                false_path,
            } => ControlFlowInstruction::JmpIf {
                condition: condition.map_context::<CD>(),
                true_path: BasicBlockJump(
                    true_path.0.map_context::<PD>(),
                    true_path
                        .1
                        .into_iter()
                        .map(|r| r.map_context::<CD>())
                        .collect(),
                ),
                false_path: BasicBlockJump(
                    false_path.0.map_context::<PD>(),
                    false_path
                        .1
                        .into_iter()
                        .map(|r| r.map_context::<CD>())
                        .collect(),
                ),
            },
            ControlFlowInstruction::Ret(v) => {
                ControlFlowInstruction::Ret(v.map(|r| r.map_context::<CD>()))
            }
        }
    }
}

impl<C: Copy> Instruction<C> {
    pub fn assigned_to(&self) -> Option<RegisterId<C>> {
        match self {
            Instruction::Call(result, _, _) => *result,
            Instruction::GetRuntime(result)
            | Instruction::MakeString(result, _)
            | Instruction::MakeInteger(result, _)
            | Instruction::CompareLessThan(result, _, _)
            | Instruction::Add(result, _, _) => Some(*result),
        }
    }

    pub fn used_registers(&self) -> Vec<RegisterId<C>> {
        match self {
            Instruction::Call(_, _, params) => params.clone(),
            Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
                vec![*lhs, *rhs]
            }
            Instruction::GetRuntime(_)
            | Instruction::MakeString(_, _)
            | Instruction::MakeInteger(_, _) => Vec::new(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        match self {
            Instruction::Call(_, _, params) => params.iter_mut().collect(),
            Instruction::CompareLessThan(_, lhs, rhs) | Instruction::Add(_, lhs, rhs) => {
                vec![lhs, rhs]
            }
            Instruction::GetRuntime(_)
            | Instruction::MakeString(_, _)
            | Instruction::MakeInteger(_, _) => Vec::new(),
        }
    }
}

impl<C: Copy, P> ControlFlowInstruction<C, P> {
    pub fn used_registers(&self) -> Vec<RegisterId<C>> {
        match self {
            ControlFlowInstruction::Jmp(path) => path.1.clone(),
            ControlFlowInstruction::JmpIf {
                condition,
                true_path,
                false_path,
            } => {
                let mut r = vec![*condition];
                r.extend(true_path.1.clone());
                r.extend(false_path.1.clone());
                r
            }
            ControlFlowInstruction::Ret(Some(register)) => vec![*register],
            ControlFlowInstruction::Ret(None) => vec![],
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut handles = Vec::new();

        match self {
            ControlFlowInstruction::Jmp(path) => handles.extend(path.1.iter_mut()),
            ControlFlowInstruction::JmpIf {
                condition,
                true_path,
                false_path,
            } => {
                handles.push(condition);
                handles.extend(true_path.1.iter_mut());
                handles.extend(false_path.1.iter_mut());
            }
            ControlFlowInstruction::Ret(Some(register)) => handles.push(register),
            ControlFlowInstruction::Ret(None) => {}
        };

        handles
    }

    pub fn children(&self) -> Vec<&BasicBlockJump<C, P>> {
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

    pub fn children_mut(&mut self) -> Vec<&mut BasicBlockJump<C, P>> {
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
