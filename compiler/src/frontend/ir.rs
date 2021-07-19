use std::hash::Hash;

use rustc_hash::FxHashMap;

use crate::name::DebugName;

use crate::id::{IrCtx, Tag};
type BlockId = crate::id::BlockId<IrCtx>;
type FunctionId = crate::id::FunctionId<IrCtx>;
type ConstantId = crate::id::ConstantId<IrCtx>;
use crate::id::RegisterId;

use super::isa::{
    BlockJump, CallExtern, CallStatic, CallVirt, ISAInstruction, Jump, JumpIf, MakeInteger,
    MakeRecord, MakeTrivial, OpAdd, OpEquals, OpLessThan, OpNegate, RecordGet, RecordSet, Return,
};
use super::retag::{BlkRetagger, ExtFnRetagger, FnRetagger, RegRetagger};
type PlainRegisterId = RegisterId<IrCtx>;
type ExternalFunctionId = crate::id::ExternalFunctionId<IrCtx>;

#[derive(Debug)]
pub struct IR {
    pub entrypoint: FunctionId,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub functions: FxHashMap<FunctionId, Function>,
}

impl IR {
    pub fn entry_block(&self) -> BlockId {
        self.functions.get(&self.entrypoint).unwrap().entry_block
    }
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Returns<T> {
    Value(T),
    Void,
}

pub trait Unifyable: Clone {
    fn unify(&self, other: &Self) -> Self;
}

impl<T: Unifyable> Unifyable for Returns<T> {
    fn unify(&self, other: &Self) -> Self {
        match (self, other) {
            (Returns::Value(lhs), Returns::Value(rhs)) => Returns::Value(lhs.unify(rhs)),
            (Returns::Value(v), Returns::Void) | (Returns::Void, Returns::Value(v)) => {
                Returns::Value(v.clone())
            }
            (Returns::Void, Returns::Void) => Returns::Void,
        }
    }
}

pub type FFIReturnType = Returns<FFIValueType>;

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
pub enum Instruction<C: Tag = crate::id::IrCtx, F: Tag = crate::id::IrCtx> {
    RecordNew(MakeRecord<C>),
    RecordGet(RecordGet<C>),
    RecordSet(RecordSet<C>),
    ReferenceOfFunction(RegisterId<C>, crate::id::FunctionId<F>),
    CallStatic(CallStatic<C, F>),
    CallExtern(CallExtern<C, F>),
    CallVirt(CallVirt<C>),
    // RefIsEmpty(RegisterId /*=*/, RegisterId),
    // RefDeref(RegisterId /*=*/, RegisterId),
    // FAR FUTURE: GcMakeRegion(RegisterId /*=*/),
    // FAR FUTURE: GcEndRegion(RegisterId),
    // FAR FUTURE: GcTracingMarkRoot(RegisterId),
    // FAR FUTURE: GcTracingUnmarkRoot(RegisterId),
    MakeTrivial(MakeTrivial<C>),
    /// # `MakeString`
    ///
    /// Will instantiate a string, using the constant referenced as payload for
    /// the value of the string. JS strings are UTF-16, so it is expected that
    /// the constant referenced is a valid UTF-16 string.
    // TODO: the conv_bb_block phase doesn't mutate constants, so they're still
    // in the old constant phase. is this valid?
    MakeString(RegisterId<C>, crate::id::ConstantId<F>),
    // /// # [`Instruction::Unreachable`]
    // ///
    // /// Indicates that the executing code path will never reach this instruction.
    // /// It is undefined behavior for code to reach an Unreachable instruction.
    // /// This is used to implement functions that recurse an unknown amount of
    // /// times.
    // Unreachable,
    MakeInteger(MakeInteger<C>),
    CompareLessThan(OpLessThan<C>),
    CompareEqual(OpEquals<C>),
    Negate(OpNegate<C>),
    Add(OpAdd<C>),
}

#[derive(Debug, Clone)]
pub enum ControlFlowInstruction<Ctx: Tag = IrCtx, Path: Tag = IrCtx> {
    Jmp(Jump<Path, Ctx>),
    JmpIf(JumpIf<Path, Ctx>),
    Ret(Return<Ctx>),
}

impl<C: Tag, F: Tag> Instruction<C, F> {
    // this should turn into a no-op lol
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        retagger: &mut impl RegRetagger<C, C2>,
        ext_fn_retagger: &impl ExtFnRetagger<F, F2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> Instruction<C2, F2> {
        match self {
            Instruction::MakeString(r, s) => {
                Instruction::MakeString(retagger.retag_new(r), s.map_context())
            }
            Instruction::CompareLessThan(inst) => {
                Instruction::CompareLessThan(inst.retag(retagger))
            }
            Instruction::RecordNew(inst) => Instruction::RecordNew(inst.retag(retagger)),
            Instruction::RecordGet(inst) => Instruction::RecordGet(inst.retag(retagger)),
            Instruction::RecordSet(inst) => Instruction::RecordSet(inst.retag(retagger)),
            Instruction::ReferenceOfFunction(r, f) => {
                Instruction::ReferenceOfFunction(retagger.retag_new(r), fn_retagger.retag_old(f))
            }
            Instruction::CallStatic(inst) => {
                Instruction::CallStatic(inst.retag(retagger, fn_retagger))
            }
            Instruction::CallExtern(inst) => {
                Instruction::CallExtern(inst.retag(retagger, ext_fn_retagger))
            }
            Instruction::CallVirt(inst) => Instruction::CallVirt(inst.retag(retagger)),
            Instruction::MakeTrivial(inst) => Instruction::MakeTrivial(inst.retag(retagger)),
            Instruction::MakeInteger(inst) => Instruction::MakeInteger(inst.retag(retagger)),
            Instruction::CompareEqual(inst) => Instruction::CompareEqual(inst.retag(retagger)),
            Instruction::Negate(inst) => Instruction::Negate(inst.retag(retagger)),
            Instruction::Add(inst) => Instruction::Add(inst.retag(retagger)),
        }
    }
}

impl<CO: Tag, PO: Tag> ControlFlowInstruction<CO, PO> {
    // this should turn into a no-op lol
    #[track_caller]
    pub fn retag<CD: Tag, PD: Tag>(
        self,
        retagger: &impl RegRetagger<CO, CD>,
        blk_retagger: &impl BlkRetagger<PO, PD>,
    ) -> ControlFlowInstruction<CD, PD> {
        match self {
            ControlFlowInstruction::Jmp(inst) => {
                ControlFlowInstruction::Jmp(inst.retag(retagger, blk_retagger))
            }
            ControlFlowInstruction::JmpIf(inst) => {
                ControlFlowInstruction::JmpIf(inst.retag(retagger, blk_retagger))
            }
            ControlFlowInstruction::Ret(inst) => ControlFlowInstruction::Ret(inst.retag(retagger)),
        }
    }
}

impl<C: Tag> Instruction<C> {
    pub fn assigned_to(&self) -> Option<RegisterId<C>> {
        match self {
            Instruction::MakeString(result, _) | Instruction::ReferenceOfFunction(result, _) => {
                Some(*result)
            }
            Instruction::RecordNew(isa) => isa.declared_register(),
            Instruction::CompareLessThan(inst) => inst.declared_register(),
            Instruction::CallStatic(inst) => inst.declared_register(),
            Instruction::CallExtern(inst) => inst.declared_register(),
            Instruction::CallVirt(inst) => inst.declared_register(),
            Instruction::MakeTrivial(inst) => inst.declared_register(),
            Instruction::RecordGet(inst) => inst.declared_register(),
            Instruction::RecordSet(inst) => inst.declared_register(),
            Instruction::MakeInteger(inst) => inst.declared_register(),
            Instruction::CompareEqual(inst) => inst.declared_register(),
            Instruction::Negate(inst) => inst.declared_register(),
            Instruction::Add(inst) => inst.declared_register(),
        }
    }

    pub fn used_registers(&self) -> Vec<RegisterId<C>> {
        match self {
            Instruction::MakeString(_, _) | Instruction::ReferenceOfFunction(_, _) => Vec::new(),
            Instruction::RecordNew(inst) => inst.used_registers().to_vec(),
            Instruction::CompareLessThan(inst) => inst.used_registers().to_vec(),
            Instruction::CallStatic(inst) => inst.used_registers().to_vec(),
            Instruction::CallExtern(inst) => inst.used_registers().to_vec(),
            Instruction::CallVirt(inst) => inst.used_registers().to_vec(),
            Instruction::MakeTrivial(inst) => inst.used_registers().to_vec(),
            Instruction::RecordGet(inst) => inst.used_registers().to_vec(),
            Instruction::RecordSet(inst) => inst.used_registers().to_vec(),
            Instruction::MakeInteger(inst) => inst.used_registers().to_vec(),
            Instruction::CompareEqual(inst) => inst.used_registers().to_vec(),
            Instruction::Negate(inst) => inst.used_registers().to_vec(),
            Instruction::Add(inst) => inst.used_registers().to_vec(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        match self {
            Instruction::MakeString(_, _) | Instruction::ReferenceOfFunction(_, _) => Vec::new(),
            Instruction::RecordNew(inst) => inst.used_registers_mut(),
            Instruction::CompareLessThan(inst) => inst.used_registers_mut(),
            Instruction::CallStatic(inst) => inst.used_registers_mut(),
            Instruction::CallExtern(inst) => inst.used_registers_mut(),
            Instruction::CallVirt(inst) => inst.used_registers_mut(),
            Instruction::MakeTrivial(inst) => inst.used_registers_mut(),
            Instruction::RecordGet(inst) => inst.used_registers_mut(),
            Instruction::RecordSet(inst) => inst.used_registers_mut(),
            Instruction::MakeInteger(inst) => inst.used_registers_mut(),
            Instruction::CompareEqual(inst) => inst.used_registers_mut(),
            Instruction::Negate(inst) => inst.used_registers_mut(),
            Instruction::Add(inst) => inst.used_registers_mut(),
        }
    }
}

impl<C: Tag, P: Tag> ControlFlowInstruction<C, P> {
    pub fn used_registers(&self) -> Vec<RegisterId<C>> {
        match self {
            ControlFlowInstruction::Jmp(inst) => inst.used_registers().to_vec(),
            ControlFlowInstruction::JmpIf(inst) => inst.used_registers().to_vec(),
            ControlFlowInstruction::Ret(inst) => inst.used_registers().to_vec(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        match self {
            ControlFlowInstruction::Jmp(inst) => inst.used_registers_mut(),
            ControlFlowInstruction::JmpIf(inst) => inst.used_registers_mut(),
            ControlFlowInstruction::Ret(inst) => inst.used_registers_mut(),
        }
    }

    pub fn children(&self) -> Vec<&BlockJump<P, C>> {
        match self {
            ControlFlowInstruction::Jmp(inst) => inst.paths(),
            ControlFlowInstruction::JmpIf(inst) => inst.paths(),
            ControlFlowInstruction::Ret(_) => Vec::new(),
        }
    }

    pub fn children_mut(&mut self) -> Vec<&mut BlockJump<P, C>> {
        match self {
            ControlFlowInstruction::Jmp(inst) => inst.paths_mut(),
            ControlFlowInstruction::JmpIf(inst) => inst.paths_mut(),
            ControlFlowInstruction::Ret(_) => Vec::new(),
        }
    }
}
