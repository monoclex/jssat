use std::fmt::Display;
use std::hash::Hash;

use rustc_hash::FxHashMap;
use tinyvec::TinyVec;

use crate::id::{IrCtx, Tag};
type BlockId = crate::id::BlockId<IrCtx>;
type FunctionId = crate::id::FunctionId<IrCtx>;
type ConstantId = crate::id::ConstantId<IrCtx>;
use crate::id::RegisterId;

use crate::isa::*;
use crate::retag::{BlkRetagger, CnstRetagger, ExtFnRetagger, FnRetagger, RegRetagger};
type PlainRegisterId = RegisterId<IrCtx>;
type ExternalFunctionId = crate::id::ExternalFunctionId<IrCtx>;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Constant {
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

pub type FFIReturnType = Returns<FFIValueType>;

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Parameter>,
    // require that the entry block has 0 parameters
    pub entry_block: BlockId,
    pub blocks: FxHashMap<BlockId, FunctionBlock>,
    /* pub control_flow: ControlFlowGraph,
     * pub register_flow: ValueFlowGraph, */
}

#[derive(Debug, Clone)]
pub struct Parameter {
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
    Comment(Comment),
    NewRecord(NewRecord<C>),
    RecordGet(RecordGet<C>),
    RecordSet(RecordSet<C>),
    RecordHasKey(RecordHasKey<C>),
    NewList(NewList<C>),
    ListGet(ListGet<C>),
    ListSet(ListSet<C>),
    ListHasKey(ListHasKey<C>),
    ListLen(ListLen<C>),
    GetFnPtr(Make<C, crate::id::FunctionId<F>>),
    CallStatic(Call<C, crate::id::FunctionId<F>>),
    CallExtern(Call<C, crate::id::ExternalFunctionId<F>>),
    CallVirt(Call<C, crate::id::RegisterId<C>>),
    // RefIsEmpty(RegisterId /*=*/, RegisterId),
    // RefDeref(RegisterId /*=*/, RegisterId),
    // FAR FUTURE: GcMakeRegion(RegisterId /*=*/),
    // FAR FUTURE: GcEndRegion(RegisterId),
    // FAR FUTURE: GcTracingMarkRoot(RegisterId),
    // FAR FUTURE: GcTracingUnmarkRoot(RegisterId),
    MakeTrivial(Make<C, Atom>),
    /// # `MakeString`
    ///
    /// Will instantiate a string, using the constant referenced as payload for
    /// the value of the string. JS strings are UTF-16, so it is expected that
    /// the constant referenced is a valid UTF-16 string.
    // TODO: the conv_bb_block phase doesn't mutate constants, so they're still
    // in the old constant phase. is this valid?
    MakeBytes(Make<C, crate::id::ConstantId<F>>),
    // /// # [`Instruction::Unreachable`]
    // ///
    // /// Indicates that the executing code path will never reach this instruction.
    // /// It is undefined behavior for code to reach an Unreachable instruction.
    // /// This is used to implement functions that recurse an unknown amount of
    // /// times.
    // Unreachable,
    // TODO: should these be TrivialItems?
    MakeInteger(Make<C, i64>),
    MakeBoolean(Make<C, bool>),
    BinOp(BinOp<C>),
    Negate(Negate<C>),
    Generalize(Generalize<C>),
    Assert(Assert<C>),
    IsType(IsType<C>),
    GetRuntime(GetRuntime<C>),
    Unreachable(Unreachable<C>),
}

pub struct DisplayInst<'instruction, C: Tag, F: Tag>(&'instruction Instruction<C, F>);
impl<'i, C: Tag, F: Tag> Display for DisplayInst<'i, C, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display(f)
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlowInstruction<Ctx: Tag = IrCtx, Path: Tag = IrCtx> {
    Jmp(Jump<crate::id::BlockId<Path>, Ctx>),
    JmpIf(JumpIf<crate::id::BlockId<Path>, Ctx>),
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
        const_retagger: &impl CnstRetagger<F, F2>,
    ) -> Instruction<C2, F2> {
        match self {
            Instruction::Comment(c) => Instruction::Comment(c.retag()),
            Instruction::MakeBytes(inst) => {
                Instruction::MakeBytes(inst.retag(retagger, const_retagger))
            }
            Instruction::BinOp(inst) => Instruction::BinOp(inst.retag(retagger)),
            Instruction::NewRecord(inst) => Instruction::NewRecord(inst.retag(retagger)),
            Instruction::RecordGet(inst) => Instruction::RecordGet(inst.retag(retagger)),
            Instruction::RecordSet(inst) => Instruction::RecordSet(inst.retag(retagger)),
            Instruction::RecordHasKey(inst) => Instruction::RecordHasKey(inst.retag(retagger)),
            Instruction::NewList(inst) => Instruction::NewList(inst.retag(retagger)),
            Instruction::ListGet(inst) => Instruction::ListGet(inst.retag(retagger)),
            Instruction::ListSet(inst) => Instruction::ListSet(inst.retag(retagger)),
            Instruction::ListHasKey(inst) => Instruction::ListHasKey(inst.retag(retagger)),
            Instruction::ListLen(inst) => Instruction::ListLen(inst.retag(retagger)),
            Instruction::GetFnPtr(inst) => Instruction::GetFnPtr(inst.retag(retagger, fn_retagger)),
            Instruction::CallStatic(inst) => {
                Instruction::CallStatic(inst.retag(retagger, fn_retagger))
            }
            Instruction::CallExtern(inst) => {
                Instruction::CallExtern(inst.retag(retagger, ext_fn_retagger))
            }
            Instruction::CallVirt(inst) => Instruction::CallVirt(inst.retag(retagger)),
            Instruction::MakeTrivial(inst) => Instruction::MakeTrivial(inst.retag(retagger)),
            Instruction::MakeInteger(inst) => Instruction::MakeInteger(inst.retag(retagger)),
            Instruction::MakeBoolean(inst) => Instruction::MakeBoolean(inst.retag(retagger)),
            Instruction::Negate(inst) => Instruction::Negate(inst.retag(retagger)),
            Instruction::Generalize(inst) => Instruction::Generalize(inst.retag(retagger)),
            Instruction::Assert(inst) => Instruction::Assert(inst.retag(retagger)),
            Instruction::IsType(inst) => Instruction::IsType(inst.retag(retagger)),
            Instruction::GetRuntime(inst) => Instruction::GetRuntime(inst.retag(retagger)),
            Instruction::Unreachable(inst) => Instruction::Unreachable(inst.retag(retagger)),
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

impl<C: Tag, F: Tag> Instruction<C, F> {
    pub fn assigned_to(&self) -> Option<RegisterId<C>> {
        match self {
            Instruction::Comment(inst) => inst.declared_register(),
            Instruction::GetFnPtr(inst) => inst.declared_register(),
            Instruction::MakeBytes(inst) => inst.declared_register(),
            Instruction::NewRecord(isa) => isa.declared_register(),
            Instruction::BinOp(inst) => inst.declared_register(),
            Instruction::CallStatic(inst) => inst.declared_register(),
            Instruction::CallExtern(inst) => inst.declared_register(),
            Instruction::CallVirt(inst) => inst.declared_register(),
            Instruction::MakeTrivial(inst) => inst.declared_register(),
            Instruction::RecordGet(inst) => inst.declared_register(),
            Instruction::RecordSet(inst) => inst.declared_register(),
            Instruction::RecordHasKey(inst) => inst.declared_register(),
            Instruction::MakeInteger(inst) => inst.declared_register(),
            Instruction::MakeBoolean(inst) => inst.declared_register(),
            Instruction::Negate(inst) => inst.declared_register(),
            Instruction::Generalize(inst) => inst.declared_register(),
            Instruction::Assert(inst) => inst.declared_register(),
            Instruction::IsType(inst) => inst.declared_register(),
            Instruction::NewList(inst) => inst.declared_register(),
            Instruction::ListGet(inst) => inst.declared_register(),
            Instruction::ListSet(inst) => inst.declared_register(),
            Instruction::ListHasKey(inst) => inst.declared_register(),
            Instruction::ListLen(inst) => inst.declared_register(),
            Instruction::GetRuntime(inst) => inst.declared_register(),
            Instruction::Unreachable(inst) => inst.declared_register(),
        }
    }

    pub fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        match self {
            Instruction::Comment(inst) => inst.used_registers(),
            Instruction::GetFnPtr(inst) => inst.used_registers(),
            Instruction::MakeBytes(inst) => inst.used_registers(),
            Instruction::NewRecord(inst) => inst.used_registers(),
            Instruction::BinOp(inst) => inst.used_registers(),
            Instruction::CallStatic(inst) => inst.used_registers(),
            Instruction::CallExtern(inst) => inst.used_registers(),
            Instruction::CallVirt(inst) => inst.used_registers(),
            Instruction::MakeTrivial(inst) => inst.used_registers(),
            Instruction::RecordGet(inst) => inst.used_registers(),
            Instruction::RecordSet(inst) => inst.used_registers(),
            Instruction::RecordHasKey(inst) => inst.used_registers(),
            Instruction::MakeInteger(inst) => inst.used_registers(),
            Instruction::MakeBoolean(inst) => inst.used_registers(),
            Instruction::Negate(inst) => inst.used_registers(),
            Instruction::Generalize(inst) => inst.used_registers(),
            Instruction::Assert(inst) => inst.used_registers(),
            Instruction::IsType(inst) => inst.used_registers(),
            Instruction::NewList(inst) => inst.used_registers(),
            Instruction::ListGet(inst) => inst.used_registers(),
            Instruction::ListSet(inst) => inst.used_registers(),
            Instruction::ListHasKey(inst) => inst.used_registers(),
            Instruction::ListLen(inst) => inst.used_registers(),
            Instruction::GetRuntime(inst) => inst.used_registers(),
            Instruction::Unreachable(inst) => inst.used_registers(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        match self {
            Instruction::Comment(inst) => inst.used_registers_mut(),
            Instruction::GetFnPtr(inst) => inst.used_registers_mut(),
            Instruction::MakeBytes(inst) => inst.used_registers_mut(),
            Instruction::NewRecord(inst) => inst.used_registers_mut(),
            Instruction::BinOp(inst) => inst.used_registers_mut(),
            Instruction::CallStatic(inst) => inst.used_registers_mut(),
            Instruction::CallExtern(inst) => inst.used_registers_mut(),
            Instruction::CallVirt(inst) => inst.used_registers_mut(),
            Instruction::MakeTrivial(inst) => inst.used_registers_mut(),
            Instruction::RecordGet(inst) => inst.used_registers_mut(),
            Instruction::RecordSet(inst) => inst.used_registers_mut(),
            Instruction::RecordHasKey(inst) => inst.used_registers_mut(),
            Instruction::MakeInteger(inst) => inst.used_registers_mut(),
            Instruction::MakeBoolean(inst) => inst.used_registers_mut(),
            Instruction::Negate(inst) => inst.used_registers_mut(),
            Instruction::Generalize(inst) => inst.used_registers_mut(),
            Instruction::Assert(inst) => inst.used_registers_mut(),
            Instruction::IsType(inst) => inst.used_registers_mut(),
            Instruction::NewList(inst) => inst.used_registers_mut(),
            Instruction::ListGet(inst) => inst.used_registers_mut(),
            Instruction::ListSet(inst) => inst.used_registers_mut(),
            Instruction::ListHasKey(inst) => inst.used_registers_mut(),
            Instruction::ListLen(inst) => inst.used_registers_mut(),
            Instruction::GetRuntime(inst) => inst.used_registers_mut(),
            Instruction::Unreachable(inst) => inst.used_registers_mut(),
        }
    }

    pub fn as_display(&self) -> DisplayInst<C, F> {
        DisplayInst(self)
    }

    pub fn display(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Instruction::Comment(inst) => ISAInstruction::<crate::id::NoContext>::display(inst, w),
            Instruction::GetFnPtr(inst) => inst.display(w),
            Instruction::MakeBytes(inst) => inst.display(w),
            Instruction::NewRecord(inst) => inst.display(w),
            Instruction::BinOp(inst) => inst.display(w),
            Instruction::CallStatic(inst) => inst.display(w),
            Instruction::CallExtern(inst) => inst.display(w),
            Instruction::CallVirt(inst) => inst.display(w),
            Instruction::MakeTrivial(inst) => inst.display(w),
            Instruction::RecordGet(inst) => inst.display(w),
            Instruction::RecordSet(inst) => inst.display(w),
            Instruction::RecordHasKey(inst) => inst.display(w),
            Instruction::MakeInteger(inst) => inst.display(w),
            Instruction::MakeBoolean(inst) => inst.display(w),
            Instruction::Negate(inst) => inst.display(w),
            Instruction::Generalize(inst) => inst.display(w),
            Instruction::Assert(inst) => inst.display(w),
            Instruction::IsType(inst) => inst.display(w),
            Instruction::NewList(inst) => inst.display(w),
            Instruction::ListGet(inst) => inst.display(w),
            Instruction::ListSet(inst) => inst.display(w),
            Instruction::ListHasKey(inst) => inst.display(w),
            Instruction::ListLen(inst) => inst.display(w),
            Instruction::GetRuntime(inst) => inst.display(w),
            Instruction::Unreachable(inst) => inst.display(w),
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

    pub fn children(&self) -> Vec<&BlockJump<crate::id::BlockId<P>, C>> {
        match self {
            ControlFlowInstruction::Jmp(inst) => inst.paths(),
            ControlFlowInstruction::JmpIf(inst) => inst.paths(),
            ControlFlowInstruction::Ret(_) => Vec::new(),
        }
    }

    pub fn children_mut(&mut self) -> Vec<&mut BlockJump<crate::id::BlockId<P>, C>> {
        match self {
            ControlFlowInstruction::Jmp(inst) => inst.paths_mut(),
            ControlFlowInstruction::JmpIf(inst) => inst.paths_mut(),
            ControlFlowInstruction::Ret(_) => Vec::new(),
        }
    }
}
