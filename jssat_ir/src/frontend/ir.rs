use std::fmt::Display;
use std::hash::Hash;
use std::panic::Location;
use std::sync::Arc;

use rustc_hash::FxHashMap;
use tinyvec::TinyVec;

use crate::id::{IrCtx, Tag};
type BlockId = crate::id::BlockId<IrCtx>;
type FunctionId = crate::id::FunctionId<IrCtx>;
type ConstantId = crate::id::ConstantId<IrCtx>;
use crate::id::RegisterId;

use crate::isa::*;
use crate::retag::{BlkRetagger, CnstRetagger, ExtFnRetagger, FnRetagger, RegRetagger};

use super::source_map::SourceMapIdx;
type PlainRegisterId = RegisterId<IrCtx>;
type ExternalFunctionId = crate::id::ExternalFunctionId<IrCtx>;

#[derive(Debug, Clone)]
pub struct IR {
    pub dealer: Arc<AtomDealer>,
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
    pub name: Option<String>,
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
pub struct Instruction<C: Tag = crate::id::IrCtx, F: Tag = crate::id::IrCtx> {
    pub source_map_idx: Option<SourceMapIdx>,
    pub constructed_at: &'static Location<'static>,
    pub data: InstructionData<C, F>,
}

macro_rules! enum_bridge {
    ($variant:ident,$kind:ty) => {
        #[track_caller]
        #[allow(non_snake_case)]
        pub fn $variant(x: $kind) -> Self {
            Self::make(InstructionData::$variant(x))
        }
    };
}

impl<C: Tag, F: Tag> Instruction<C, F> {
    #[track_caller]
    fn make(data: InstructionData<C, F>) -> Self {
        Self {
            source_map_idx: None,
            constructed_at: std::panic::Location::caller(),
            data,
        }
    }

    enum_bridge!(Comment, Comment);
    enum_bridge!(NewRecord, NewRecord<C>);
    enum_bridge!(RecordGet, RecordGet<C>);
    enum_bridge!(RecordSet, RecordSet<C>);
    enum_bridge!(RecordHasKey, RecordHasKey<C>);
    enum_bridge!(NewList, NewList<C>);
    enum_bridge!(ListGet, ListGet<C>);
    enum_bridge!(ListSet, ListSet<C>);
    enum_bridge!(ListHasKey, ListHasKey<C>);
    enum_bridge!(ListLen, ListLen<C>);
    enum_bridge!(GetFnPtr, Make<C, crate::id::FunctionId<F>>);
    enum_bridge!(CallStatic, Call<C, crate::id::FunctionId<F>>);
    enum_bridge!(CallExtern, Call<C, crate::id::ExternalFunctionId<F>>);
    enum_bridge!(CallVirt, Call<C, crate::id::RegisterId<C>>);
    enum_bridge!(MakeAtom, Make<C, Atom>);
    enum_bridge!(MakeBytes, Make<C, crate::id::ConstantId<F>>);
    enum_bridge!(MakeInteger, Make<C, i64>);
    enum_bridge!(MakeBoolean, Make<C, bool>);
    enum_bridge!(BinOp, BinOp<C>);
    enum_bridge!(Negate, Negate<C>);
    enum_bridge!(Generalize, Generalize<C>);
    enum_bridge!(Assert, Assert<C>);
    enum_bridge!(IsType, IsType<C>);
    enum_bridge!(GetRuntime, GetRuntime<C>);
    enum_bridge!(Unreachable, Unreachable<C>);
}

#[derive(Debug, Clone)]
pub enum InstructionData<C: Tag = crate::id::IrCtx, F: Tag = crate::id::IrCtx> {
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
    MakeAtom(Make<C, Atom>),
    MakeBytes(Make<C, crate::id::ConstantId<F>>),
    MakeInteger(Make<C, i64>),
    MakeBoolean(Make<C, bool>),
    BinOp(BinOp<C>),
    Negate(Negate<C>),
    Generalize(Generalize<C>),
    Assert(Assert<C>),
    IsType(IsType<C>),
    GetRuntime(GetRuntime<C>),
    Unreachable(Unreachable<C>),
    /* RefIsEmpty(RegisterId /*=*/, RegisterId),
     * RefDeref(RegisterId /*=*/, RegisterId),
     * FAR FUTURE: GcMakeRegion(RegisterId /*=*/),
     * FAR FUTURE: GcEndRegion(RegisterId),
     * FAR FUTURE: GcTracingMarkRoot(RegisterId),
     * FAR FUTURE: GcTracingUnmarkRoot(RegisterId),
     * # `MakeString`
     *
     * Will instantiate a string, using the constant referenced as payload for
     * the value of the string. JS strings are UTF-16, so it is expected that
     * the constant referenced is a valid UTF-16 string.
     * TODO: the conv_bb_block phase doesn't mutate constants, so they're still
     * in the old constant phase. is this valid?
     * /// # [`Instruction::Unreachable`]
     * ///
     * /// Indicates that the executing code path will never reach this instruction.
     * /// It is undefined behavior for code to reach an Unreachable instruction.
     * /// This is used to implement functions that recurse an unknown amount of
     * /// times.
     * Unreachable, */
}

pub struct DisplayInst<'instruction, C: Tag, F: Tag>(&'instruction InstructionData<C, F>);
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
        Instruction {
            source_map_idx: self.source_map_idx,
            constructed_at: self.constructed_at,
            data: self
                .data
                .retag(retagger, ext_fn_retagger, fn_retagger, const_retagger),
        }
    }

    pub fn assigned_to(&self) -> Option<RegisterId<C>> {
        self.data.assigned_to()
    }

    pub fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        self.data.used_registers()
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        self.data.used_registers_mut()
    }

    pub fn as_display(&self) -> DisplayInst<C, F> {
        self.data.as_display()
    }

    pub fn display(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        self.data.display(w)
    }
}

impl<C: Tag, F: Tag> InstructionData<C, F> {
    // this should turn into a no-op lol
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        retagger: &mut impl RegRetagger<C, C2>,
        ext_fn_retagger: &impl ExtFnRetagger<F, F2>,
        fn_retagger: &impl FnRetagger<F, F2>,
        const_retagger: &impl CnstRetagger<F, F2>,
    ) -> InstructionData<C2, F2> {
        match self {
            InstructionData::Comment(c) => InstructionData::Comment(c.retag()),
            InstructionData::MakeBytes(inst) => {
                InstructionData::MakeBytes(inst.retag(retagger, const_retagger))
            }
            InstructionData::BinOp(inst) => InstructionData::BinOp(inst.retag(retagger)),
            InstructionData::NewRecord(inst) => InstructionData::NewRecord(inst.retag(retagger)),
            InstructionData::RecordGet(inst) => InstructionData::RecordGet(inst.retag(retagger)),
            InstructionData::RecordSet(inst) => InstructionData::RecordSet(inst.retag(retagger)),
            InstructionData::RecordHasKey(inst) => {
                InstructionData::RecordHasKey(inst.retag(retagger))
            }
            InstructionData::NewList(inst) => InstructionData::NewList(inst.retag(retagger)),
            InstructionData::ListGet(inst) => InstructionData::ListGet(inst.retag(retagger)),
            InstructionData::ListSet(inst) => InstructionData::ListSet(inst.retag(retagger)),
            InstructionData::ListHasKey(inst) => InstructionData::ListHasKey(inst.retag(retagger)),
            InstructionData::ListLen(inst) => InstructionData::ListLen(inst.retag(retagger)),
            InstructionData::GetFnPtr(inst) => {
                InstructionData::GetFnPtr(inst.retag(retagger, fn_retagger))
            }
            InstructionData::CallStatic(inst) => {
                InstructionData::CallStatic(inst.retag(retagger, fn_retagger))
            }
            InstructionData::CallExtern(inst) => {
                InstructionData::CallExtern(inst.retag(retagger, ext_fn_retagger))
            }
            InstructionData::CallVirt(inst) => InstructionData::CallVirt(inst.retag(retagger)),
            InstructionData::MakeAtom(inst) => InstructionData::MakeAtom(inst.retag(retagger)),
            InstructionData::MakeInteger(inst) => {
                InstructionData::MakeInteger(inst.retag(retagger))
            }
            InstructionData::MakeBoolean(inst) => {
                InstructionData::MakeBoolean(inst.retag(retagger))
            }
            InstructionData::Negate(inst) => InstructionData::Negate(inst.retag(retagger)),
            InstructionData::Generalize(inst) => InstructionData::Generalize(inst.retag(retagger)),
            InstructionData::Assert(inst) => InstructionData::Assert(inst.retag(retagger)),
            InstructionData::IsType(inst) => InstructionData::IsType(inst.retag(retagger)),
            InstructionData::GetRuntime(inst) => InstructionData::GetRuntime(inst.retag(retagger)),
            InstructionData::Unreachable(inst) => {
                InstructionData::Unreachable(inst.retag(retagger))
            }
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

impl<C: Tag, F: Tag> InstructionData<C, F> {
    pub fn assigned_to(&self) -> Option<RegisterId<C>> {
        match self {
            InstructionData::Comment(inst) => inst.declared_register(),
            InstructionData::GetFnPtr(inst) => inst.declared_register(),
            InstructionData::MakeBytes(inst) => inst.declared_register(),
            InstructionData::NewRecord(isa) => isa.declared_register(),
            InstructionData::BinOp(inst) => inst.declared_register(),
            InstructionData::CallStatic(inst) => inst.declared_register(),
            InstructionData::CallExtern(inst) => inst.declared_register(),
            InstructionData::CallVirt(inst) => inst.declared_register(),
            InstructionData::MakeAtom(inst) => inst.declared_register(),
            InstructionData::RecordGet(inst) => inst.declared_register(),
            InstructionData::RecordSet(inst) => inst.declared_register(),
            InstructionData::RecordHasKey(inst) => inst.declared_register(),
            InstructionData::MakeInteger(inst) => inst.declared_register(),
            InstructionData::MakeBoolean(inst) => inst.declared_register(),
            InstructionData::Negate(inst) => inst.declared_register(),
            InstructionData::Generalize(inst) => inst.declared_register(),
            InstructionData::Assert(inst) => inst.declared_register(),
            InstructionData::IsType(inst) => inst.declared_register(),
            InstructionData::NewList(inst) => inst.declared_register(),
            InstructionData::ListGet(inst) => inst.declared_register(),
            InstructionData::ListSet(inst) => inst.declared_register(),
            InstructionData::ListHasKey(inst) => inst.declared_register(),
            InstructionData::ListLen(inst) => inst.declared_register(),
            InstructionData::GetRuntime(inst) => inst.declared_register(),
            InstructionData::Unreachable(inst) => inst.declared_register(),
        }
    }

    pub fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        match self {
            InstructionData::Comment(inst) => inst.used_registers(),
            InstructionData::GetFnPtr(inst) => inst.used_registers(),
            InstructionData::MakeBytes(inst) => inst.used_registers(),
            InstructionData::NewRecord(inst) => inst.used_registers(),
            InstructionData::BinOp(inst) => inst.used_registers(),
            InstructionData::CallStatic(inst) => inst.used_registers(),
            InstructionData::CallExtern(inst) => inst.used_registers(),
            InstructionData::CallVirt(inst) => inst.used_registers(),
            InstructionData::MakeAtom(inst) => inst.used_registers(),
            InstructionData::RecordGet(inst) => inst.used_registers(),
            InstructionData::RecordSet(inst) => inst.used_registers(),
            InstructionData::RecordHasKey(inst) => inst.used_registers(),
            InstructionData::MakeInteger(inst) => inst.used_registers(),
            InstructionData::MakeBoolean(inst) => inst.used_registers(),
            InstructionData::Negate(inst) => inst.used_registers(),
            InstructionData::Generalize(inst) => inst.used_registers(),
            InstructionData::Assert(inst) => inst.used_registers(),
            InstructionData::IsType(inst) => inst.used_registers(),
            InstructionData::NewList(inst) => inst.used_registers(),
            InstructionData::ListGet(inst) => inst.used_registers(),
            InstructionData::ListSet(inst) => inst.used_registers(),
            InstructionData::ListHasKey(inst) => inst.used_registers(),
            InstructionData::ListLen(inst) => inst.used_registers(),
            InstructionData::GetRuntime(inst) => inst.used_registers(),
            InstructionData::Unreachable(inst) => inst.used_registers(),
        }
    }

    pub fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        match self {
            InstructionData::Comment(inst) => inst.used_registers_mut(),
            InstructionData::GetFnPtr(inst) => inst.used_registers_mut(),
            InstructionData::MakeBytes(inst) => inst.used_registers_mut(),
            InstructionData::NewRecord(inst) => inst.used_registers_mut(),
            InstructionData::BinOp(inst) => inst.used_registers_mut(),
            InstructionData::CallStatic(inst) => inst.used_registers_mut(),
            InstructionData::CallExtern(inst) => inst.used_registers_mut(),
            InstructionData::CallVirt(inst) => inst.used_registers_mut(),
            InstructionData::MakeAtom(inst) => inst.used_registers_mut(),
            InstructionData::RecordGet(inst) => inst.used_registers_mut(),
            InstructionData::RecordSet(inst) => inst.used_registers_mut(),
            InstructionData::RecordHasKey(inst) => inst.used_registers_mut(),
            InstructionData::MakeInteger(inst) => inst.used_registers_mut(),
            InstructionData::MakeBoolean(inst) => inst.used_registers_mut(),
            InstructionData::Negate(inst) => inst.used_registers_mut(),
            InstructionData::Generalize(inst) => inst.used_registers_mut(),
            InstructionData::Assert(inst) => inst.used_registers_mut(),
            InstructionData::IsType(inst) => inst.used_registers_mut(),
            InstructionData::NewList(inst) => inst.used_registers_mut(),
            InstructionData::ListGet(inst) => inst.used_registers_mut(),
            InstructionData::ListSet(inst) => inst.used_registers_mut(),
            InstructionData::ListHasKey(inst) => inst.used_registers_mut(),
            InstructionData::ListLen(inst) => inst.used_registers_mut(),
            InstructionData::GetRuntime(inst) => inst.used_registers_mut(),
            InstructionData::Unreachable(inst) => inst.used_registers_mut(),
        }
    }

    pub fn as_display(&self) -> DisplayInst<C, F> {
        DisplayInst(self)
    }

    pub fn display(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            InstructionData::Comment(inst) => {
                ISAInstruction::<crate::id::NoContext>::display(inst, w)
            }
            InstructionData::GetFnPtr(inst) => inst.display(w),
            InstructionData::MakeBytes(inst) => inst.display(w),
            InstructionData::NewRecord(inst) => inst.display(w),
            InstructionData::BinOp(inst) => inst.display(w),
            InstructionData::CallStatic(inst) => inst.display(w),
            InstructionData::CallExtern(inst) => inst.display(w),
            InstructionData::CallVirt(inst) => inst.display(w),
            InstructionData::MakeAtom(inst) => inst.display(w),
            InstructionData::RecordGet(inst) => inst.display(w),
            InstructionData::RecordSet(inst) => inst.display(w),
            InstructionData::RecordHasKey(inst) => inst.display(w),
            InstructionData::MakeInteger(inst) => inst.display(w),
            InstructionData::MakeBoolean(inst) => inst.display(w),
            InstructionData::Negate(inst) => inst.display(w),
            InstructionData::Generalize(inst) => inst.display(w),
            InstructionData::Assert(inst) => inst.display(w),
            InstructionData::IsType(inst) => inst.display(w),
            InstructionData::NewList(inst) => inst.display(w),
            InstructionData::ListGet(inst) => inst.display(w),
            InstructionData::ListSet(inst) => inst.display(w),
            InstructionData::ListHasKey(inst) => inst.display(w),
            InstructionData::ListLen(inst) => inst.display(w),
            InstructionData::GetRuntime(inst) => inst.display(w),
            InstructionData::Unreachable(inst) => inst.display(w),
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
