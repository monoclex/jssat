//! Backend code that abstract interpretation emits for processing.

mod lower;
pub use lower::lower;

mod typed_program;
pub use typed_program::type_program;

use std::fmt::Write;

use rustc_hash::FxHashMap;
use tinyvec::TinyVec;

use crate::frontend::ir::{Constant, ExternalFunction};
use crate::id::{AssemblerCtx, LiftedCtx};
use crate::isa::*;
use crate::symbolic_execution::types::TypeBag;
type RegisterId = crate::id::RegisterId<AssemblerCtx>;
type FunctionId = crate::id::FunctionId<AssemblerCtx>;
type BlockId = crate::id::BlockId<AssemblerCtx>;
type ExternalFunctionId = crate::id::ExternalFunctionId<AssemblerCtx>;
type ConstantId = crate::id::ConstantId<AssemblerCtx>;

#[derive(Clone)]
pub struct Program {
    pub entrypoint: FunctionId,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub functions: FxHashMap<FunctionId, Function>,
}

#[derive(Clone)]
pub struct Function {
    pub entry: BlockId,
    pub blocks: FxHashMap<BlockId, Block>,
}

#[derive(Clone)]
pub struct TypedProgram {
    pub entrypoint: FunctionId,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub functions: FxHashMap<FunctionId, Block<TypeBag>>,
}

#[derive(Clone)]
pub struct Block<T = ()> {
    pub parameters: Vec<RegisterId>,
    pub instructions: Vec<Instruction>,
    pub end: EndInstruction<FunctionId>,
    pub type_info: T,
}

#[derive(Clone)]
pub enum Instruction {
    Noop(Noop),
    Comment(Comment),
    NewRecord(NewRecord<AssemblerCtx>),
    RecordGet(RecordGet<AssemblerCtx>),
    RecordSet(RecordSet<AssemblerCtx>),
    RecordHasKey(RecordHasKey<AssemblerCtx>),
    CallStatic(Call<AssemblerCtx, FunctionId>),
    CallExtern(Call<AssemblerCtx, ExternalFunctionId>),
    CallVirt(Call<AssemblerCtx, RegisterId>),
    GetFnPtr(Make<AssemblerCtx, crate::id::FunctionId<LiftedCtx>>),
    MakeTrivial(Make<AssemblerCtx, TrivialItem>),
    MakeBytes(Make<AssemblerCtx, ConstantId>),
    MakeInteger(Make<AssemblerCtx, i64>),
    MakeBoolean(Make<AssemblerCtx, bool>),
    BinOp(BinOp<AssemblerCtx>),
    Negate(Negate<AssemblerCtx>),
}

impl Instruction {
    pub fn is_pure(&self) -> bool {
        match self {
            Instruction::Noop(_) => <Noop as ISAInstruction<AssemblerCtx>>::is_pure(),
            Instruction::Comment(_) => <Comment as ISAInstruction<AssemblerCtx>>::is_pure(),
            Instruction::NewRecord(_) => NewRecord::<AssemblerCtx>::is_pure(),
            Instruction::RecordGet(_) => RecordGet::<AssemblerCtx>::is_pure(),
            Instruction::RecordSet(_) => RecordSet::<AssemblerCtx>::is_pure(),
            Instruction::RecordHasKey(_) => RecordHasKey::<AssemblerCtx>::is_pure(),
            Instruction::CallStatic(_) => Call::<AssemblerCtx, FunctionId>::is_pure(),
            Instruction::CallExtern(_) => Call::<AssemblerCtx, ExternalFunctionId>::is_pure(),
            Instruction::CallVirt(_) => Call::<AssemblerCtx, RegisterId>::is_pure(),
            Instruction::GetFnPtr(_) => Make::<AssemblerCtx, FunctionId>::is_pure(),
            Instruction::MakeTrivial(_) => Make::<AssemblerCtx, TrivialItem>::is_pure(),
            Instruction::MakeBytes(_) => Make::<AssemblerCtx, ConstantId>::is_pure(),
            Instruction::MakeInteger(_) => Make::<AssemblerCtx, i64>::is_pure(),
            Instruction::MakeBoolean(_) => Make::<AssemblerCtx, bool>::is_pure(),
            Instruction::BinOp(_) => BinOp::<AssemblerCtx>::is_pure(),
            Instruction::Negate(_) => Negate::<AssemblerCtx>::is_pure(),
        }
    }

    fn declared_register(&self) -> Option<RegisterId> {
        match self {
            Instruction::Noop(i) => i.declared_register(),
            Instruction::Comment(i) => i.declared_register(),
            Instruction::NewRecord(i) => i.declared_register(),
            Instruction::RecordGet(i) => i.declared_register(),
            Instruction::RecordSet(i) => i.declared_register(),
            Instruction::RecordHasKey(i) => i.declared_register(),
            Instruction::CallStatic(i) => i.declared_register(),
            Instruction::CallExtern(i) => i.declared_register(),
            Instruction::CallVirt(i) => i.declared_register(),
            Instruction::GetFnPtr(i) => i.declared_register(),
            Instruction::MakeTrivial(i) => i.declared_register(),
            Instruction::MakeBytes(i) => i.declared_register(),
            Instruction::MakeInteger(i) => i.declared_register(),
            Instruction::MakeBoolean(i) => i.declared_register(),
            Instruction::BinOp(i) => i.declared_register(),
            Instruction::Negate(i) => i.declared_register(),
        }
    }

    fn used_registers(&self) -> TinyVec<[RegisterId; 3]> {
        match self {
            Instruction::Noop(i) => i.used_registers(),
            Instruction::Comment(i) => i.used_registers(),
            Instruction::NewRecord(i) => i.used_registers(),
            Instruction::RecordGet(i) => i.used_registers(),
            Instruction::RecordSet(i) => i.used_registers(),
            Instruction::RecordHasKey(i) => i.used_registers(),
            Instruction::CallStatic(i) => i.used_registers(),
            Instruction::CallExtern(i) => i.used_registers(),
            Instruction::CallVirt(i) => i.used_registers(),
            Instruction::GetFnPtr(i) => i.used_registers(),
            Instruction::MakeTrivial(i) => i.used_registers(),
            Instruction::MakeBytes(i) => i.used_registers(),
            Instruction::MakeInteger(i) => i.used_registers(),
            Instruction::MakeBoolean(i) => i.used_registers(),
            Instruction::BinOp(i) => i.used_registers(),
            Instruction::Negate(i) => i.used_registers(),
        }
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId> {
        match self {
            Instruction::Noop(i) => i.used_registers_mut(),
            Instruction::Comment(i) => i.used_registers_mut(),
            Instruction::NewRecord(i) => i.used_registers_mut(),
            Instruction::RecordGet(i) => i.used_registers_mut(),
            Instruction::RecordSet(i) => i.used_registers_mut(),
            Instruction::RecordHasKey(i) => i.used_registers_mut(),
            Instruction::CallStatic(i) => i.used_registers_mut(),
            Instruction::CallExtern(i) => i.used_registers_mut(),
            Instruction::CallVirt(i) => i.used_registers_mut(),
            Instruction::GetFnPtr(i) => i.used_registers_mut(),
            Instruction::MakeTrivial(i) => i.used_registers_mut(),
            Instruction::MakeBytes(i) => i.used_registers_mut(),
            Instruction::MakeInteger(i) => i.used_registers_mut(),
            Instruction::MakeBoolean(i) => i.used_registers_mut(),
            Instruction::BinOp(i) => i.used_registers_mut(),
            Instruction::Negate(i) => i.used_registers_mut(),
        }
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        match self {
            Instruction::Noop(i) => <Noop as ISAInstruction<AssemblerCtx>>::display(i, w),
            Instruction::Comment(i) => <Comment as ISAInstruction<AssemblerCtx>>::display(i, w),
            Instruction::NewRecord(i) => i.display(w),
            Instruction::RecordGet(i) => i.display(w),
            Instruction::RecordSet(i) => i.display(w),
            Instruction::RecordHasKey(i) => i.display(w),
            Instruction::CallStatic(i) => i.display(w),
            Instruction::CallExtern(i) => i.display(w),
            Instruction::CallVirt(i) => i.display(w),
            Instruction::GetFnPtr(i) => i.display(w),
            Instruction::MakeTrivial(i) => i.display(w),
            Instruction::MakeBytes(i) => i.display(w),
            Instruction::MakeInteger(i) => i.display(w),
            Instruction::MakeBoolean(i) => i.display(w),
            Instruction::BinOp(i) => i.display(w),
            Instruction::Negate(i) => i.display(w),
        }
    }
}

#[derive(Clone)]
pub enum EndInstruction<B = BlockId>
where
    B: crate::id::IdCompat,
{
    Unreachable(Unreachable),
    Jump(Jump<B, AssemblerCtx>),
    JumpIf(JumpIf<B, AssemblerCtx>),
    Return(Return<AssemblerCtx>),
}
