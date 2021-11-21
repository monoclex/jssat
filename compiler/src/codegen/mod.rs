//! Backend code that abstract interpretation emits for processing.

#![allow(warnings)]

mod lower;
pub use lower::lower;

mod typed_program;
pub use typed_program::type_program;

mod display_typed;
pub use display_typed::display_typed;

mod display_program;
pub use display_program::display_program;

use std::fmt::Write;

use rustc_hash::FxHashMap;
use tinyvec::TinyVec;

use crate::frontend::ir::{Constant, ExternalFunction};
use crate::id::{
    AssemblerCtx, BlockId, FunctionId, IdCompat, LiftedCtx, LowerCtx, RegisterId, Tag,
};
use crate::isa::*;
use crate::symbolic_execution::types::TypeBag;
type ExternalFunctionId = crate::id::ExternalFunctionId<AssemblerCtx>;
type ConstantId = crate::id::ConstantId<AssemblerCtx>;

#[derive(Clone)]
pub struct Program<T: Tag = LowerCtx> {
    pub entrypoint: FunctionId<T>,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub functions: FxHashMap<FunctionId<T>, Function<T>>,
}

#[derive(Clone)]
pub struct Function<T: Tag> {
    pub entry: BlockId<T>,
    pub blocks: FxHashMap<BlockId<T>, Block<T, BlockId<T>>>,
}

#[derive(Clone)]
pub struct TypedProgram<T: Tag = AssemblerCtx> {
    pub entrypoint: FunctionId<T>,
    pub external_functions: FxHashMap<ExternalFunctionId, ExternalFunction>,
    pub constants: FxHashMap<ConstantId, Constant>,
    pub functions: FxHashMap<FunctionId<T>, Block<T>>,
}

#[derive(Clone)]
pub struct Block<T: Tag, B: IdCompat = FunctionId<T>> {
    pub parameters: Vec<RegisterId<T>>,
    pub instructions: Vec<Instruction<T>>,
    pub end: EndInstruction<T, B>,
    pub type_info: TypeBag,
}

#[derive(Clone)]
pub enum Instruction<T: Tag> {
    Noop(Noop),
    Comment(Comment),
    NewRecord(NewRecord<T>),
    RecordGet(RecordGet<T>),
    RecordSet(RecordSet<T>),
    RecordHasKey(RecordHasKey<T>),
    CallStatic(Call<T, FunctionId<T>>),
    CallExtern(Call<T, ExternalFunctionId>),
    CallVirt(Call<T, RegisterId<T>>),
    /// `GetFnPtr` has to have its argument be in a `LiftedCtx` as we cannot
    /// convert the function id into a statically known argument. Consider the
    /// following code:
    ///
    /// ```js
    /// function id(x) { return x; }
    ///
    /// let fnPtr = id;
    /// fnPtr(0);
    /// fnPtr("a");
    /// ```
    ///
    /// In the above code, `fnPtr` is a pointer to *one* function, but ends up
    /// calling *two* variants of the `id` function:
    ///
    /// - `id : Int -> Int`
    /// - `id : String -> String`
    ///
    /// Thus, it is better to think of function pointers as symbolic values.
    ///
    /// In the future, these may be compiled down to integers and `match`ed at
    /// the call site to call the correct function.
    GetFnPtr(Make<T, crate::id::FunctionId<LiftedCtx>>),
    MakeTrivial(Make<T, TrivialItem>),
    MakeBytes(Make<T, ConstantId>),
    MakeInteger(Make<T, i64>),
    MakeBoolean(Make<T, bool>),
    BinOp(BinOp<T>),
    Negate(Negate<T>),
}

impl<T: Tag> Instruction<T> {
    pub fn is_pure(&self) -> bool {
        match self {
            Instruction::Noop(_) => <Noop as ISAInstruction<T>>::is_pure(),
            Instruction::Comment(_) => <Comment as ISAInstruction<T>>::is_pure(),
            Instruction::NewRecord(_) => NewRecord::<T>::is_pure(),
            Instruction::RecordGet(_) => RecordGet::<T>::is_pure(),
            Instruction::RecordSet(_) => RecordSet::<T>::is_pure(),
            Instruction::RecordHasKey(_) => RecordHasKey::<T>::is_pure(),
            Instruction::CallStatic(_) => Call::<T, FunctionId<T>>::is_pure(),
            Instruction::CallExtern(_) => Call::<T, FunctionId<T>>::is_pure(),
            Instruction::CallVirt(_) => Call::<T, FunctionId<T>>::is_pure(),
            Instruction::GetFnPtr(_) => Make::<T, FunctionId<T>>::is_pure(),
            Instruction::MakeTrivial(_) => Make::<T, FunctionId<T>>::is_pure(),
            Instruction::MakeBytes(_) => Make::<T, FunctionId<T>>::is_pure(),
            Instruction::MakeInteger(_) => Make::<T, FunctionId<T>>::is_pure(),
            Instruction::MakeBoolean(_) => Make::<T, FunctionId<T>>::is_pure(),
            Instruction::BinOp(_) => BinOp::<T>::is_pure(),
            Instruction::Negate(_) => Negate::<T>::is_pure(),
        }
    }

    fn declared_register(&self) -> Option<RegisterId<T>> {
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

    fn used_registers(&self) -> TinyVec<[RegisterId<T>; 3]> {
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

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<T>> {
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
            Instruction::Noop(i) => <Noop as ISAInstruction<T>>::display(i, w),
            Instruction::Comment(i) => <Comment as ISAInstruction<T>>::display(i, w),
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
pub enum EndInstruction<T: Tag, B = BlockId<T>>
where
    B: crate::id::IdCompat,
{
    Unreachable(Unreachable<T>),
    Jump(Jump<B, T>),
    JumpIf(JumpIf<B, T>),
    Return(Return<T>),
}
