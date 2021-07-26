//! This crate defines a contract that all instructions must provide. It is used
//! to allow easy composability of instructions for other IR passes with their
//! own in-house ISAs.

use std::fmt::{Display, Write};

use derive_more::Display;
use tinyvec::{tiny_vec, TinyVec};

use crate::id::ConstantId;
use crate::id::ExternalFunctionId;
use crate::id::FunctionId;
use crate::id::RegisterId;
use crate::id::Tag;
use crate::id::{BlockId, IdCompat};

use crate::retag::{BlkRetagger, BlkToFn, CnstRetagger, ExtFnRetagger, FnRetagger, RegRetagger};

pub struct Registers<'a, R: Tag>(pub &'a Vec<RegisterId<R>>);

impl<R: Tag> Display for Registers<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();

        match iter.next() {
            None => return Ok(()),
            Some(r) => write!(f, "%{}", r)?,
        };

        for r in iter {
            write!(f, ", %{}", r)?;
        }

        Ok(())
    }
}

pub trait TypeStore {}

/// The contract provided by any single instruction. Provides methods to make
/// interfacing with all instructions easy.
pub trait ISAInstruction<C: Tag> {
    /// An instruction is considered `pure` if its removal has no side effects
    /// for the execution of the program.
    ///
    /// Pure instructions are removed by optimization passes if the resultant
    /// type of the operation is a known constant, or if its result is unused.
    ///
    /// # Examples
    ///
    /// An example of a pure instruction is allocation of memory is considered.
    /// Despite it possibly having side effects regarding memory allocation,
    /// this type of side effect is un-observable to the actual behavior of the
    /// program.
    ///
    /// An example of a non-pure instruction would be calls to external
    /// functions, because removal of the instruction could cause a change in
    /// the behavior of the program.
    fn is_pure() -> bool {
        true
    }

    /// When an instruction introduces a register into the program, it
    /// "declares" it. This method is used to get what instructions declare
    /// which registers, so that optimization passes may examine the usages of
    /// these registers.
    fn declared_register(&self) -> Option<RegisterId<C>>;

    /// An instruction is considered to use registers when those registers are
    /// used as operands of the current register. This means that declared
    /// registers are not considered used.
    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]>;

    /// Analogous to [`ISAInstruction::used_registers`], except that it
    /// provides mutable access to the registers being used to allow for
    /// changes to the registers.
    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>>;

    fn display(&self, w: &mut impl Write) -> std::fmt::Result;

    fn display_typed(&self, w: &mut impl Write, t: &impl TypeStore) -> std::fmt::Result {
        self.display(w)
    }
}

pub struct Noop;

impl<C: Tag> ISAInstruction<C> for Noop {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "Noop;")
    }
}

pub struct Unreachable;

impl<C: Tag> ISAInstruction<C> for Unreachable {
    fn is_pure() -> bool {
        // removing this instruction affects program optimization
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "Unreachable;")
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Return<C: Tag>(pub Option<RegisterId<C>>);

impl<C: Tag> ISAInstruction<C> for Return<C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        match self.0 {
            Some(r) => TinyVec::from([r, Default::default(), Default::default()]),
            None => TinyVec::new(),
        }
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        match &mut self.0 {
            Some(r) => vec![r],
            None => Vec::new(),
        }
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        match self.0 {
            Some(r) => write!(w, "Return %{};", r),
            None => write!(w, "Return;"),
        }
    }
}

impl<C: Tag> Return<C> {
    #[track_caller]
    #[allow(clippy::manual_map)] // can't use closures because then `track_caller` doesn't work
    pub fn retag<C2: Tag>(self, retagger: &impl RegRetagger<C, C2>) -> Return<C2> {
        Return(match self.0 {
            Some(r) => Some(retagger.retag_old(r)),
            None => None,
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct BlockJump<B: IdCompat, C: Tag>(pub B, pub Vec<RegisterId<C>>);

impl<B: Tag, C: Tag> BlockJump<BlockId<B>, C> {
    #[track_caller]
    pub fn retag<B2: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_retagger: &impl BlkRetagger<B, B2>,
    ) -> BlockJump<BlockId<B2>, C2> {
        BlockJump(blk_retagger.retag_old(self.0), retagger.retag_olds(self.1))
    }

    #[track_caller]
    pub fn retag_to_blk<F: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_to_fn: &impl BlkToFn<B, F>,
    ) -> BlockJump<FunctionId<F>, C2> {
        BlockJump(blk_to_fn.retag(self.0), retagger.retag_olds(self.1))
    }
}

impl<B: Tag, R: Tag> Display for BlockJump<BlockId<B>, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}({})", self.0, Registers(&self.1))
    }
}

impl<F: Tag, R: Tag> Display for BlockJump<FunctionId<F>, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}({})", self.0, Registers(&self.1))
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Jump<B: IdCompat, C: Tag>(pub BlockJump<B, C>);

impl<B: Tag, C: Tag> ISAInstruction<C> for Jump<BlockId<B>, C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&(self.0).1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.0).1.iter_mut().collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "Jump {}", self.0)
    }
}

// TODO: is there a way we can de-duplicate code?
impl<F: Tag, C: Tag> ISAInstruction<C> for Jump<FunctionId<F>, C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&(self.0).1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.0).1.iter_mut().collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "Jump {}", self.0)
    }
}

impl<B: Tag, C: Tag> Jump<BlockId<B>, C> {
    #[track_caller]
    pub fn retag<B2: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_retagger: &impl BlkRetagger<B, B2>,
    ) -> Jump<BlockId<B2>, C2> {
        Jump(self.0.retag(retagger, blk_retagger))
    }

    #[track_caller]
    pub fn retag_to_blk<F: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_to_fn: &impl BlkToFn<B, F>,
    ) -> Jump<FunctionId<F>, C2> {
        Jump(self.0.retag_to_blk(retagger, blk_to_fn))
    }

    pub fn paths(&self) -> Vec<&BlockJump<BlockId<B>, C>> {
        vec![&self.0]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<BlockId<B>, C>> {
        vec![&mut self.0]
    }
}

impl<F: Tag, R: Tag> Jump<FunctionId<F>, R> {
    pub fn paths(&self) -> Vec<&BlockJump<FunctionId<F>, R>> {
        vec![&self.0]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<FunctionId<F>, R>> {
        vec![&mut self.0]
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct NewRecord<C: Tag> {
    pub result: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for NewRecord<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = NewRecord;", self.result)
    }
}

impl<C: Tag> NewRecord<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> NewRecord<C2> {
        NewRecord {
            result: retagger.retag_new(self.result),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct MakeInteger<C: Tag> {
    pub result: RegisterId<C>,
    pub value: i64,
}

impl<C: Tag> ISAInstruction<C> for MakeInteger<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = MakeInteger {};", self.result, self.value)
    }
}

impl<C: Tag> MakeInteger<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> MakeInteger<C2> {
        MakeInteger {
            result: retagger.retag_new(self.result),
            value: self.value,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct MakeBoolean<C: Tag> {
    pub result: RegisterId<C>,
    pub value: bool,
}

impl<C: Tag> ISAInstruction<C> for MakeBoolean<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = MakeBoolean {};", self.result, self.value)
    }
}

impl<C: Tag> MakeBoolean<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> MakeBoolean<C2> {
        MakeBoolean {
            result: retagger.retag_new(self.result),
            value: self.value,
        }
    }
}

/// [`MakeTrivial`] creates trivial items. Trivial items are elements with a
/// single possible value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MakeTrivial<C: Tag> {
    pub result: RegisterId<C>,
    pub item: TrivialItem,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum TrivialItem {
    /// JSSAT Runtime
    Runtime,
    /// JS null
    Null,
    /// JS undefined
    Undefined,
    /// ECMAScript "empty"
    Empty,
    /// Completion record "throw"
    Throw,
    /// Reference Record "unresolvable"
    Unresolvable,
}

impl<C: Tag> ISAInstruction<C> for MakeTrivial<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = MakeTrivial {};", self.result, self.item)
    }
}

impl<C: Tag> MakeTrivial<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> MakeTrivial<C2> {
        MakeTrivial {
            result: retagger.retag_new(self.result),
            item: self.item,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MakeBytes<R: Tag, C: Tag> {
    pub result: RegisterId<R>,
    pub constant: ConstantId<C>,
}

impl<R: Tag, C: Tag> ISAInstruction<R> for MakeBytes<R, C> {
    fn declared_register(&self) -> Option<RegisterId<R>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<R>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<R>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = MakeBytes #{};", self.result, self.constant)
    }
}

impl<R: Tag, C: Tag> MakeBytes<R, C> {
    #[track_caller]
    pub fn retag<R2: Tag, C2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<R, R2>,
        const_retagger: &impl CnstRetagger<C, C2>,
    ) -> MakeBytes<R2, C2> {
        MakeBytes {
            result: reg_retagger.retag_new(self.result),
            constant: const_retagger.retag_old(self.constant),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct GetFnPtr<R: Tag, F: Tag> {
    pub result: RegisterId<R>,
    pub function: FunctionId<F>,
}

impl<R: Tag, F: Tag> ISAInstruction<R> for GetFnPtr<R, F> {
    fn declared_register(&self) -> Option<RegisterId<R>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<R>; 3]> {
        tiny_vec![]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<R>> {
        vec![]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = GetFnPtr @{};", self.result, self.function)
    }
}

impl<R: Tag, F: Tag> GetFnPtr<R, F> {
    #[track_caller]
    pub fn retag<R2: Tag, F2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<R, R2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> GetFnPtr<R2, F2> {
        GetFnPtr {
            result: reg_retagger.retag_new(self.result),
            function: fn_retagger.retag_old(self.function),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Negate<C: Tag> {
    pub result: RegisterId<C>,
    pub operand: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for Negate<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.operand]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.operand]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = Negate %{};", self.result, self.operand)
    }
}

impl<C: Tag> Negate<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> Negate<C2> {
        Negate {
            result: retagger.retag_new(self.result),
            operand: retagger.retag_old(self.operand),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Add<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for Add<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = Add %{}, %{}", self.result, self.lhs, self.rhs)
    }
}

impl<C: Tag> Add<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> Add<C2> {
        Add {
            result: retagger.retag_new(self.result),
            lhs: retagger.retag_old(self.lhs),
            rhs: retagger.retag_old(self.rhs),
        }
    }
}

pub struct Or<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for Or<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = Or %{}, %{};", self.result, self.lhs, self.rhs)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LessThan<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for LessThan<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = LessThan %{}, %{};",
            self.result, self.lhs, self.rhs
        )
    }
}

impl<C: Tag> LessThan<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> LessThan<C2> {
        LessThan {
            result: retagger.retag_new(self.result),
            lhs: retagger.retag_old(self.lhs),
            rhs: retagger.retag_old(self.rhs),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Equals<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for Equals<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = Equals %{}, %{};", self.result, self.lhs, self.rhs)
    }
}

impl<C: Tag> Equals<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> Equals<C2> {
        Equals {
            result: retagger.retag_new(self.result),
            lhs: retagger.retag_old(self.lhs),
            rhs: retagger.retag_old(self.rhs),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum RecordKey<C: Tag> {
    #[display(fmt = "%{}", _0)]
    Prop(RegisterId<C>),
    #[display(fmt = "[[{}]]", _0)]
    Slot(InternalSlot),
}

impl<C: Tag> RecordKey<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &impl RegRetagger<C, C2>) -> RecordKey<C2> {
        match self {
            RecordKey::Prop(r) => RecordKey::Prop(retagger.retag_old(r)),
            RecordKey::Slot(s) => RecordKey::Slot(s),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum InternalSlot {
    JSSATRandomDebugSlot,
    JSSATHasBinding,
    JSSATHasVarDeclaration,
    JSSATHasLexicalDeclaration,
    JSSATHasRestrictedGlobalProperty,
    JSSATCreateGlobalFunctionBinding,
    // TODO: expand this to all ecmascript internal slot types
    // (should this even be here?)
    Function,
    Realm,
    ScriptOrModule,
    GlobalObject,
    GlobalEnv,
    TemplateMap,
    Intrinsics,
    Call,
    ECMAScriptCode,
    HostDefined,
    VariableEnvironment,
    LexicalEnvironment,
    PrivateEnvironment,
    Base,
    ReferencedName,
    Strict,
    ThisValue,
    Value,
    Type,
    Target,
    BindingObject,
    DeclarativeRecord,
    ObjectRecord,
    OuterEnv,
    IsWithEnvironment,
    GlobalThisValue,
    /// `%Function.prototype%`
    FunctionPrototype,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordGet<C: Tag, S = ()> {
    pub result: RegisterId<C>,
    pub shape: S,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
}

impl<C: Tag, S> ISAInstruction<C> for RecordGet<C, S> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.record];
        if let RecordKey::Prop(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.record];
        if let RecordKey::Prop(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = RecordGet %{}.{};",
            self.result, self.record, self.key
        )
    }
}

impl<C: Tag, S> RecordGet<C, S> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordGet<C2, S> {
        RecordGet {
            shape: self.shape,
            result: retagger.retag_new(self.result),
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordSet<C: Tag, S = ()> {
    pub shape: S,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
    pub value: RegisterId<C>,
}

impl<C: Tag, S> ISAInstruction<C> for RecordSet<C, S> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.record, self.value];
        if let RecordKey::Prop(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.record, &mut self.value];
        if let RecordKey::Prop(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "RecordSet %{}.{} = %{}",
            self.record, self.key, self.value
        )
    }
}

impl<C: Tag, S> RecordSet<C, S> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordSet<C2, S> {
        RecordSet {
            shape: self.shape,
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
            value: retagger.retag_old(self.value),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct JumpIf<B: IdCompat, C: Tag> {
    pub condition: RegisterId<C>,
    pub if_so: BlockJump<B, C>,
    pub other: BlockJump<B, C>,
}

impl<B: Tag, C: Tag> ISAInstruction<C> for JumpIf<BlockId<B>, C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&self.if_so.1);
        used_registers.extend_from_slice(&self.other.1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.if_so.1.iter_mut())
            .chain(self.other.1.iter_mut())
            .collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "If %{}:", self.condition)?;
        w.write_str("\n")?;
        write!(w, "     {};", self.if_so)?;
        w.write_str("\n")?;
        write!(w, "else {};", self.other)
    }
}

// TODO: is there a way we can deduplicate code?
impl<F: Tag, C: Tag> ISAInstruction<C> for JumpIf<FunctionId<F>, C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&self.if_so.1);
        used_registers.extend_from_slice(&self.other.1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.if_so.1.iter_mut())
            .chain(self.other.1.iter_mut())
            .collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "If %{}:", self.condition)?;
        w.write_str("\n")?;
        write!(w, "     {};", self.if_so)?;
        w.write_str("\n")?;
        write!(w, "else {};", self.other)
    }
}

impl<B: Tag, C: Tag> JumpIf<BlockId<B>, C> {
    #[track_caller]
    pub fn retag<B2: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_retagger: &impl BlkRetagger<B, B2>,
    ) -> JumpIf<BlockId<B2>, C2> {
        JumpIf {
            condition: retagger.retag_old(self.condition),
            if_so: self.if_so.retag(retagger, blk_retagger),
            other: self.other.retag(retagger, blk_retagger),
        }
    }

    #[track_caller]
    pub fn retag_to_blk<F: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_to_fn: &impl BlkToFn<B, F>,
    ) -> JumpIf<FunctionId<F>, C2> {
        JumpIf {
            condition: retagger.retag_old(self.condition),
            if_so: self.if_so.retag_to_blk(retagger, blk_to_fn),
            other: self.other.retag_to_blk(retagger, blk_to_fn),
        }
    }

    pub fn paths(&self) -> Vec<&BlockJump<BlockId<B>, C>> {
        vec![&self.if_so, &self.other]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<BlockId<B>, C>> {
        vec![&mut self.if_so, &mut self.other]
    }
}

impl<F: Tag, R: Tag> JumpIf<FunctionId<F>, R> {
    pub fn paths(&self) -> Vec<&BlockJump<FunctionId<F>, R>> {
        vec![&self.if_so, &self.other]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<FunctionId<F>, R>> {
        vec![&mut self.if_so, &mut self.other]
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallStatic<C: Tag, F: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_id: FunctionId<F>,
    // TODO: figure out if there's a common size, to use `TinyVec`
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag, F: Tag> ISAInstruction<C> for CallStatic<C, F> {
    fn is_pure() -> bool {
        // inside of the function may be calls to external functions
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::from(self.args.as_slice())
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        self.args.iter_mut().collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        if let Some(r) = self.result {
            write!(w, "%{} = ", r)?;
        }
        write!(w, "CallStatic @{}({})", self.fn_id, Registers(&self.args))
    }
}

impl<C: Tag, F: Tag> CallStatic<C, F> {
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        retagger: &mut impl RegRetagger<C, C2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> CallStatic<C2, F2> {
        CallStatic {
            result: self.result.map(|r| retagger.retag_new(r)),
            fn_id: fn_retagger.retag_old(self.fn_id),
            args: retagger.retag_olds(self.args),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallVirt<C: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_ptr: RegisterId<C>,
    // TODO: figure out if there's a common size, to use `TinyVec`
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag> ISAInstruction<C> for CallVirt<C> {
    fn is_pure() -> bool {
        // calling a function that may call external functions is side-effectful
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::with_capacity(self.args.len() + 1);
        used_registers.extend(self.args.iter().copied());
        used_registers.push(self.fn_ptr);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        self.args
            .iter_mut()
            .chain(std::iter::once(&mut self.fn_ptr))
            .collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        if let Some(r) = self.result {
            write!(w, "%{} = ", r)?;
        }
        write!(w, "CallVirt %{}({})", self.fn_ptr, Registers(&self.args))
    }
}

impl<C: Tag> CallVirt<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> CallVirt<C2> {
        CallVirt {
            result: self.result.map(|r| retagger.retag_new(r)),
            fn_ptr: retagger.retag_old(self.fn_ptr),
            args: retagger.retag_olds(self.args),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallExtern<C: Tag, F: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_id: ExternalFunctionId<F>,
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag, F: Tag> ISAInstruction<C> for CallExtern<C, F> {
    fn is_pure() -> bool {
        // calling external functions is inherently side-effectful
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::from(self.args.as_slice())
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        self.args.iter_mut().collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        if let Some(r) = self.result {
            write!(w, "%{} = ", r)?;
        }
        write!(w, "CallExtern @@{}({})", self.fn_id, Registers(&self.args))
    }
}

impl<C: Tag, F: Tag> CallExtern<C, F> {
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<C, C2>,
        ext_fn_retagger: &impl ExtFnRetagger<F, F2>,
    ) -> CallExtern<C2, F2> {
        CallExtern {
            result: self.result.map(|r| reg_retagger.retag_new(r)),
            fn_id: ext_fn_retagger.retag_old(self.fn_id),
            args: reg_retagger.retag_olds(self.args),
        }
    }
}

// TODO: widen/narrow instructions that operate based on a type
pub struct Widen<C: Tag> {
    pub result: RegisterId<C>,
    pub input: RegisterId<C>,
    pub typ: (),
}

impl<C: Tag> ISAInstruction<C> for Widen<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.input]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.input]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        todo!()
    }
}

pub struct Narrow<C: Tag> {
    pub result: RegisterId<C>,
    pub input: RegisterId<C>,
    pub typ: (),
}

impl<C: Tag> ISAInstruction<C> for Narrow<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.input]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.input]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        todo!()
    }
}
