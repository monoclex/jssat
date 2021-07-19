//! This crate defines a contract that all instructions must provide. It is used
//! to allow easy composability of instructions for other IR passes with their
//! own in-house ISAs.

use derive_more::Display;
use tinyvec::{tiny_vec, TinyVec};

use crate::id::ExternalFunctionId;
use crate::id::FunctionId;
use crate::id::RegisterId;
use crate::id::Tag;

type ConstantId = crate::id::ConstantId<crate::id::NoContext>;
type BlockId = crate::id::BlockId<crate::id::NoContext>;

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
}

// TODO: check if this works
pub trait ISAInstruction2<C: Tag> {
    type ContextMap;

    fn map_context(self) -> Self::ContextMap;
}

impl<C: Tag, C2: Tag> ISAInstruction2<C2> for Return<C> {
    type ContextMap = Return<C2>;

    fn map_context(self) -> Self::ContextMap {
        todo!()
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
}

pub struct Return<C: Tag>(pub Option<RegisterId<C>>);

impl<C: Tag> ISAInstruction<C> for Return<C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        self.0
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
}

pub struct BlockJump<C: Tag>(pub BlockId, pub Vec<RegisterId<C>>);

pub struct Jump<C: Tag>(pub BlockJump<C>);

impl<C: Tag> ISAInstruction<C> for Jump<C> {
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
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct MakeRecord<C: Tag> {
    pub result: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for MakeRecord<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }
}

impl<C: Tag> MakeRecord<C> {
    pub fn map_context<C2: Tag>(self) -> MakeRecord<C2> {
        MakeRecord {
            result: self.result.map_context(),
        }
    }
}

pub struct MakeBoolean<C: Tag>(pub RegisterId<C>, pub bool);

impl<C: Tag> ISAInstruction<C> for MakeBoolean<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.0)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
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
}

impl<C: Tag> MakeInteger<C> {
    pub fn map_context<C2: Tag>(self) -> MakeInteger<C2> {
        MakeInteger {
            result: self.result.map_context(),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TrivialItem {
    /// JSSAT Runtime
    Runtime,
    /// JS null
    Null,
    /// JS undefined
    Undefined,
    /// ECMAScript "empty"
    Empty,
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
}

impl<C: Tag> MakeTrivial<C> {
    pub fn map_context<C2: Tag>(self) -> MakeTrivial<C2> {
        MakeTrivial {
            result: self.result.map_context(),
            item: self.item,
        }
    }
}

pub struct MakeBytes<C: Tag>(pub RegisterId<C>, pub ConstantId);

impl<C: Tag> ISAInstruction<C> for MakeBytes<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.0)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        Vec::new()
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct OpNegate<C: Tag> {
    pub result: RegisterId<C>,
    pub operand: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for OpNegate<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.operand]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.operand]
    }
}

impl<C: Tag> OpNegate<C> {
    pub fn map_context<C2: Tag>(self) -> OpNegate<C2> {
        OpNegate {
            result: self.result.map_context(),
            operand: self.operand.map_context(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct OpAdd<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for OpAdd<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }
}

impl<C: Tag> OpAdd<C> {
    pub fn map_context<C2: Tag>(self) -> OpAdd<C2> {
        OpAdd {
            result: self.result.map_context(),
            lhs: self.lhs.map_context(),
            rhs: self.rhs.map_context(),
        }
    }
}

pub struct OpOr<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for OpOr<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct OpLessThan<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for OpLessThan<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }
}

impl<C: Tag> OpLessThan<C> {
    pub fn map_context<C2: Tag>(self) -> OpLessThan<C2> {
        OpLessThan {
            result: self.result.map_context(),
            lhs: self.lhs.map_context(),
            rhs: self.rhs.map_context(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct OpEquals<C: Tag> {
    pub result: RegisterId<C>,
    pub lhs: RegisterId<C>,
    pub rhs: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for OpEquals<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.lhs, &mut self.rhs]
    }
}

impl<C: Tag> OpEquals<C> {
    pub fn map_context<C2: Tag>(self) -> OpEquals<C2> {
        OpEquals {
            result: self.result.map_context(),
            lhs: self.lhs.map_context(),
            rhs: self.rhs.map_context(),
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
    pub fn map_context<C2: Tag>(self) -> RecordKey<C2> {
        match self {
            RecordKey::Prop(r) => RecordKey::Prop(r.map_context()),
            RecordKey::Slot(s) => RecordKey::Slot(s),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum InternalSlot {
    // TODO: expand this to all ecmascript internal slot types
    // (should this even be here?)
    Call,
    HostDefined,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordGet<C: Tag> {
    pub result: RegisterId<C>,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
}

impl<C: Tag> ISAInstruction<C> for RecordGet<C> {
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
}

impl<C: Tag> RecordGet<C> {
    pub fn map_context<C2: Tag>(self) -> RecordGet<C2> {
        RecordGet {
            result: self.result.map_context(),
            record: self.record.map_context(),
            key: self.key.map_context(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordSet<C: Tag> {
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
    pub value: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for RecordSet<C> {
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
}

impl<C: Tag> RecordSet<C> {
    pub fn map_context<C2: Tag>(self) -> RecordSet<C2> {
        RecordSet {
            record: self.record.map_context(),
            key: self.key.map_context(),
            value: self.value.map_context(),
        }
    }
}

pub struct JumpIf<C: Tag> {
    pub condition: RegisterId<C>,
    pub if_so: BlockJump<C>,
    pub other: BlockJump<C>,
}

impl<C: Tag> ISAInstruction<C> for JumpIf<C> {
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
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallStatic<C: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_id: FunctionId<C>,
    // TODO: figure out if there's a common size, to use `TinyVec`
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag> ISAInstruction<C> for CallStatic<C> {
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
}

impl<C: Tag> CallStatic<C> {
    pub fn map_context<C2: Tag>(self) -> CallStatic<C2> {
        CallStatic {
            result: self.result.map(|r| r.map_context()),
            fn_id: self.fn_id.map_context(),
            args: self.args.into_iter().map(|r| r.map_context()).collect(),
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
}

impl<C: Tag> CallVirt<C> {
    pub fn map_context<C2: Tag>(self) -> CallVirt<C2> {
        CallVirt {
            result: self.result.map(|r| r.map_context()),
            fn_ptr: self.fn_ptr.map_context(),
            args: self.args.into_iter().map(|r| r.map_context()).collect(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallExtern<C: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_id: ExternalFunctionId<C>,
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag> ISAInstruction<C> for CallExtern<C> {
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
}

impl<C: Tag> CallExtern<C> {
    pub fn map_context<C2: Tag>(self) -> CallExtern<C2> {
        CallExtern {
            result: self.result.map(|r| r.map_context()),
            fn_id: self.fn_id.map_context(),
            args: self.args.into_iter().map(|r| r.map_context()).collect(),
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
}
