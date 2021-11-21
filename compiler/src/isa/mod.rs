//! This crate defines a contract that all instructions must provide. It is used
//! to allow easy composability of instructions for other IR passes with their
//! own in-house ISAs.

use std::fmt::{Display, Write};
use tinyvec::TinyVec;

use crate::id::RegisterId;
use crate::id::Tag;

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
}

mod atom;
pub use atom::{Atom, AtomDealer};

mod noop;
pub use noop::Noop;

mod unreachable;
pub use unreachable::Unreachable;

mod comment;
pub use comment::Comment;

mod control_flow;
pub use control_flow::{BlockJump, Jump, JumpIf, Return};

mod make;
pub use make::{Make, TrivialItem};

mod negate;
pub use negate::Negate;

mod binop;
pub use binop::{BinOp, BinaryOperator};

mod records;
pub use records::{InternalSlot, NewRecord, RecordGet, RecordHasKey, RecordKey, RecordSet};

mod lists;
pub use lists::{ListGet, ListHasKey, ListKey, ListLen, ListSet, NewList};

mod call;
pub use call::Call;

// TODO: widen/narrow instructions that operate based on a type
mod widen;
pub use widen::Widen;

mod narrow;
pub use narrow::Narrow;

mod generalize;
pub use generalize::Generalize;

mod assert;
pub use assert::Assert;

mod is_type;
pub use is_type::{CompareType, IsType, ValueType};

mod get_runtime;
pub use get_runtime::GetRuntime;

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
