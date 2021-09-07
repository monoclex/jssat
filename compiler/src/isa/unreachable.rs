use std::fmt::Write;
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::id::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
