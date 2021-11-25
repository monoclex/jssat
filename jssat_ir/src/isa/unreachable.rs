use std::fmt::Write;
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Unreachable<T: Tag> {
    pub result: RegisterId<T>,
}

impl<C: Tag> ISAInstruction<C> for Unreachable<C> {
    fn is_pure() -> bool {
        // removing this instruction affects program optimization
        false
    }

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
        write!(w, "%{} = Unreachable;", self.result)
    }
}

impl<C: Tag> Unreachable<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> Unreachable<C2> {
        Unreachable {
            result: retagger.retag_new(self.result),
        }
    }
}
