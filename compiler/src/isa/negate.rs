use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

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
