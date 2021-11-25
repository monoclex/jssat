use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Assert<C: Tag> {
    pub condition: RegisterId<C>,
    pub message: &'static str,
}

impl<C: Tag> ISAInstruction<C> for Assert<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.condition]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.condition]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "Assert %{}; -- {}", self.condition, self.message)
    }
}

impl<C: Tag> Assert<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> Assert<C2> {
        Assert {
            condition: retagger.retag_old(self.condition),
            message: self.message,
        }
    }
}
