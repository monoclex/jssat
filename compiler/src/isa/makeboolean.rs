use std::fmt::Write;
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

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
