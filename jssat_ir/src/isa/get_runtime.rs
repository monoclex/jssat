use std::fmt::Write;
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct GetRuntime<T: Tag> {
    pub result: RegisterId<T>,
}

impl<C: Tag> ISAInstruction<C> for GetRuntime<C> {
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
        write!(w, "%{} = GetRuntime;", self.result)
    }
}

impl<T: Tag> GetRuntime<T> {
    #[track_caller]
    pub fn retag<T2: Tag>(self, retagger: &mut impl RegRetagger<T, T2>) -> GetRuntime<T2> {
        GetRuntime {
            result: retagger.retag_new(self.result),
        }
    }
}
