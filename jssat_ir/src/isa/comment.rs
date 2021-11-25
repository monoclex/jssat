use std::{fmt::Write, panic::Location};
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::id::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Comment {
    pub message: &'static str,
    pub location: &'static Location<'static>,
}

impl<T: Tag> ISAInstruction<T> for Comment {
    fn declared_register(&self) -> Option<RegisterId<T>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<T>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<T>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "-- {}, {}", self.message, self.location)
    }
}

impl Comment {
    #[track_caller]
    pub fn retag(self) -> Comment {
        self
    }
}
