use derive_more::Display;
use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum ValueType {
    Trivial,
    Bytes,
    Number,
    Boolean,
    FnPtr,
    Record,
    Symbol,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IsType<C: Tag> {
    pub result: RegisterId<C>,
    pub value: RegisterId<C>,
    pub kind: ValueType,
}

impl<C: Tag> ISAInstruction<C> for IsType<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        tiny_vec![self.value]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        vec![&mut self.value]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = IsType {} %{};",
            self.result, self.kind, self.value
        )
    }
}

impl<C: Tag> IsType<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> IsType<C2> {
        IsType {
            result: retagger.retag_new(self.result),
            value: retagger.retag_old(self.value),
            kind: self.kind,
        }
    }
}
