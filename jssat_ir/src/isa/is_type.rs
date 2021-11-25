use derive_more::Display;
use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum ValueType {
    Atom,
    Bytes,
    Number,
    Boolean,
    FnPtr,
    Record,
    BigNumber,
    List,
    Runtime,
}

// TODO(isa/specification): add a new 'type' value to allow carrying around and
//     passing types as values? not sure if that should be done or not
/// Allows checking if the type is equal to the type of another register, or a
/// specific type.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum CompareType<C: Tag> {
    #[display("%{}", .0)]
    Register(RegisterId<C>),
    #[display("{}", .0)]
    Kind(ValueType),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IsType<C: Tag> {
    pub result: RegisterId<C>,
    pub value: RegisterId<C>,
    pub kind: CompareType<C>,
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
            "%{} = IsType %{} = {};",
            self.result, self.value, self.kind
        )
    }
}

impl<C: Tag> CompareType<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> CompareType<C2> {
        match self {
            CompareType::Kind(kind) => CompareType::Kind(kind),
            CompareType::Register(register) => CompareType::Register(retagger.retag_old(register)),
        }
    }
}

impl<C: Tag> IsType<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> IsType<C2> {
        IsType {
            result: retagger.retag_new(self.result),
            value: retagger.retag_old(self.value),
            kind: self.kind.retag(retagger),
        }
    }
}
