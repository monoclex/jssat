use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::{ISAInstruction, RecordKey};
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordHasKey<C: Tag, S = ()> {
    pub result: RegisterId<C>,
    pub shape: S,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
}

impl<C: Tag, S> ISAInstruction<C> for RecordHasKey<C, S> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.record];
        if let RecordKey::Prop(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.record];
        if let RecordKey::Prop(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = RecordHasKey %{}.{};",
            self.result, self.record, self.key
        )
    }
}

impl<C: Tag, S> RecordHasKey<C, S> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordHasKey<C2, S> {
        RecordHasKey {
            shape: self.shape,
            result: retagger.retag_new(self.result),
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
        }
    }
}
