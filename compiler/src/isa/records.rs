#![allow(non_camel_case_types)]

use derive_more::Display;
use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::{Atom, ISAInstruction};
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum RecordKey<C: Tag> {
    #[display(fmt = "%{}", _0)]
    Prop(RegisterId<C>),
    #[display(fmt = "[[{}]]", _0)]
    Atom(Atom),
}

impl<C: Tag> RecordKey<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &impl RegRetagger<C, C2>) -> RecordKey<C2> {
        match self {
            RecordKey::Prop(r) => RecordKey::Prop(retagger.retag_old(r)),
            RecordKey::Atom(s) => RecordKey::Atom(s),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct NewRecord<C: Tag> {
    pub result: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for NewRecord<C> {
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
        write!(w, "%{} = NewRecord;", self.result)
    }
}

impl<C: Tag> NewRecord<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> NewRecord<C2> {
        NewRecord {
            result: retagger.retag_new(self.result),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordHasKey<C: Tag> {
    pub result: RegisterId<C>,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
}

impl<C: Tag> ISAInstruction<C> for RecordHasKey<C> {
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

impl<C: Tag> RecordHasKey<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordHasKey<C2> {
        RecordHasKey {
            result: retagger.retag_new(self.result),
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordGet<C: Tag> {
    pub result: RegisterId<C>,
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
}

impl<C: Tag> ISAInstruction<C> for RecordGet<C> {
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
            "%{} = RecordGet %{}.{};",
            self.result, self.record, self.key
        )
    }
}

impl<C: Tag> RecordGet<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordGet<C2> {
        RecordGet {
            result: retagger.retag_new(self.result),
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecordSet<C: Tag> {
    pub record: RegisterId<C>,
    pub key: RecordKey<C>,
    pub value: Option<RegisterId<C>>,
}

impl<C: Tag> ISAInstruction<C> for RecordSet<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.record];
        if let Some(register) = self.value {
            used_registers.push(register);
        }
        if let RecordKey::Prop(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.record];
        if let Some(register) = &mut self.value {
            used_registers.push(register);
        }
        if let RecordKey::Prop(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "RecordSet %{}.{} = {}",
            self.record,
            self.key,
            match self.value {
                Some(r) => format!("%{}", r),
                None => "<remove>".to_string(),
            }
        )
    }
}

impl<C: Tag> RecordSet<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> RecordSet<C2> {
        RecordSet {
            record: retagger.retag_old(self.record),
            key: self.key.retag(retagger),
            value: self.value.map(|value| retagger.retag_old(value)),
        }
    }
}
