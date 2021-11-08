use derive_more::Display;
use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum ListKey<T: Tag> {
    #[display(fmt = "[{}]", _0)]
    Index(RegisterId<T>),
}

impl<C: Tag> ListKey<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &impl RegRetagger<C, C2>) -> ListKey<C2> {
        match self {
            ListKey::Index(r) => ListKey::Index(retagger.retag_old(r)),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct NewList<C: Tag> {
    pub result: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for NewList<C> {
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
        write!(w, "%{} = NewList;", self.result)
    }
}

impl<C: Tag> NewList<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> NewList<C2> {
        NewList {
            result: retagger.retag_new(self.result),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ListHasKey<C: Tag> {
    pub result: RegisterId<C>,
    pub list: RegisterId<C>,
    pub key: ListKey<C>,
}

impl<C: Tag> ISAInstruction<C> for ListHasKey<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.list];
        if let ListKey::Index(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.list];
        if let ListKey::Index(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = ListHasKey %{}.{};",
            self.result, self.list, self.key
        )
    }
}

impl<C: Tag> ListHasKey<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> ListHasKey<C2> {
        ListHasKey {
            result: retagger.retag_new(self.result),
            list: retagger.retag_old(self.list),
            key: self.key.retag(retagger),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ListGet<C: Tag> {
    pub result: RegisterId<C>,
    pub list: RegisterId<C>,
    pub key: ListKey<C>,
}

impl<C: Tag> ISAInstruction<C> for ListGet<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.list];
        if let ListKey::Index(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.list];
        if let ListKey::Index(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = ListGet %{}.{};", self.result, self.list, self.key)
    }
}

impl<C: Tag> ListGet<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> ListGet<C2> {
        ListGet {
            result: retagger.retag_new(self.result),
            list: retagger.retag_old(self.list),
            key: self.key.retag(retagger),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ListSet<C: Tag> {
    pub list: RegisterId<C>,
    pub key: ListKey<C>,
    pub value: Option<RegisterId<C>>,
}

impl<C: Tag> ISAInstruction<C> for ListSet<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.list];
        if let Some(register) = self.value {
            used_registers.push(register);
        }
        if let ListKey::Index(register) = self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.list];
        if let Some(register) = &mut self.value {
            used_registers.push(register);
        }
        if let ListKey::Index(register) = &mut self.key {
            used_registers.push(register);
        }
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "ListSet %{}.{} = {}",
            self.list,
            self.key,
            match self.value {
                Some(r) => format!("%{}", r),
                None => format!("<remove>"),
            }
        )
    }
}

impl<C: Tag> ListSet<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> ListSet<C2> {
        ListSet {
            list: retagger.retag_old(self.list),
            key: self.key.retag(retagger),
            value: self.value.map(|value| retagger.retag_old(value)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ListLen<C: Tag> {
    pub result: RegisterId<C>,
    pub list: RegisterId<C>,
}

impl<C: Tag> ISAInstruction<C> for ListLen<C> {
    fn declared_register(&self) -> Option<RegisterId<C>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = tiny_vec![self.list];
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.list];
        used_registers
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = ListLen %{};", self.result, self.list)
    }
}

impl<C: Tag> ListLen<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> ListLen<C2> {
        ListLen {
            result: retagger.retag_new(self.result),
            list: retagger.retag_old(self.list),
        }
    }
}
