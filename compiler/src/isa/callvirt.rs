use std::fmt::Write;
use tinyvec::TinyVec;

use super::{ISAInstruction, Registers};
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallVirt<C: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_ptr: RegisterId<C>,
    // TODO: figure out if there's a common size, to use `TinyVec`
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag> ISAInstruction<C> for CallVirt<C> {
    fn is_pure() -> bool {
        // calling a function that may call external functions is side-effectful
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::with_capacity(self.args.len() + 1);
        used_registers.extend(self.args.iter().copied());
        used_registers.push(self.fn_ptr);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        self.args
            .iter_mut()
            .chain(std::iter::once(&mut self.fn_ptr))
            .collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        if let Some(r) = self.result {
            write!(w, "%{} = ", r)?;
        }
        write!(w, "CallVirt %{}({})", self.fn_ptr, Registers(&self.args))
    }
}

impl<C: Tag> CallVirt<C> {
    #[track_caller]
    pub fn retag<C2: Tag>(self, retagger: &mut impl RegRetagger<C, C2>) -> CallVirt<C2> {
        CallVirt {
            result: self.result.map(|r| retagger.retag_new(r)),
            fn_ptr: retagger.retag_old(self.fn_ptr),
            args: retagger.retag_olds(self.args),
        }
    }
}
