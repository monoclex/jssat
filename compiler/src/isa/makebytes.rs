use std::fmt::Write;
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::{
    id::*,
    retag::{CnstRetagger, RegRetagger},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MakeBytes<R: Tag, C: Tag> {
    pub result: RegisterId<R>,
    pub constant: ConstantId<C>,
}

impl<R: Tag, C: Tag> ISAInstruction<R> for MakeBytes<R, C> {
    fn declared_register(&self) -> Option<RegisterId<R>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<R>; 3]> {
        TinyVec::new()
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<R>> {
        Vec::new()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = MakeBytes #{};", self.result, self.constant)
    }
}

impl<R: Tag, C: Tag> MakeBytes<R, C> {
    #[track_caller]
    pub fn retag<R2: Tag, C2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<R, R2>,
        const_retagger: &impl CnstRetagger<C, C2>,
    ) -> MakeBytes<R2, C2> {
        MakeBytes {
            result: reg_retagger.retag_new(self.result),
            constant: const_retagger.retag_old(self.constant),
        }
    }
}
