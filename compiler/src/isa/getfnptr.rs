use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{
    id::*,
    retag::{FnRetagger, RegRetagger},
};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct GetFnPtr<R: Tag, F: Tag> {
    pub result: RegisterId<R>,
    pub function: FunctionId<F>,
}

impl<R: Tag, F: Tag> ISAInstruction<R> for GetFnPtr<R, F> {
    fn declared_register(&self) -> Option<RegisterId<R>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<R>; 3]> {
        tiny_vec![]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<R>> {
        vec![]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "%{} = GetFnPtr @{};", self.result, self.function)
    }
}

impl<R: Tag, F: Tag> GetFnPtr<R, F> {
    #[track_caller]
    pub fn retag<R2: Tag, F2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<R, R2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> GetFnPtr<R2, F2> {
        GetFnPtr {
            result: reg_retagger.retag_new(self.result),
            function: fn_retagger.retag_old(self.function),
        }
    }
}
