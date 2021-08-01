use std::fmt::Write;
use tinyvec::TinyVec;

use super::{ISAInstruction, Registers};
use crate::{
    id::*,
    retag::{ExtFnRetagger, RegRetagger},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallExtern<C: Tag, F: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_id: ExternalFunctionId<F>,
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag, F: Tag> ISAInstruction<C> for CallExtern<C, F> {
    fn is_pure() -> bool {
        // calling external functions is inherently side-effectful
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        self.result
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        TinyVec::from(self.args.as_slice())
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        self.args.iter_mut().collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        if let Some(r) = self.result {
            write!(w, "%{} = ", r)?;
        }
        write!(w, "CallExtern @@{}({})", self.fn_id, Registers(&self.args))
    }
}

impl<C: Tag, F: Tag> CallExtern<C, F> {
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<C, C2>,
        ext_fn_retagger: &impl ExtFnRetagger<F, F2>,
    ) -> CallExtern<C2, F2> {
        CallExtern {
            result: self.result.map(|r| reg_retagger.retag_new(r)),
            fn_id: ext_fn_retagger.retag_old(self.fn_id),
            args: reg_retagger.retag_olds(self.args),
        }
    }
}
