use std::fmt::Write;
use tinyvec::TinyVec;

use super::{ISAInstruction, Registers};
use crate::{
    id::*,
    retag::{FnRetagger, RegRetagger},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallStatic<C: Tag, F: Tag> {
    pub result: Option<RegisterId<C>>,
    pub fn_id: FunctionId<F>,
    // TODO: figure out if there's a common size, to use `TinyVec`
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag, F: Tag> ISAInstruction<C> for CallStatic<C, F> {
    fn is_pure() -> bool {
        // inside of the function may be calls to external functions
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
        write!(w, "CallStatic @{}({})", self.fn_id, Registers(&self.args))
    }
}

impl<C: Tag, F: Tag> CallStatic<C, F> {
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        retagger: &mut impl RegRetagger<C, C2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> CallStatic<C2, F2> {
        CallStatic {
            result: self.result.map(|r| retagger.retag_new(r)),
            fn_id: fn_retagger.retag_old(self.fn_id),
            args: retagger.retag_olds(self.args),
        }
    }
}
