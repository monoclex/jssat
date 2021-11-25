use std::fmt::Write;
use tinyvec::TinyVec;

use super::{ISAInstruction, Registers};
use crate::{
    id::*,
    retag::{ExtFnRetagger, FnRetagger, RegRetagger},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Call<C: Tag, F> {
    pub result: Option<RegisterId<C>>,
    pub calling: F,
    pub args: Vec<RegisterId<C>>,
}

impl<C: Tag, F: Tag> ISAInstruction<C> for Call<C, ExternalFunctionId<F>> {
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
        write!(
            w,
            "CallExtern @@{}({})",
            self.calling,
            Registers(&self.args)
        )
    }
}

impl<C: Tag, F: Tag> Call<C, ExternalFunctionId<F>> {
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        reg_retagger: &mut impl RegRetagger<C, C2>,
        ext_fn_retagger: &impl ExtFnRetagger<F, F2>,
    ) -> Call<C2, ExternalFunctionId<F2>> {
        Call {
            result: self.result.map(|r| reg_retagger.retag_new(r)),
            calling: ext_fn_retagger.retag_old(self.calling),
            args: reg_retagger.retag_olds(self.args),
        }
    }
}

impl<C: Tag, F: Tag> ISAInstruction<C> for Call<C, FunctionId<F>> {
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
        write!(w, "CallStatic @{}({})", self.calling, Registers(&self.args))
    }
}

impl<C: Tag, F: Tag> Call<C, FunctionId<F>> {
    #[track_caller]
    pub fn retag<C2: Tag, F2: Tag>(
        self,
        retagger: &mut impl RegRetagger<C, C2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> Call<C2, FunctionId<F2>> {
        Call {
            result: self.result.map(|r| retagger.retag_new(r)),
            calling: fn_retagger.retag_old(self.calling),
            args: retagger.retag_olds(self.args),
        }
    }
}

impl<C: Tag> ISAInstruction<C> for Call<C, RegisterId<C>> {
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
        used_registers.push(self.calling);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        self.args
            .iter_mut()
            .chain(std::iter::once(&mut self.calling))
            .collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        if let Some(r) = self.result {
            write!(w, "%{} = ", r)?;
        }
        write!(w, "CallVirt %{}({})", self.calling, Registers(&self.args))
    }
}

impl<C: Tag> Call<C, RegisterId<C>> {
    #[track_caller]
    pub fn retag<C2: Tag>(
        self,
        retagger: &mut impl RegRetagger<C, C2>,
    ) -> Call<C2, RegisterId<C2>> {
        Call {
            result: self.result.map(|r| retagger.retag_new(r)),
            calling: retagger.retag_old(self.calling),
            args: retagger.retag_olds(self.args),
        }
    }
}
