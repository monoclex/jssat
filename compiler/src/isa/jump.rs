use std::fmt::Write;
use tinyvec::TinyVec;

use super::{BlockJump, ISAInstruction};
use crate::{
    id::*,
    retag::{BlkRetagger, BlkToFn, RegRetagger},
};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Jump<B: IdCompat, C: Tag>(pub BlockJump<B, C>);

impl<B: Tag, C: Tag> ISAInstruction<C> for Jump<BlockId<B>, C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&(self.0).1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.0).1.iter_mut().collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "Jump {}", self.0)
    }
}

// TODO: is there a way we can de-duplicate code?
impl<F: Tag, C: Tag> ISAInstruction<C> for Jump<FunctionId<F>, C> {
    fn is_pure() -> bool {
        // purity is only useful in regards to eliminating work,
        // LLVM will optimize control flow
        false
    }

    fn declared_register(&self) -> Option<RegisterId<C>> {
        None
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<C>; 3]> {
        let mut used_registers = TinyVec::new();
        used_registers.extend_from_slice(&(self.0).1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.0).1.iter_mut().collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "Jump {}", self.0)
    }
}

impl<B: Tag, C: Tag> Jump<BlockId<B>, C> {
    #[track_caller]
    pub fn retag<B2: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_retagger: &impl BlkRetagger<B, B2>,
    ) -> Jump<BlockId<B2>, C2> {
        Jump(self.0.retag(retagger, blk_retagger))
    }

    #[track_caller]
    pub fn retag_to_blk<F: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_to_fn: &impl BlkToFn<B, F>,
    ) -> Jump<FunctionId<F>, C2> {
        Jump(self.0.retag_to_blk(retagger, blk_to_fn))
    }

    pub fn paths(&self) -> Vec<&BlockJump<BlockId<B>, C>> {
        vec![&self.0]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<BlockId<B>, C>> {
        vec![&mut self.0]
    }
}

impl<F: Tag, R: Tag> Jump<FunctionId<F>, R> {
    pub fn paths(&self) -> Vec<&BlockJump<FunctionId<F>, R>> {
        vec![&self.0]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<FunctionId<F>, R>> {
        vec![&mut self.0]
    }
}
