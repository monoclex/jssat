use std::fmt::Write;
use tinyvec::TinyVec;

use super::{BlockJump, ISAInstruction};
use crate::{
    id::*,
    retag::{BlkRetagger, BlkToFn, RegRetagger},
};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct JumpIf<B: IdCompat, C: Tag> {
    pub condition: RegisterId<C>,
    pub if_so: BlockJump<B, C>,
    pub other: BlockJump<B, C>,
}

impl<B: Tag, C: Tag> ISAInstruction<C> for JumpIf<BlockId<B>, C> {
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
        used_registers.extend_from_slice(&self.if_so.1);
        used_registers.extend_from_slice(&self.other.1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.if_so.1.iter_mut())
            .chain(self.other.1.iter_mut())
            .collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "If %{}:", self.condition)?;
        w.write_str("\n")?;
        write!(w, "     {};", self.if_so)?;
        w.write_str("\n")?;
        write!(w, "else {};", self.other)
    }
}

// TODO: is there a way we can deduplicate code?
impl<F: Tag, C: Tag> ISAInstruction<C> for JumpIf<FunctionId<F>, C> {
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
        used_registers.extend_from_slice(&self.if_so.1);
        used_registers.extend_from_slice(&self.other.1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        (self.if_so.1.iter_mut())
            .chain(self.other.1.iter_mut())
            .collect()
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(w, "If %{}:", self.condition)?;
        w.write_str("\n")?;
        write!(w, "     {};", self.if_so)?;
        w.write_str("\n")?;
        write!(w, "else {};", self.other)
    }
}

impl<B: Tag, C: Tag> JumpIf<BlockId<B>, C> {
    #[track_caller]
    pub fn retag<B2: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_retagger: &impl BlkRetagger<B, B2>,
    ) -> JumpIf<BlockId<B2>, C2> {
        JumpIf {
            condition: retagger.retag_old(self.condition),
            if_so: self.if_so.retag(retagger, blk_retagger),
            other: self.other.retag(retagger, blk_retagger),
        }
    }

    #[track_caller]
    pub fn retag_to_blk<F: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_to_fn: &impl BlkToFn<B, F>,
    ) -> JumpIf<FunctionId<F>, C2> {
        JumpIf {
            condition: retagger.retag_old(self.condition),
            if_so: self.if_so.retag_to_blk(retagger, blk_to_fn),
            other: self.other.retag_to_blk(retagger, blk_to_fn),
        }
    }

    pub fn paths(&self) -> Vec<&BlockJump<BlockId<B>, C>> {
        vec![&self.if_so, &self.other]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<BlockId<B>, C>> {
        vec![&mut self.if_so, &mut self.other]
    }
}

impl<F: Tag, R: Tag> JumpIf<FunctionId<F>, R> {
    pub fn paths(&self) -> Vec<&BlockJump<FunctionId<F>, R>> {
        vec![&self.if_so, &self.other]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<FunctionId<F>, R>> {
        vec![&mut self.if_so, &mut self.other]
    }
}
