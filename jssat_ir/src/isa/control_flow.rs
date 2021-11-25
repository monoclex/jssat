use std::fmt::{Display, Write};
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::{
    id::*,
    isa::Registers,
    retag::{BlkRetagger, BlkToFn, FnRetagger, RegRetagger},
};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct BlockJump<B: IdCompat, C: Tag>(pub B, pub Vec<RegisterId<C>>);

impl<B: Tag, C: Tag> BlockJump<BlockId<B>, C> {
    #[track_caller]
    pub fn retag<B2: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_retagger: &impl BlkRetagger<B, B2>,
    ) -> BlockJump<BlockId<B2>, C2> {
        BlockJump(blk_retagger.retag_old(self.0), retagger.retag_olds(self.1))
    }

    #[track_caller]
    pub fn retag_to_blk<F: Tag, C2: Tag>(
        self,
        retagger: &impl RegRetagger<C, C2>,
        blk_to_fn: &impl BlkToFn<B, F>,
    ) -> BlockJump<FunctionId<F>, C2> {
        BlockJump(blk_to_fn.retag(self.0), retagger.retag_olds(self.1))
    }
}

impl<F: Tag, R: Tag> BlockJump<FunctionId<F>, R> {
    #[track_caller]
    pub fn retag<F2: Tag, R2: Tag>(
        self,
        retagger: &impl RegRetagger<R, R2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> BlockJump<FunctionId<F2>, R2> {
        BlockJump(fn_retagger.retag_old(self.0), retagger.retag_olds(self.1))
    }
}

impl<B: Tag, R: Tag> Display for BlockJump<BlockId<B>, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}({})", self.0, Registers(&self.1))
    }
}

impl<F: Tag, R: Tag> Display for BlockJump<FunctionId<F>, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}({})", self.0, Registers(&self.1))
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Return<C: Tag>(pub Option<RegisterId<C>>);

impl<C: Tag> ISAInstruction<C> for Return<C> {
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

        if let Some(r) = self.0 {
            used_registers.push(r)
        }

        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        match &mut self.0 {
            Some(r) => vec![r],
            None => Vec::new(),
        }
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        match self.0 {
            Some(r) => write!(w, "Return %{};", r),
            None => write!(w, "Return;"),
        }
    }
}

impl<C: Tag> Return<C> {
    #[track_caller]
    #[allow(clippy::manual_map)] // can't use closures because then `track_caller` doesn't work
    pub fn retag<C2: Tag>(self, retagger: &impl RegRetagger<C, C2>) -> Return<C2> {
        Return(match self.0 {
            Some(r) => Some(retagger.retag_old(r)),
            None => None,
        })
    }
}

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
    #[track_caller]
    pub fn retag<F2: Tag, R2: Tag>(
        self,
        retagger: &impl RegRetagger<R, R2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> Jump<FunctionId<F2>, R2> {
        Jump(self.0.retag(retagger, fn_retagger))
    }

    pub fn paths(&self) -> Vec<&BlockJump<FunctionId<F>, R>> {
        vec![&self.0]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<FunctionId<F>, R>> {
        vec![&mut self.0]
    }
}

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
        used_registers.push(self.condition);
        used_registers.extend_from_slice(&self.if_so.1);
        used_registers.extend_from_slice(&self.other.1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.condition];
        used_registers.extend(self.if_so.1.iter_mut());
        used_registers.extend(self.other.1.iter_mut());
        used_registers
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
        used_registers.push(self.condition);
        used_registers.extend_from_slice(&self.if_so.1);
        used_registers.extend_from_slice(&self.other.1);
        used_registers
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<C>> {
        let mut used_registers = vec![&mut self.condition];
        used_registers.extend(self.if_so.1.iter_mut());
        used_registers.extend(self.other.1.iter_mut());
        used_registers
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
    #[track_caller]
    pub fn retag<F2: Tag, R2: Tag>(
        self,
        retagger: &impl RegRetagger<R, R2>,
        fn_retagger: &impl FnRetagger<F, F2>,
    ) -> JumpIf<FunctionId<F2>, R2> {
        JumpIf {
            condition: retagger.retag_old(self.condition),
            if_so: self.if_so.retag(retagger, fn_retagger),
            other: self.other.retag(retagger, fn_retagger),
        }
    }

    pub fn paths(&self) -> Vec<&BlockJump<FunctionId<F>, R>> {
        vec![&self.if_so, &self.other]
    }

    pub fn paths_mut(&mut self) -> Vec<&mut BlockJump<FunctionId<F>, R>> {
        vec![&mut self.if_so, &mut self.other]
    }
}

impl<B: IdCompat, T: Tag> JumpIf<B, T> {
    pub fn take_path(&self, condition: bool) -> &BlockJump<B, T> {
        match condition {
            true => &self.if_so,
            false => &self.other,
        }
    }

    pub fn take_path_mut(&mut self, condition: bool) -> &BlockJump<B, T> {
        match condition {
            true => &mut self.if_so,
            false => &mut self.other,
        }
    }
}
