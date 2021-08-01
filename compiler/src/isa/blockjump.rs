use std::fmt::Display;

use super::Registers;
use crate::{
    id::*,
    retag::{BlkRetagger, BlkToFn, RegRetagger},
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
