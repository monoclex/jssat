use derive_more::Display;
use std::fmt::Write;
use tinyvec::{tiny_vec, TinyVec};

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Display)]
pub enum BinaryOperator {
    Add,
    And,
    Or,
    Equals,
    LessThan,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BinOp<T: Tag> {
    pub result: RegisterId<T>,
    pub op: BinaryOperator,
    pub lhs: RegisterId<T>,
    pub rhs: RegisterId<T>,
}

impl<T: Tag> ISAInstruction<T> for BinOp<T> {
    fn declared_register(&self) -> Option<RegisterId<T>> {
        Some(self.result)
    }

    fn used_registers(&self) -> TinyVec<[RegisterId<T>; 3]> {
        tiny_vec![self.lhs, self.rhs]
    }

    fn used_registers_mut(&mut self) -> Vec<&mut RegisterId<T>> {
        vec![&mut self.lhs, &mut self.rhs]
    }

    fn display(&self, w: &mut impl Write) -> std::fmt::Result {
        write!(
            w,
            "%{} = BinOp {} %{}, %{}",
            self.result, self.op, self.lhs, self.rhs
        )
    }
}

impl<T: Tag> BinOp<T> {
    #[track_caller]
    pub fn retag<T2: Tag>(self, retagger: &mut impl RegRetagger<T, T2>) -> BinOp<T2> {
        BinOp {
            result: retagger.retag_new(self.result),
            op: self.op,
            lhs: retagger.retag_old(self.lhs),
            rhs: retagger.retag_old(self.rhs),
        }
    }
}
