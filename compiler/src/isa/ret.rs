use std::fmt::Write;
use tinyvec::TinyVec;

use super::ISAInstruction;
use crate::{id::*, retag::RegRetagger};

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
        match self.0 {
            Some(r) => TinyVec::from([r, Default::default(), Default::default()]),
            None => TinyVec::new(),
        }
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
