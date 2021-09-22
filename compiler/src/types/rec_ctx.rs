use std::convert::TryInto;

use derive_more::Deref;
use rustc_hash::FxHashMap;

use super::{Type, TypeCtx};
use crate::id::{RecordId, Tag};

impl<'ctx, T: Tag> TryInto<Record<'ctx, T>> for Type<'ctx, T> {
    type Error = ();

    fn try_into(self) -> Result<Record<'ctx, T>, Self::Error> {
        todo!()
    }
}

#[derive(Clone)]
pub struct Record<'ctx, T: Tag> {
    facts: Vec<Type<'ctx, T>>,
}
