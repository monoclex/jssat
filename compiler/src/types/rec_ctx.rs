// TODO: mention the blog-post
// [blog-post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

use std::convert::TryInto;

use derive_more::{Deref, DerefMut, Display};
use rustc_hash::FxHashMap;
use tinyvec::{Array, ArrayVec, TinyVec};

use super::{EqualityResolver, Type, TypeCtx};
use crate::id::{RecordId, Tag, UniqueRecordId};

#[derive(Clone, Default, Deref, DerefMut, Eq)]
pub struct Record<'ctx, T: Tag> {
    unique_id: UniqueRecordId<T>,
    #[deref]
    #[deref_mut]
    items: FxHashMap<Type<'ctx, T>, Type<'ctx, T>>,
}

impl<'ctx, T: Tag> Record<'ctx, T> {
    pub fn new(unique_id: UniqueRecordId<T>) -> Self {
        Self {
            unique_id,
            items: Default::default(),
        }
    }

    pub fn unique_id(&self) -> UniqueRecordId<T> {
        self.unique_id
    }
}

impl<'ctx1, 'ctx2, T: Tag> PartialEq<Record<'ctx2, T>> for Record<'ctx1, T> {
    fn eq(&self, other: &Record<'ctx2, T>) -> bool {
        let mut eq = EqualityResolver::new();

        if eq.records_not_equal(self, other) {
            return false;
        }

        if eq.solve_constraints() {
            return false;
        }

        true
    }
}
