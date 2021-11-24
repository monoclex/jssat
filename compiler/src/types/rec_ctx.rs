// TODO: mention the blog-post
// [blog-post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

use std::convert::TryInto;

use derive_more::{Deref, Display};
use rustc_hash::FxHashMap;
use tinyvec::{Array, ArrayVec, TinyVec};

use super::{Type, TypeCtx};
use crate::id::{RecordId, Tag, UniqueRecordId};

#[derive(Clone, Default)]
pub struct Record<'ctx, T: Tag> {
    unique_id: UniqueRecordId<T>,
    facts: TinyVec<[Facts<'ctx, T>; 1]>,
}

#[derive(Clone)]
pub struct Facts<'ctx, T: Tag> {
    facts: Vec<Fact<'ctx, T>>,
}

impl<'ctx, T: Tag> Default for Facts<'ctx, T> {
    fn default() -> Self {
        Self {
            facts: Default::default(),
        }
    }
}

#[derive(Clone, Copy, Hash)]
struct RecordKey<'ctx, T: Tag>(Type<'ctx, T>);

/// we need a number to determine when a fact was added so that optimization
/// passes acting on the list of facts can know at the instruction it's looking
/// at if a fact is true or false
///
/// when calling functions, each record in the typebag has an initial fact set
/// up so that's represented by `prologue`, and during execution if types change
/// *after* an instruction that's represented with `inst`
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Display)]
pub enum InstIdx {
    Prologue,
    #[display(fmt = "{0}", _0)]
    Inst(usize),
    /// only needed as a hack really but oh well
    Epilogue,
}

#[derive(Clone, Copy, Hash)]
pub struct Fact<'ctx, T: Tag> {
    time: InstIdx,
    key: RecordKey<'ctx, T>,
    data: FactData<'ctx, T>,
}

#[derive(Clone, Copy, Hash)]
pub enum FactData<'ctx, T: Tag> {
    Set(Type<'ctx, T>),
    Remove,
}

impl<'ctx, T: Tag> Record<'ctx, T> {
    pub fn new(unique_id: UniqueRecordId<T>) -> Self {
        Self {
            unique_id,
            facts: TinyVec::Inline(ArrayVec::from([Facts::default()])),
        }
    }

    pub fn unique_id(&self) -> UniqueRecordId<T> {
        self.unique_id
    }

    pub fn facts(&self) -> impl Iterator<Item = &Facts<'ctx, T>> {
        self.facts.iter()
    }

    pub fn facts_mut(&mut self) -> impl Iterator<Item = &mut Facts<'ctx, T>> {
        self.facts.iter_mut()
    }

    pub fn key_value_pairs(&self) -> Vec<(String, &Type<'ctx, T>)> {
        todo!()
    }
}
