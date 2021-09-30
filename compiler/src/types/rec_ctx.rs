use std::convert::TryInto;

use derive_more::{Deref, Display};
use rustc_hash::FxHashMap;

use super::{Type, TypeCtx};
use crate::{
    id::{RecordId, Tag, UniqueRecordId},
    isa::InternalSlot,
};

#[derive(Clone)]
pub struct Record<'ctx, T: Tag> {
    unique_id: UniqueRecordId<T>,
    facts: Vec<Type<'ctx, T>>,
}

#[derive(Clone, Copy, Hash)]
enum RecordKey<'ctx, T: Tag> {
    Key(Type<'ctx, T>),
    Slot(InternalSlot),
}

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

pub struct Fact<'ctx, T: Tag> {
    time: InstIdx,
    key: RecordKey<'ctx, T>,
    data: FactData<'ctx, T>,
}

pub enum FactData<'ctx, T: Tag> {
    Set(Type<'ctx, T>),
    Remove,
}

impl<'ctx, T: Tag> Record<'ctx, T> {
    pub fn new(unique_id: UniqueRecordId<T>) -> Self {
        Self {
            unique_id,
            facts: Vec::new(),
        }
    }

    pub fn unique_id(&self) -> UniqueRecordId<T> {
        self.unique_id
    }

    pub fn key_value_pairs(&self) -> Vec<(String, &Type<'ctx, T>)> {
        todo!()
    }
}
