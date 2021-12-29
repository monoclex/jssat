use derive_more::{Deref, DerefMut};
use jssat_ir::id::{Tag, UniqueListId};

use super::Type;

// TODO: use an enum
// pub enum List { KnownSize, VariableLength }

#[derive(Clone, Default, Deref, DerefMut, Eq)]
pub struct List<'ctx, T: Tag> {
    unique_id: UniqueListId<T>,
    #[deref]
    #[deref_mut]
    items: Vec<Type<'ctx, T>>,
}

impl<'ctx, T: Tag> List<'ctx, T> {
    pub fn new(unique_id: UniqueListId<T>) -> Self {
        Self {
            unique_id,
            items: Default::default(),
        }
    }

    pub fn unique_id(&self) -> UniqueListId<T> {
        self.unique_id
    }
}

impl<'ctx1, 'ctx2, T: Tag> PartialEq<List<'ctx2, T>> for List<'ctx1, T> {
    fn eq(&self, other: &List<'ctx2, T>) -> bool {
        self.unique_id == other.unique_id && self.items == other.items
    }
}
