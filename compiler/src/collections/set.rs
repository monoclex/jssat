use ref_cast::RefCast;
use std::{hash::Hash, iter::FromIterator};

#[derive(Clone, Eq)]
pub struct Set<T> {
    set: Vec<Item<T>>,
}

impl<T> PartialEq for Set<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        // super naive & simple algorithm, otherwise i'll get stuck in the weeds
        // trying to micro optimize

        if self.set.len() != other.set.len() {
            return false;
        }

        'next_item: for item in self.set.iter() {
            for other in other.set.iter() {
                if item == other {
                    continue 'next_item;
                }
            }

            // we iterated the entire other set without finding a matching item
            return false;
        }

        true
    }
}

impl<T> Set<T> {
    pub fn new() -> Self {
        Self {
            set: Default::default(),
        }
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.set.iter().map(|i| &i.0)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.set.iter_mut().map(|i| &mut i.0)
    }

    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.set.into_iter().map(|i| i.0)
    }
}

impl<T: PartialEq> Set<T> {
    pub fn insert(&mut self, value: T) -> bool {
        let item = Item(value);
        for elem in self.set.iter() {
            if elem == &item {
                return false;
            }
        }

        self.set.push(item);
        true
    }

    pub fn contains_key(&self, value: &T) -> bool {
        self.set.contains(Item::ref_cast(value))
    }
}

impl<I: PartialEq> FromIterator<I> for Set<I> {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let mut set = Set::new();

        for item in iter {
            set.insert(item);
        }

        set
    }
}

impl<T> Default for Set<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Eq, RefCast, Clone)]
#[repr(transparent)]
struct Item<T>(T);

impl<T> Hash for Item<T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

impl<T: PartialEq> PartialEq for Item<T> {
    fn eq(&self, other: &Self) -> bool {
        (&self.0).eq(&other.0)
    }
}
