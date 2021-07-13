use rustc_hash::{FxHashMap, FxHasher};
use std::hash::{Hash, Hasher};

// TODO: maybe there's a better way to implement a generic interner?
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InternedIdx(usize);

pub struct Interner<T> {
    // the bad thing about this strategy is that it's a bit cache inefficient but oh well
    map: FxHashMap<u64, Vec<usize>>,
    elements: Vec<T>,
}

impl<T: Hash + Eq> Interner<T> {
    pub fn intern(&mut self, item: T) -> InternedIdx {
        let bucket_idx = Self::hash(&item);
        let bucket = self.map.entry(bucket_idx).or_insert_with(Default::default);

        for idx in bucket.iter().copied() {
            debug_assert!(
                self.elements.len() > idx,
                "idx in bucket should exist in `elements`"
            );
            let element = self.elements.get(idx).unwrap();

            debug_assert_eq!(
                Self::hash(element),
                Self::hash(&item),
                "hashes of elements should be equal"
            );
            if element == &item {
                return InternedIdx(idx);
            }
        }

        let idx = self.elements.len();
        self.elements.push(item);
        bucket.push(idx);
        InternedIdx(idx)
    }

    pub fn data(&self, idx: InternedIdx) -> &T {
        let idx = idx.0 as usize;
        debug_assert!(idx < self.elements.len());
        &self.elements[idx]
    }

    fn hash(item: &T) -> u64 {
        let mut hasher = FxHasher::default();
        item.hash(&mut hasher);
        hasher.finish()
    }
}

impl<T> Default for Interner<T> {
    fn default() -> Self {
        Self {
            map: Default::default(),
            elements: Default::default(),
        }
    }
}
