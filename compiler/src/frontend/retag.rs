//! Used for implementing the `retag()` feature, which allows a piece of data
//! to be tagged for usage in only a specific context. Tags prevents errors
//! regarding mismatching the registers in one phase and another. To prevent
//! more errors, a specific [`CoreRetagger`] must be used to retag the tag in
//! the first place. Depending upon the retagger used, there will be additional
//! assertions placed to ensure that data from different phases are mixed up as
//! infrequently as possible.

use std::marker::PhantomData;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::id::IdCompat;
use crate::id::RegisterId;
use crate::id::Tag;

/// A retagger specifically for registers.
pub trait RegRetagger<C: Tag, C2: Tag> {
    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn retag_new(&mut self, id: RegisterId<C>) -> RegisterId<C2>;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn retag_old(&self, id: RegisterId<C>) -> RegisterId<C2>;
}

pub struct RegPassRetagger<C: Tag, C2: Tag>(PassRetagger<RegisterId<C>, RegisterId<C2>>);

impl<A: Tag, B: Tag> Default for RegPassRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> RegRetagger<C, C2> for RegPassRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_old(id)
    }
}

pub struct RegMapRetagger<C: Tag, C2: Tag>(MapRetagger<RegisterId<C>, RegisterId<C2>>);

impl<A: Tag, B: Tag> Default for RegMapRetagger<A, B> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<C: Tag, C2: Tag> RegRetagger<C, C2> for RegMapRetagger<C, C2> {
    #[track_caller]
    fn retag_new(&mut self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_new(id)
    }

    #[track_caller]
    fn retag_old(&self, id: RegisterId<C>) -> RegisterId<C2> {
        self.0.core_retag_old(id)
    }
}

impl<C: Tag, C2: Tag> RegMapRetagger<C, C2> {
    pub fn gen(&mut self) -> RegisterId<C2> {
        self.0.core_gen()
    }
}

/// Underlying trait used to implement retagging. Additional traits are added
/// to convert between the various IDs defined. This is used to overcome rust
/// not having HKTs.
trait CoreRetagger {
    type I1: IdCompat;
    type I2: IdCompat;

    /// Used to retag something for the first time. If an element has been
    /// tagged before, this method may panic.
    #[track_caller]
    fn core_retag_new(&mut self, id: Self::I1) -> Self::I2;

    /// Used to retag something that has already been retagged. If an element
    /// has never been tagged before, this method may panic.
    #[track_caller]
    fn core_retag_old(&self, id: Self::I1) -> Self::I2;
}

/// Implements completely transparent retaggings. In release mode, this will
/// have no performance impact for retagging. However, it does not provide
/// a way to generate new IDs.
#[derive(Default)]
struct PassRetagger<I1, I2> {
    #[cfg(debug_assertions)]
    tagged: FxHashSet<usize>,
    ids: PhantomData<(I1, I2)>,
}

impl<I1: IdCompat, I2: IdCompat> CoreRetagger for PassRetagger<I1, I2> {
    type I1 = I1;
    type I2 = I2;

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(not(debug_assertions))]
    fn core_retag_old(&self, id: I1) -> I2 {
        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        if !self.tagged.insert(id.raw_value()) {
            panic!("attempted to retag new value {}, value was old", id.value());
        }

        I2::raw_new_with_value(id.raw_value())
    }

    #[track_caller]
    #[cfg(debug_assertions)]
    fn core_retag_old(&self, id: I1) -> I2 {
        if !self.tagged.contains(&id.raw_value()) {
            panic!("attempted to retag old value {}, value was new", id.value());
        }

        I2::raw_new_with_value(id.raw_value())
    }
}

/// Implements retaggings by maintaining state about what it has mapped, and to
/// what values. This gives it the ability to generate new IDs that have not
/// been mapped while mapping old values, but is not as fast as
/// [`TransparentRetagger`].
#[derive(Default)]
struct MapRetagger<I1, I2> {
    counter: usize,
    map: FxHashMap<usize, usize>,
    ids: PhantomData<(I1, I2)>,
}

impl<I1: IdCompat, I2: IdCompat> CoreRetagger for MapRetagger<I1, I2> {
    type I1 = I1;
    type I2 = I2;

    #[track_caller]
    fn core_retag_new(&mut self, id: I1) -> I2 {
        self.counter += 1;
        let old_value = self.map.insert(id.raw_value(), self.counter);

        if old_value.is_some() {
            panic!("attempted to retag new value {}, value was old", id.value());
        }

        I2::raw_new_with_value(self.counter)
    }

    #[track_caller]
    fn core_retag_old(&self, id: I1) -> I2 {
        match self.map.get(&id.raw_value()) {
            Some(id) => I2::raw_new_with_value(*id),
            None => panic!("attempted to retag old value {}, value was new", id.value()),
        }
    }
}

impl<I1, I2: IdCompat> MapRetagger<I1, I2> {
    fn core_gen(&mut self) -> I2 {
        self.counter += 1;
        I2::raw_new_with_value(self.counter)
    }
}
