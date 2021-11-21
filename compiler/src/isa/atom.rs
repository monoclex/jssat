use derive_more::Display;
use lasso::{Key, Rodeo};
use std::{convert::TryInto, num::NonZeroU16};

/// An atom represents a unique value that can be generated at compile time.
/// These are very similar to atoms in Erlang/Elixir, or Symbols in JavaScript.
/// An [`AtomDealer`] can be used to keep track of [`Atom`]s.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Atom(pub NonZeroU16);

impl Atom {
    fn next(self) -> Self {
        let next = (self.0.checked_add(1)).expect("shouldn't overflow in our lifetimes");
        Self(next)
    }
}

unsafe impl Key for Atom {
    fn into_usize(self) -> usize {
        self.0.get() as usize - 1
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        let u16: Option<u16> = (int + 1).try_into().ok();
        u16.and_then(|u16| u16.try_into().ok()).map(Atom)
    }
}

/// Assists in ensuring unique atoms are given on request. If all requests for
/// new atoms go through an [`AtomDealer`], the API will prevent duplicate atoms
/// from being issued.
pub struct AtomDealer {
    rodeo: Rodeo<Atom>,
}

impl AtomDealer {
    /// Creates an instance of an [`AtomDealer`].
    pub fn new() -> Self {
        Default::default()
    }

    /// Issues a unique [`Atom`] that corresponds to only the string given.
    pub fn deal(&mut self, name: &'static str) -> Atom {
        self.rodeo.get_or_intern_static(name)
    }
}

impl Default for AtomDealer {
    fn default() -> Self {
        Self {
            rodeo: Rodeo::new(),
        }
    }
}
