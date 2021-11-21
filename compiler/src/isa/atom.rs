use derive_more::Display;
use std::num::NonZeroU16;

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

/// Assists in ensuring unique atoms are given on request. If all requests for
/// new atoms go through an [`AtomDealer`], the API will prevent duplicate atoms
/// from being issued.
#[derive(Clone, Copy)]
pub struct AtomDealer {
    current: Atom,
}

impl AtomDealer {
    /// Creates an instance of an [`AtomDealer`].
    pub fn new() -> Self {
        Default::default()
    }

    /// Issues a unique [`Atom`] that this dealer has not given out before.
    pub fn deal(&mut self) -> Atom {
        let result = self.current;
        self.current = self.current.next();
        result
    }
}

impl Default for AtomDealer {
    fn default() -> Self {
        Self {
            current: Atom(NonZeroU16::new(1).unwrap()),
        }
    }
}
