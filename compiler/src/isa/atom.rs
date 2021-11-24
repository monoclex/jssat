use derive_more::Display;
use lasso::{Key, Rodeo};
use std::{convert::TryInto, num::NonZeroU16};

/// An atom represents a unique value that can be generated at compile time.
/// These are very similar to atoms in Erlang/Elixir, or Symbols in JavaScript.
/// An [`AtomDealer`] can be used to keep track of [`Atom`]s.
///
/// # Layout Guarantees
///
/// An [`Atom`] uses a [`NonZeroU16`] to ensure that the following is true:
///
/// ```
/// assert_eq!(std::mem::size_of::<Atom>(), std::mem::size_of::<Option<Atom>>());
/// ```
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Atom(pub NonZeroU16);

/// To support [`AtomDealer::gen`] method, we set a limit to the number of
/// interned atoms that exist. This is so that we can begin generating [`Atom`]s
/// after this value, and prevent interning before this value.
const MAX_INTERNED_ATOM: u16 = 0b1000_0000_0000_0000;

unsafe impl Key for Atom {
    fn into_usize(self) -> usize {
        self.0.get() as usize - 1
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        // prevent `usize::MAX + 1` from being an issue
        if int == usize::MAX {
            return None;
        }

        let int = int + 1;

        if int >= MAX_INTERNED_ATOM as usize {
            return None;
        }

        let value = int as u16;

        debug_assert_ne!(value, 0);
        debug_assert!(value < MAX_INTERNED_ATOM);

        NonZeroU16::new(value).map(Atom)
    }
}

/// Assists in ensuring unique atoms are given on request. If all requests for
/// new atoms go through an [`AtomDealer`], the API will prevent duplicate atoms
/// from being issued.
pub struct AtomDealer {
    rodeo: Rodeo<Atom>,
    gen_unused: Atom,
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

    /// Generates a unique [`Atom`] that has never been generated before.
    pub fn gen(&mut self) -> Atom {
        let return_result = self.gen_unused;

        let value = self.gen_unused.0.get();
        if value == u16::MAX {
            panic!("Reached maximum number of Atoms generatable.")
        }

        NonZeroU16::new(value + 1).expect("`value + 1` should succeed");

        return_result
    }
}

impl Default for AtomDealer {
    fn default() -> Self {
        Self {
            rodeo: Rodeo::new(),
            gen_unused: Atom(NonZeroU16::new(MAX_INTERNED_ATOM).expect("`MAX_INTERNED_ATOM` != 0")),
        }
    }
}
