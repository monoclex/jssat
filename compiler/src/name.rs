type StringValue = Option<Box<str>>;

// this documentation is outdated but being left here for reference
// -- /// # Name
// -- ///
// -- /// Parameters that accept a [`Name`] were considered to just accept a
// -- /// [`Option<Box<str>>`] directly, so that users could succinctly type [`None`].
// -- /// However, this was decided against as it could lead users into the pitfall
// -- /// of typing out a lengthy `Some("asdf".to_string().into_boxed_str())`, which
// -- /// is exactly the pitfall requiring a [`Name`] is designed to prevent.
// -- ///
// -- /// Thus, we believe users will opt to type `Name::` and wait for intellisense.
// -- /// This will guide them into the happy path of typing `Name::new("name")`, or
// -- /// `Name::none()` for no names.
#[derive(Debug, Clone)]
pub struct Name(StringValue);

impl Name {
    pub fn none() -> Self {
        Name(None)
    }

    /// Creates a name, given a [`String`]. To create a name without an
    /// identifying value, consider using [`Name::none`].
    pub fn new<S: ToString>(name: S) -> Self {
        Self(Some(name.to_string().into_boxed_str()))
    }

    pub fn value(&self) -> Option<&str> {
        self.0.as_deref()
    }
}

// in debug mode, store a `Name` for a `DebugName`. we don't use type aliasing
// so that we can't possibly make the mistake of passing a `Name` for `DebugName`,
// which would then fail to compile in release mode
//
// TODO: in the IR, we store the debug name and where the thing is from in the
// frontend source code. we could probably take out this functionality and make
// it more genreic

#[cfg(debug_assertions)]
#[derive(Debug, Clone)]
pub struct DebugName(Name);

#[cfg(debug_assertions)]
impl DebugName {
    pub fn none() -> Self {
        DebugName(Name::none())
    }

    pub fn new<S: ToString>(name: S) -> Self {
        DebugName(Name::new(name))
    }

    pub fn value(&self) -> Option<&str> {
        self.0.value()
    }
}

#[cfg(debug_assertions)]
impl Into<Name> for DebugName {
    fn into(self) -> Name {
        self.0
    }
}

// in release mode, utilize ZSTs so that we don't store any debug information

#[cfg(not(debug_assertions))]
#[derive(Debug, Clone)]
pub struct DebugName;

#[cfg(not(debug_assertions))]
impl DebugName {
    pub fn none() -> Self {
        DebugName {}
    }

    pub fn new<S: ToString>(_thing: S) -> Self {
        DebugName {}
    }

    pub fn value(&self) -> Option<&str> {
        None
    }
}

#[cfg(not(debug_assertions))]
impl Into<Name> for DebugName {
    fn into(self) -> Name {
        Name::none()
    }
}
