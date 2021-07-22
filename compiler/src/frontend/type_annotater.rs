use ref_cast::RefCast;
use rustc_hash::FxHashMap;
use std::hash::Hash;

use crate::frontend::ir::*;
use crate::id::*;
use crate::poor_hashmap::PoorMap;

use super::old_types::RegMap;

// output data structures

// symbolic execution engine

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
    Void,
    Value(ValueType),
    /// # [`ValueType::Never`]
    ///
    /// The type assigned to a function when it recurses to infinity, with no
    /// end in sight.
    Never,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueType {
    /// # `Any`
    ///
    /// The `Any` type in JSSAT is used a a polymorphic "catch-all" for when
    /// the type system cannot figure something out.
    ///
    /// Narrowing an `Any` into a more specific type when it's not possible to
    /// do so results in runtime errors. This feature of the `Any` type allows
    /// us to compile all user provided code into an output, even if the code
    /// given should be considered a compiler error.
    ///
    /// The `Any` type is the most generic type possible for all values. Any
    /// JSSAT RT value can be cast into an `Any`, besides exotic primitives,
    // TODO: is `Reference`/`Pointer` the finalized name?
    /// such as a `Runtime` or `Reference`/`Pointer`.
    ///
    /// A hierarchy of JSSAT RT types is shown below:
    ///
    /// - [`ValueType::Any`]
    ///   - [`ValueType::String`]
    ///     - [`ValueType::ExactString`]
    Any,
    Runtime,
    String,
    // TODO: an "ExactString" should just be a String with some kind of
    // ExactnessGuarantee to be exactly a type of a constant
    ExactString(Vec<u8>),
    Number,
    ExactInteger(i64),
    Boolean,
    Bool(bool),
    /// A record. The ID present inside of the object is the allocation id. The
    /// allocation id is then linked to a table of allocation IDs to the
    Record(AllocationId<NoContext>),
    FnPtr(BlockId<PureBbCtx>),
    Null,
    Undefined,
}
