//! The JSSAT type system.
//!
//! The JSSAT compiler solves a very difficult problem: to take typeless code,
//! and automatically add types to it. The type system it uses is written in
//! these modules, and the details of the type system are reasoned about in
//! depth at the corresponding [blog post][blog-post].
//!
//! [blog-post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

use ordered_float::OrderedFloat;
use rustc_hash::{FxHashMap, FxHashSet};
use std::convert::TryFrom;

use crate::{
    id::{AllocationId, FunctionId, LiftedCtx, RecordId, RegisterId, SymbolicCtx, Tag, UnionId},
    isa::TrivialItem,
};

use derive_more::Deref;
use enum_kinds::EnumKind;

// use enum_kinds::enum_kind;
use lasso::Spur;
use lazy_static::lazy_static;

mod type_relation;
pub use type_relation::TypeRelation;

mod assoc_eq;
mod type_ctx;

lazy_static! {
    static ref CONSTANTS: lasso::ThreadedRodeo = lasso::ThreadedRodeo::default();
}

fn resolve_spur(spur: &Spur) -> &[u8] {
    let result = CONSTANTS.resolve(spur);
    result.as_bytes()
}

/// The JSSAT [`Type`] used to represent the type of a value.
///
/// Types in a type system are a way to represent the possible values a given
/// variable can have. The types possible to model in JSSAT exist to accurately
/// model JSSAT IR, which in turn aims to model dynamically typed programming
/// languages as best as possible.
///
/// It is not possible to compare for equality between types. To do so requires
/// more information than stored within the [`Type`], such as when comparing
/// records or unions. Usage of a [`TypeCtx`] is required to compare for deep
/// equality.
///
/// Determining if a [`Type`] is a subtype or supertype is doable via the
/// [`Type::is_substitutable_by`] method.
#[derive(Clone, Debug, Hash, EnumKind)]
#[enum_kind(TypeKind)]
pub enum Type<T: Tag> {
    /// [`Type::Any`] represents any possible value in JSSAT, as a catch-all.
    ///
    /// Since [`Type::Any`] represents all possible values, the least amount of
    /// information can be assumed from it, and thus the generated code for
    /// values of type [`Type::Any`] is guaranteed to be poor because of
    /// lack of information.
    ///
    /// The only times a [`Type::Any`] could be produced is either when an
    /// external function produces an [`Type::Any`], or when a variable is
    /// generalized into an [`Type::Any`].
    ///
    /// [`Type::Any`]s must be downcasted to another type before using them,
    /// otherwise the abstract interpreter will produce an error.
    Any,
    /// [`Type::Bytes`] represents any series of bytes, and is often used to
    /// model strings efficiently. The [`Type::Byts`] type represents an exact
    /// value of a string.
    Bytes,
    /// [`Type::Number`] means a number at the most general level, which could
    /// include integers of varying sizes or floats.
    Number,
    /// [`Type::Boolean`] represents a variable which can hold either `true` or
    /// `false`.
    ///
    /// Booleans are used when when branching, and because it is not
    /// known whether the variable is `true` or `false`, the abstract
    /// interpreter must take both paths when trying to decide which path to
    /// take for a [`Type::Boolean`].
    Boolean,
    /// [`Type::Trivial`] represents a variable with a symbolic value. This is
    /// used to represent `null` and `undefined`, and is also used to represent
    /// any other sentinals.
    Trivial(TrivialItem),
    /// [`Type::Byts`] represents a series of bytes of exactly the specified
    /// value. A variable of this type can only hold one possible value, which
    /// is the series of bytes specified.
    ///
    /// The [`Spur`] is used to lookup the value within the data structure that
    /// holds constants, [`CONSTANTS`].
    Byts(Spur),
    /// [`Type::Int`] represents an integer of exactly the specified value. A
    /// variable of this type can only hold one possible value, which is the
    /// integer specified.
    Int(i64),
    /// [`Type::Float`] represents a floating point number of exactly the
    /// specified value. A variable of this type can only hold one possible
    /// value, which is the float specified.
    ///
    /// An [`OrderedFloat`] is used to facilitate usage of a [`Type`] within
    /// datastructures that require hashing, such as [`FxHashMap`]s or
    /// [`FxHashSet`]s.
    Float(OrderedFloat<f64>),
    /// [`Type::Bool`] represents a boolean of exactly the specified value. A
    /// variable of this type can only hold one possible value, which is the
    /// boolean specified.
    ///
    /// Since [`Type::Bool`] represents exactly either `true` or `false`, the
    /// abstract interpreter can determine that exactly one path needs to be
    /// taken when evaluating code, which makes for a significant decrease in
    /// the amount of work that needs to be done.
    Bool(bool),
    /// [`Type::FnPtr`] represents a handle to a function.
    ///
    /// Function pointers are often used in conjunction with virtual calls to
    /// implement dynamic dispatch. Since the abstract interpreter executes the
    /// program in its entirety, it is capable of converting all virtual calls
    /// into static calls to prevent dynamic dispatch.
    ///
    /// Function pointers are often tagged as being in the [`LiftedCtx`] phase
    /// due to it being impossible to attribute a specific function to a
    /// function pointer in all cases. This is because two virtual calls to the
    /// same function pointer may supply different argument types.
    FnPtr(FunctionId<LiftedCtx>),
    /// [`Type::Record`] represents a JSSAT record, which is a key-value mapping
    /// of JSSAT types to JSSAT types.
    ///
    /// The value contained within a [`Type::Record`] is considered the handle
    /// to a record, which is used in the appropriate data structures to look up
    /// information about the record.
    Record(RecordId<T>),
    /// [`Type::Union`] represents a possible range of different types. For
    /// example, the TypeScript type `"asdf" | 1` would be cleanly represented
    /// with a union. Unions are used as a single type to join two completely
    /// different types.
    Union(UnionId<T>),
}

/// Datastructure used to keep track of the types of registers.
///
/// By associating types with a [`TypeCtx`], it also provides more
/// functionality, such as:
///
/// - Equality comparisons between types
/// - Transferring types between [`TypeCtx`]s
/// - Comparing if [`TypeCtx`]s are equal (used to prevent re-executing a
///   function during abstract interpretation)
#[derive(Clone)]
pub struct TypeCtx<T: Tag> {
    registers: FxHashMap<RegisterId<T>, Type<T>>,
}

/// Represents a type associated with a [`TypeCtx`].
///
/// When a type is associated with a [`TypeCtx`], it provides more capabilities
/// to the type, such as displaying the type or equality comparisons.
#[derive(Clone, Deref)]
pub struct AssociatedType<'ctx, T: Tag>(#[deref] &'ctx Type<T>, &'ctx TypeCtx<T>);
