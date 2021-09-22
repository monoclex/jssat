//! The JSSAT type system.
//!
//! The JSSAT compiler solves a very difficult problem: to take typeless code,
//! and automatically add types to it. The type system it uses is written in
//! these modules, and the details of the type system are reasoned about in
//! depth at the corresponding [blog post][blog-post].
//!
//! [blog-post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

use std::{cell::RefCell, hash::Hash, pin::Pin};

use bumpalo::{boxed::Box, Bump};
use ordered_float::OrderedFloat;
use rustc_hash::FxHashMap;

use crate::{
    id::{FunctionId, LiftedCtx, RecordId, RegisterId, Tag, UnionId},
    isa::TrivialItem,
};

use derive_more::Deref;
use enum_kinds::EnumKind;

// use enum_kinds::enum_kind;
use lasso::Spur;
use lazy_static::lazy_static;

mod type_relation;
pub use type_relation::TypeRelation;

mod rec_ctx;

mod assoc_eq;
mod type_ctx;
pub use type_ctx::TypeCtx;

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
#[derive(Clone, Copy, EnumKind)]
#[enum_kind(TypeKind)]
pub enum Type<'ctx, T: Tag> {
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
    /// [`Type::Byts`] represents a series of bytes of exactly the specified
    /// value. A variable of this type can only hold one possible value, which
    /// is the series of bytes specified.
    ///
    /// The [`Spur`] is used to lookup the value within the data structure that
    /// holds constants, [`CONSTANTS`].
    Byts(&'ctx Pin<Box<'ctx, [u8]>>),
    /// [`Type::Record`] represents a JSSAT record, which is a key-value mapping
    /// of JSSAT types to JSSAT types.
    ///
    /// The value contained within a [`Type::Record`] is considered the handle
    /// to a record, which is used in the appropriate data structures to look up
    /// information about the record.
    Record(&'ctx Pin<Box<'ctx, RefCell<Record<'ctx, T>>>>),
    /// [`Type::Union`] represents a possible range of different types. For
    /// example, the TypeScript type `"asdf" | 1` would be cleanly represented
    /// with a union. Unions are used as a single type to join two completely
    /// different types.
    Union(UnionId<T>),
}

impl<'c, T: Tag> Hash for Type<'c, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            // these types are "simple"
            // in that they are not references, but simply pure values
            Type::Any | Type::Bytes | Type::Number | Type::Boolean => {}
            Type::Trivial(ref inner) => inner.hash(state),
            Type::Int(ref inner) => inner.hash(state),
            Type::Float(ref inner) => inner.hash(state),
            Type::Bool(ref inner) => inner.hash(state),
            Type::FnPtr(ref inner) => inner.hash(state),
            // these types are "complicated"
            // that is because they are *references* to data stored for `'ctx`
            // rather than perform a deep hash, we simply hash the pointer
            // this is because the pointer is stable, but moreover, if we were
            // to perform a deep hash, cyclic object records could be a problem
            // (moreover, hash is not implemented for records)
            Type::Byts(inner) => {
                // TODO: unit tests to make sure the address is getting hashed or something?
                let address = Pin::get_ref(Pin::as_ref(inner)) as *const [u8];
                address.hash(state);
            }
            Type::Record(inner) => {
                let address = Pin::get_ref(Pin::as_ref(inner)) as *const RefCell<Record<'c, T>>;
                address.hash(state);
            }
            Type::Union(ref inner) => inner.hash(state),
        }
    }
}

pub use rec_ctx::Record;
