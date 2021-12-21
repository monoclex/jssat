//! The JSSAT type system.
//!
//! The JSSAT compiler solves a very difficult problem: to take typeless code,
//! and automatically add types to it. The type system it uses is written in
//! these modules, and the details of the type system are reasoned about in
//! depth at the corresponding [blog post][blog-post].
// TODO: only reference the blog post for records? idk
//! [blog-post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

#![allow(warnings)]

use core::slice::SlicePattern;
use std::{cell::RefCell, fmt::Debug, hash::Hash, pin::Pin};

use bumpalo::{boxed::Box, Bump};
use ordered_float::OrderedFloat;

use crate::{
    id::{FunctionId, LiftedCtx, Tag, UnionId},
    isa::Atom,
};

use derive_more::{Deref, DerefMut};
use enum_kinds::EnumKind;

mod type_relation;
pub use type_relation::TypeRelation;

mod rec_ctx;
pub use rec_ctx::Record;

mod union;
pub use union::Union;

mod assoc_eq;
mod type_ctx;
pub use type_ctx::{TypeCtx, TypeDuplication};

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
///
/// [`Type`]s implement the [`PartialEq`] and [`Eq`] trait for equality.
/// However, this only performs a shallow equality comparison. Two types may
/// have the same values, but if they are not exactly the same type within the
/// same [`TypeCtx`], this equality comparison will fail. Use the
// TODO: make `Type::deep_eq`
/// [`Type::deep_eq`] method to check for deep equality.
#[derive(Clone, Copy, Debug, EnumKind, Hash, PartialEq, Eq)]
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
    /// [`Type::Nothing`] does not represent anything at all. It is, by
    /// definition, purely nothing. Observing a value of [`Type::Nothing`] is an
    /// error.
    ///
    /// Attempts to use [`Type::Nothing`] will result in errors. It exists to
    /// simplify checking if a record has a value present at a key or not.
    // TODO: this section could probably be removed
    /// A single instruction can be emitted to ensure that the value is not
    /// nothing, and it will often be more performant as there is no preliminary
    /// "does it exists" followed by a "get the value" check, which could
    /// potentially involve two hashes and lookups for a single field.
    Nothing,
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
    /// [`Type::Atom`] represents a unique symbolic value. These are often used
    /// to represent unique values (such as `null` or `undefined`), and are
    /// usable as the keys of records. Using an [`Atom`] as a record key is
    /// often preferable to a string, as it is impossible to produce a duplicate
    /// [`Atom`] from within JSSAT IR unintentionally.
    Atom(Atom),
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
    /// Function pointers are tagged as being in the [`LiftedCtx`] phase due to
    /// it being impossible to attribute a specific monomorphized version of a
    /// function to a function pointer in all cases. This is because two virtual
    /// calls to the same function pointer may supply different argument types,
    /// requiring a call to two different monomorphized versions of that
    /// function.
    FnPtr(FunctionId<LiftedCtx>),
    /// [`Type::Byts`] represents a series of bytes of exactly the specified
    /// value. A variable of this type can only hold one possible value, which
    /// is the series of bytes specified.
    ///
    /// This type is difficult to construct manually. To construct one, use a
    /// [`TypeCtx`], call the `borrow` method to borrow it immutably, then call
    /// `make_bytes`. See `make_type_byts` on [`type_ctx::TypeCtxImmut`].
    Byts(BytsHandle<'ctx>),
    /// [`Type::Record`] represents a JSSAT record, which is a key-value mapping
    /// of JSSAT types to JSSAT types.
    ///
    /// The value contained within a [`Type::Record`] is considered the handle
    /// to a record, which is used in the appropriate data structures to look up
    /// information about the record.
    ///
    /// This type is difficult to construct manually. To construct one, use a
    /// [`TypeCtx`], call the `borrow` method to borrow it immutably, then call
    /// `make_record`. See `make_type_record` on [`type_ctx::TypeCtxImmut`].
    Record(RecordHandle<'ctx, T>),
    /// [`Type::Union`] represents a possible range of different types. For
    /// example, the TypeScript type `"asdf" | 1` would be cleanly represented
    /// with a union. Unions are used as a single type to join two completely
    /// different types.
    Union(UnionHandle<'ctx, T>),
}

impl<'ctx, T: Tag> Type<'ctx, T> {
    pub fn try_into_record(self) -> Option<RecordHandle<'ctx, T>> {
        match self {
            Type::Record(handle) => Some(handle),
            _ => None,
        }
    }

    pub fn unwrap_record(&self) -> RecordHandle<'ctx, T> {
        self.try_into_record().unwrap()
    }

    pub fn try_into_union(self) -> Option<UnionHandle<'ctx, T>> {
        match self {
            Type::Union(handle) => Some(handle),
            _ => None,
        }
    }

    pub fn unwrap_union(&self) -> UnionHandle<'ctx, T> {
        self.try_into_union().unwrap()
    }
}

/// The handle to some bytes. Conceptually, this type can be thought of as a
/// `BytsHandle(&mut [u8])`, but the [`CtxBox`] is used to achieve interior
/// mutability.
#[repr(transparent)]
#[derive(Clone, Copy, Hash, Deref, DerefMut, PartialEq, Eq)]
pub struct BytsHandle<'ctx>(#[deref] CtxBox<'ctx, [u8]>);

impl<'ctx> Debug for BytsHandle<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Byts")
            .field(&{
                let bytes = self.as_slice();

                // TODO: trim constant size
                match std::str::from_utf8(bytes) {
                    Ok(str) => format!("{:?}", str),
                    Err(_) => format!("{:02X?}", bytes),
                }
            })
            .finish()
    }
}

/// The handle to a record. Conceptually, this type can be thought of as a
/// `RecordHandle(&mut Record)`, but the [`CtxBox`] is used to achieve interior
/// mutability.
#[repr(transparent)]
#[derive(Clone, Copy, Hash, Deref, DerefMut, PartialEq, Eq)]
pub struct RecordHandle<'ctx, T: Tag>(#[deref] CtxBox<'ctx, RefCell<Record<'ctx, T>>>);

impl<'ctx, T: Tag> Debug for RecordHandle<'ctx, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = self.borrow().unique_id();
        f.debug_struct("Record").field("id", &id).finish()
    }
}

/// The handle to a union. Conceptually, this type can be thought of as a
/// `UnionHandle(&mut Union)`, but the [`CtxBox`] is used to achieve interior
/// mutability.
#[repr(transparent)]
#[derive(Clone, Copy, Hash, Deref, DerefMut, PartialEq, Eq)]
pub struct UnionHandle<'ctx, T: Tag>(#[deref] CtxBox<'ctx, RefCell<Union<'ctx, T>>>);

impl<'ctx, T: Tag> Debug for UnionHandle<'ctx, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = self.borrow().unique_id();
        f.debug_struct("Union").field("id", &id).finish()
    }
}

/// A heap allocated `T`, associated with a context `'ctx`, with a stable
/// address, found within the `'ctx`, which implements [`Copy`] and [`Hash`] for
/// all `T`s.
///
/// # Explanation
///
/// Below are the components that comprise of a [`CtxBox`]
///
/// ```text
///   1    2        3
/// /---\ /-\ /----------\
/// &'ctx Pin<Box<'ctx, T>>
/// ```
///
/// ## 1. Reference that lives in `&'ctx`
///
/// This enables [`CtxBox`] to be used as [`Copy`]. Since the `T` must be
/// already valid for `'ctx`, it is not difficult to require a `&'ctx`. This is
/// easily possible as the [`bumpalo::Bump`] arena will allow allocation into it
/// for arbitrary types, such as a [`Pin`].
///
/// ## 2. Data pinning
///
/// By pinning the data, we are being guaranteed that the underlying address is
/// stable. Although it is not strictly correct to implement [`Hash`], it is
/// being done here as the address is being considered the "id" of something,
/// that there should only be one unique version of this type.
///
/// ## 3. [`Box`]ed data
///
/// It is debatable whether or not [`Box`] is necessary, but it allows for easy
/// usage of the [`Box::pin_in`] API. In addition, the destructors of `T` will
/// run properly, even though this is not strictly necessary.
#[repr(transparent)]
#[derive(Deref, DerefMut)]
pub struct CtxBox<'ctx, T: ?Sized>(#[deref] &'ctx Pin<Box<'ctx, T>>);

impl<'ctx, T> CtxBox<'ctx, T> {
    pub fn new(arena: &'ctx Bump, data: T) -> Self {
        CtxBox(arena.alloc(Box::pin_in(data, arena)))
    }
}

impl<'ctx, T: ?Sized + Unpin> CtxBox<'ctx, T> {
    pub fn new_unsized(arena: &'ctx Bump, data: Box<'ctx, T>) -> Self {
        CtxBox(arena.alloc(Pin::new(data)))
    }
}

impl<'ctx, T: ?Sized> Copy for CtxBox<'ctx, T> {}
impl<'ctx, T: ?Sized> Clone for CtxBox<'ctx, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'ctx, T: ?Sized> Hash for CtxBox<'ctx, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let ptr = self.0.as_ref().get_ref() as *const T;
        ptr.hash(state)
    }
}

impl<'ctx, T: ?Sized> Eq for CtxBox<'ctx, T> {}
impl<'ctx, T: ?Sized> PartialEq for CtxBox<'ctx, T> {
    fn eq(&self, other: &Self) -> bool {
        let ptr = self.0.as_ref().get_ref() as *const T;
        let ptr2 = other.0.as_ref().get_ref() as *const T;
        std::ptr::eq(ptr, ptr2)
    }
}
