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
use std::cell::{Ref, RefCell, RefMut};
use std::{fmt::Debug, hash::Hash, num::NonZeroU16, ops::DerefMut, pin::Pin};

use bumpalo::{boxed::Box, Bump};
use derivative::Derivative;
use ordered_float::OrderedFloat;

use crate::{
    id::{FunctionId, LiftedCtx, Tag, UnionId},
    isa::Atom,
};

use derive_more::{Deref, DerefMut};
use enum_kinds::EnumKind;

mod type_relation;
pub use type_relation::TypeRelation;

mod list;
pub use list::List;

mod rec_ctx;
pub use rec_ctx::Record;

mod union;
pub use union::Union;

mod assoc_eq;
pub use assoc_eq::{EqualityResolver, MaybeEqual};

mod type_ctx;
pub use type_ctx::{TypeCtx, TypeCtxImmut, TypeCtxMut, TypeDuplication};

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
#[derive(Clone, Copy, Debug, EnumKind, Hash, Eq)]
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
    /// [`Type::List`] represents a JSSAT list, which is a contiguous sequential
    /// collection of JSSAT types.
    ///
    /// A list can be fully emulated by a [`Type::Record`], but for performance
    /// reasons, it is more efficient to model lists as lists when possible.
    ///
    /// This type is difficult to construct manually. To construct one, use a
    /// [`TypeCtx`], call the `borrow` method to borrow it immutably, then call
    /// `make_list`. See `make_type_list` on [`type_ctx::TypeCtxImmut`].
    List(ListHandle<'ctx, T>),
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
    ///
    /// This type is difficult to construct manually. To construct one, use a
    /// [`TypeCtx`], call the `borrow` method to borrow it immutably, then call
    /// `make_union`. See `make_type_union` on [`type_ctx::TypeCtxImmut`].
    Union(UnionHandle<'ctx, T>),
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, Eq)]
pub struct TypeRefEq<'ctx, T: Tag>(pub Type<'ctx, T>);

impl<'ctx, T: Tag> PartialEq for TypeRefEq<'ctx, T> {
    fn eq(&self, other: &Self) -> bool {
        match (self.0, other.0) {
            (Type::Any, Type::Any)
            | (Type::Nothing, Type::Nothing)
            | (Type::Bytes, Type::Bytes)
            | (Type::Number, Type::Number)
            | (Type::Boolean, Type::Boolean) => true,
            (Type::Atom(l0), Type::Atom(r0)) => l0 == r0,
            (Type::Int(l0), Type::Int(r0)) => l0 == r0,
            (Type::Float(l0), Type::Float(r0)) => l0 == r0,
            (Type::Bool(l0), Type::Bool(r0)) => l0 == r0,
            (Type::FnPtr(l0), Type::FnPtr(r0)) => l0 == r0,
            (Type::Byts(l0), Type::Byts(r0)) => l0 == r0,
            (Type::List(l), Type::List(r)) => DoublyPtrHandle::ptr_eq(&l, &r),
            (Type::Record(l), Type::Record(r)) => DoublyPtrHandle::ptr_eq(&l, &r),
            (Type::Union(l), Type::Union(r)) => DoublyPtrHandle::ptr_eq(&l, &r),
            _ => false,
        }
    }
}

impl<'ctx1, 'ctx2, T: Tag> PartialEq<Type<'ctx2, T>> for Type<'ctx1, T> {
    fn eq(&self, other: &Type<'ctx2, T>) -> bool {
        match (*self, *other) {
            (Type::Any, Type::Any)
            | (Type::Nothing, Type::Nothing)
            | (Type::Bytes, Type::Bytes)
            | (Type::Number, Type::Number)
            | (Type::Boolean, Type::Boolean) => true,
            (Type::Atom(l0), Type::Atom(r0)) => l0 == r0,
            (Type::Int(l0), Type::Int(r0)) => l0 == r0,
            (Type::Float(l0), Type::Float(r0)) => l0 == r0,
            (Type::Bool(l0), Type::Bool(r0)) => l0 == r0,
            (Type::FnPtr(l0), Type::FnPtr(r0)) => l0 == r0,
            (Type::Byts(l0), Type::Byts(r0)) => l0 == r0,
            (Type::List(l0), Type::List(r0)) => **l0.borrow() == **r0.borrow(),
            (Type::Record(l0), Type::Record(r0)) => **l0.borrow() == **r0.borrow(),
            (Type::Union(l0), Type::Union(r0)) => **l0.borrow() == **r0.borrow(),
            _ => false,
        }
    }
}

impl<'ctx, T: Tag> Type<'ctx, T> {
    pub fn try_into_fnptr(self) -> Option<FunctionId<LiftedCtx>> {
        match self {
            Type::FnPtr(f) => Some(f),
            _ => None,
        }
    }

    #[track_caller]
    pub fn unwrap_fnptr(&self) -> FunctionId<LiftedCtx> {
        self.try_into_fnptr().unwrap()
    }

    pub fn try_into_list(self) -> Option<ListHandle<'ctx, T>> {
        match self {
            Type::List(handle) => Some(handle),
            _ => None,
        }
    }

    #[track_caller]
    pub fn unwrap_list(&self) -> ListHandle<'ctx, T> {
        self.try_into_list().unwrap()
    }

    pub fn try_into_record(self) -> Option<RecordHandle<'ctx, T>> {
        match self {
            Type::Record(handle) => Some(handle),
            _ => None,
        }
    }

    #[track_caller]
    pub fn unwrap_record(&self) -> RecordHandle<'ctx, T> {
        self.try_into_record().unwrap()
    }

    pub fn try_into_union(self) -> Option<UnionHandle<'ctx, T>> {
        match self {
            Type::Union(handle) => Some(handle),
            _ => None,
        }
    }

    #[track_caller]
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

pub type ListHandle<'ctx, T: Tag> = DoublyPtrHandle<'ctx, List<'ctx, T>>;
pub type RecordHandle<'ctx, T: Tag> = DoublyPtrHandle<'ctx, Record<'ctx, T>>;
pub type UnionHandle<'ctx, T: Tag> = DoublyPtrHandle<'ctx, Union<'ctx, T>>;

impl<'ctx, T: Tag> Debug for ListHandle<'ctx, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = self.borrow().unique_id();
        f.debug_struct("List").field("id", &id).finish()
    }
}

impl<'ctx, T: Tag> Debug for RecordHandle<'ctx, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = self.borrow().unique_id();
        f.debug_struct("Record").field("id", &id).finish()
    }
}

impl<'ctx, T: Tag> Debug for UnionHandle<'ctx, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = self.borrow().unique_id();
        f.debug_struct("Union").field("id", &id).finish()
    }
}

/// The handle to some data structure, guarded by a [`RefCell`]. Conceptually,
/// this type can be thought of as a `Handle(&mut T)`. The [`CtxBox`] and
/// [`RefCell`] are used to achieve interior mutability and cyclicity.
#[repr(transparent)]
#[derive(Deref, DerefMut, Derivative)]
#[derivative(
    Copy(bound = ""),
    Clone(bound = ""),
    Hash(bound = ""),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq")
)]
pub struct RefCellHandle<'ctx, T: ?Sized>(#[deref] CtxBox<'ctx, RefCell<T>>);

impl<'ctx, T> RefCellHandle<'ctx, T> {
    pub fn new(arena: &'ctx Bump, value: T) -> Self {
        RefCellHandle(CtxBox::new(arena, RefCell::new(value)))
    }
}

/// A doubly-pointed [`RefCellHandle`]. This is useful for when the
/// pointer-to-a-`T` needs to be updated in all places it's being used.
///
/// This struct acts like a [`RefCell<T>`] directly to the object (the "primary
/// pointer"), but allows access to the to the pointer that points to the
/// primary pointer (the "hop pointer").
///
/// To get access to the primary pointer, this implements `borrow` and
/// `borrow_mut` which access the primary pointer's [`RefCell`]
///
/// To get access to the hop pointer, call [`DoublePtrHandle::hop_ptr`], which
/// will call `borrow_mut` on the hop pointer.
#[repr(transparent)]
#[derive(Derivative)]
#[derivative(
    Copy(bound = ""),
    Clone(bound = ""),
    Hash(bound = ""),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq")
)]
pub struct DoublyPtrHandle<'ctx, T>(RefCellHandle<'ctx, RefCellHandle<'ctx, T>>);

#[derive(Deref, DerefMut)]
pub struct DoublyRef<'a: 'b, 'b, A, B> {
    hop_ref: Ref<'a, A>,
    #[deref]
    #[deref_mut]
    primary_ref: Ref<'b, B>,
}

#[derive(Deref, DerefMut)]
pub struct DoublyRefMut<'a: 'b, 'b, A, B> {
    hop_ref: Ref<'a, A>,
    #[deref]
    #[deref_mut]
    primary_ref: RefMut<'b, B>,
}

impl<'ctx, T> DoublyPtrHandle<'ctx, T> {
    pub fn new(arena: &'ctx Bump, value: T) -> Self {
        DoublyPtrHandle(RefCellHandle::new(arena, RefCellHandle::new(arena, value)))
    }

    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        let lhs_ptr = lhs.0.as_ptr();
        let rhs_ptr = rhs.0.as_ptr();
        std::ptr::eq(lhs_ptr, rhs_ptr)
    }

    pub fn borrow<'a>(&self) -> DoublyRef<'a, 'a, RefCellHandle<'ctx, T>, T> {
        let hop_ref = self.0.borrow();
        let primary_ref = hop_ref.borrow();
        DoublyRef {
            hop_ref,
            primary_ref,
        }
    }

    pub fn borrow_mut<'a>(&self) -> DoublyRefMut<'a, 'a, RefCellHandle<'ctx, T>, T> {
        let hop_ref = self.0.borrow();
        let primary_ref = hop_ref.borrow_mut();
        DoublyRefMut {
            hop_ref,
            primary_ref,
        }
    }

    pub fn hop_ptr(&self) -> RefMut<'_, RefCellHandle<'ctx, T>> {
        self.0.borrow_mut()
    }

    /// Copies the primary primary from another [`DoublyPtrHandle`] to this one.
    pub fn copy_primary_ptr<'a, 'b>(&'a self, other: &'b Self) {
        let mut self_hop = self.0.borrow_mut(); //.(1)
        let other_hop = *other.0.borrow();

        *self_hop = other_hop;
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
#[derive(Deref, DerefMut, Derivative)]
#[derivative(
    Copy(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq")
)]
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

// TODO(bug): this hash implementation is faulty
//   for example, imagine two `CtxBox`es with "asdf" in them:
//
//   +--------------+ +--------------+
//   |0x0000: "asdf"| |0x00FF: "asdf"|
//   .. . . .. . . .. .. . . .. . . ..
//
//   as both of these `TypeCtx`s have the same content ("asdf") at different
//   locations in memory, using a pointer for the hash behavior is incorrect.
impl<'ctx, T: ?Sized> Hash for CtxBox<'ctx, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // let ptr = self.0.as_ref().get_ref() as *const T;
        // ptr.hash(state)
        0u8.hash(state);
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::eq_op)]

    use super::*;
    use jssat_ir::id::{Counter, RegisterId};

    #[test]
    fn ref_eq_wrapper_works() {
        let mut a = TypeCtx::<LiftedCtx, RegisterId<LiftedCtx>>::new();

        a.borrow_mut(|mut a| {
            let rec1 = TypeRefEq(a.make_type_record(Record::new(Default::default())));
            let rec2 = TypeRefEq(a.make_type_record(Record::new(Default::default())));

            assert_eq!(rec1, rec1);
            assert_eq!(rec2, rec2);

            assert_ne!(rec1, rec2);
            assert_ne!(rec2, rec1);
        })
    }

    #[test]
    fn equality_between_bytes() {
        let mut a = TypeCtx::<LiftedCtx, RegisterId<LiftedCtx>>::new();

        a.borrow_mut(|mut a| {
            let a_asdf1 = a.make_type_byts(b"asdf");
            let a_asdf2 = a.make_type_byts(b"asdf");
            let a_ghjk = a.make_type_byts(b"ghjk");

            assert_eq!(a_asdf1, a_asdf1);
            assert_eq!(a_asdf1, a_asdf2);
            assert_eq!(a_asdf2, a_asdf1);
            assert_eq!(a_asdf2, a_asdf2);

            assert_ne!(a_asdf1, a_ghjk);
            assert_ne!(a_asdf2, a_ghjk);
            assert_ne!(a_ghjk, a_asdf1);
            assert_ne!(a_ghjk, a_asdf2);
        })
    }

    #[test]
    fn equality_between_bytes_and_atom() {
        let mut a = TypeCtx::<LiftedCtx, RegisterId<LiftedCtx>>::new();

        a.borrow_mut(|mut a| {
            let asdf = a.make_type_byts(b"asdf");
            let atom = Type::Atom(Atom(NonZeroU16::new(1).unwrap()));

            assert_eq!(asdf, asdf);
            assert_eq!(atom, atom);

            assert_ne!(asdf, atom);
            assert_ne!(atom, asdf);
        })
    }

    #[test]
    fn byts_keys_not_overwritten() {
        let mut a = TypeCtx::<LiftedCtx, RegisterId<LiftedCtx>>::new();

        a.borrow_mut(|mut a| {
            let mut rec = Record::new(Default::default());
            let asdf = a.make_type_byts(b"asdf");
            rec.insert(asdf, Type::Bool(true));

            let ghjk = a.make_type_byts(b"ghjk");
            rec.insert(ghjk, Type::Bool(false));

            assert_eq!(Some(Type::Bool(true)), rec.get(&asdf).copied());
            assert_eq!(Some(Type::Bool(false)), rec.get(&ghjk).copied());
        })
    }

    #[test]
    fn duplicating_record_keeps_keys() {
        let mut a = TypeCtx::<LiftedCtx, ()>::new();

        a.borrow_mut(|mut a| {
            let mut rec = Record::new(Default::default());
            let asdf = a.make_type_byts(b"asdf");
            rec.insert(asdf, Type::Bool(true));

            let ghjk = a.make_type_byts(b"ghjk");
            rec.insert(ghjk, Type::Bool(false));

            let rec_typ = a.make_type_record(rec);
            a.insert((), rec_typ);
        });

        let mut b = TypeCtx::<LiftedCtx, ()>::new();

        a.borrow(|a| {
            b.borrow_mut(|mut b| {
                let typ = b.duplicate_type(a.get(&()).unwrap());
                let rec = typ.unwrap_record().borrow();

                let asdf = b.make_type_byts(b"asdf");
                let ghjk = b.make_type_byts(b"ghjk");

                assert_eq!(Some(Type::Bool(true)), rec.get(&asdf).copied());
                assert_eq!(Some(Type::Bool(false)), rec.get(&ghjk).copied());
            });
        });
    }

    #[test]
    fn record_info_globally_updated() {
        let mut ctx1 = TypeCtx::<LiftedCtx, u8>::new();
        let mut unique_id_counter = Counter::new();

        // ctx1 : { 0 |-> Record(id = 1){ Any |-> Nothing } }
        ctx1.borrow_mut(|mut ctx| {
            let mut record = Record::new(unique_id_counter.next());
            record.insert(Type::Any, Type::Nothing);
            let record = ctx.make_type_record(record);

            ctx.insert(0, record);
        });

        // copy the record in `ctx1` to `ctx2`
        let mut ctx2 = TypeCtx::new();
        ctx1.borrow(|ctx| {
            ctx2.copy_from(&ctx, std::iter::once(0));
        });

        // modify the record in `ctx2`
        ctx2.borrow_mut(|mut ctx| {
            let record_ty = ctx.get(&0).unwrap();
            let mut record = record_ty.unwrap_record().borrow_mut();

            record.insert(Type::Any, Type::Number);
        });

        // copy back the modified record in `ctx1` to a different register in `ctx2`
        ctx2.borrow(|mut ctx| {
            ctx1.copy_from_map(&ctx, std::iter::once(0), |k| k + 1);
        });

        // assert that the record in `ctx1` at register `0` was updated
        ctx1.borrow_mut(|mut ctx| {
            let original_rec = ctx.get(&0).unwrap().unwrap_record().borrow();
            let copied_rec = ctx.get(&1).unwrap().unwrap_record().borrow();

            assert!(
                matches!(copied_rec.get(&Type::Any), Some(Type::Number)),
                "newly copied record should match"
            );

            assert!(
                matches!(original_rec.get(&Type::Any), Some(Type::Number)),
                "old record should be updated"
            );
        });
    }

    #[test]
    fn self_referential_record_copy() {
        let mut ctx = TypeCtx::<LiftedCtx, ()>::new();

        ctx.borrow_mut(|mut ctx| {
            let rec = Record::new(Default::default());
            let rec_typ = ctx.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(ctx.make_type_byts(b"key"), rec_typ);

            ctx.insert((), rec_typ);
        });

        let mut ctx2 = TypeCtx::new();

        ctx.borrow(|ctx| {
            ctx2.copy_from(&ctx, std::iter::once(()));
        });
    }

    #[test]
    fn self_referential_record_eq() {
        let mut ctx = TypeCtx::<LiftedCtx, ()>::new();

        ctx.borrow_mut(|mut ctx| {
            let rec = Record::new(Default::default());
            let rec_typ = ctx.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(ctx.make_type_byts(b"key"), rec_typ);
            drop(rec);

            assert_eq!(rec_typ, rec_typ);
        });
    }

    #[test]
    fn self_referential_record_2ctx_eq() {
        let mut ctx = TypeCtx::<LiftedCtx, ()>::new();

        ctx.borrow_mut(|mut ctx| {
            let rec = Record::new(Default::default());
            let rec_typ = ctx.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(ctx.make_type_byts(b"key"), rec_typ);

            ctx.insert((), rec_typ);
        });

        let mut ctx2 = TypeCtx::new();

        ctx.borrow(|ctx| {
            ctx2.copy_from(&ctx, std::iter::once(()));
        });

        ctx.borrow(|ctx| {
            ctx2.borrow(|ctx2| {
                let ctx_rec = ctx.get(&()).unwrap();
                let ctx2_rec = ctx2.get(&()).unwrap();

                assert_eq!(ctx_rec, ctx_rec);
                assert_eq!(ctx_rec, ctx2_rec);
                assert_eq!(ctx2_rec, ctx_rec);
                assert_eq!(ctx2_rec, ctx2_rec);
            })
        })
    }

    #[test]
    fn self_key_referential_record_copy() {
        let mut ctx = TypeCtx::<LiftedCtx, ()>::new();

        ctx.borrow_mut(|mut ctx| {
            let rec = Record::new(Default::default());
            let rec_typ = ctx.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(rec_typ, Type::Any);

            ctx.insert((), rec_typ);
        });

        let mut ctx2 = TypeCtx::new();

        ctx.borrow(|ctx| {
            ctx2.copy_from(&ctx, std::iter::once(()));
        });
    }

    #[test]
    fn self_key_referential_record_eq() {
        todo!("stack overflow");

        let mut ctx = TypeCtx::<LiftedCtx, ()>::new();

        ctx.borrow_mut(|mut ctx| {
            let rec = Record::new(Default::default());
            let rec_typ = ctx.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(rec_typ, Type::Any);
            drop(rec);

            assert_eq!(rec_typ, rec_typ);
        });
    }

    #[test]
    fn self_key_referential_record_2ctx_eq() {
        todo!("stack overflow");

        let mut ctx = TypeCtx::<LiftedCtx, ()>::new();

        ctx.borrow_mut(|mut ctx| {
            let rec = Record::new(Default::default());
            let rec_typ = ctx.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(rec_typ, Type::Any);

            ctx.insert((), rec_typ);
        });

        let mut ctx2 = TypeCtx::new();

        ctx.borrow(|ctx| {
            ctx2.copy_from(&ctx, std::iter::once(()));
        });

        ctx.borrow(|ctx| {
            ctx2.borrow(|ctx2| {
                let ctx_rec = ctx.get(&()).unwrap();
                let ctx2_rec = ctx2.get(&()).unwrap();

                assert_eq!(ctx_rec, ctx_rec);
                assert_eq!(ctx_rec, ctx2_rec);
                assert_eq!(ctx2_rec, ctx_rec);
                assert_eq!(ctx2_rec, ctx2_rec);
            })
        })
    }

    #[test]
    fn copies_rec_with_two_byte_keys() {
        let mut ctx = TypeCtx::<LiftedCtx, ()>::new();

        ctx.borrow_mut(|mut ctx| {
            let rec = Record::new(Default::default());
            let rec_typ = ctx.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(ctx.make_type_byts(b"key1"), Type::Int(12));
            rec.insert(ctx.make_type_byts(b"key2"), Type::Int(5));

            ctx.insert((), rec_typ);
        });

        let mut ctx2 = TypeCtx::new();

        ctx.borrow(|ctx| {
            ctx2.copy_from(&ctx, std::iter::once(()));
        });

        ctx2.borrow(|ctx2| {
            let ctx2_rec = ctx2.get(&()).unwrap();
            let rec = ctx2_rec.unwrap_record().borrow();

            let key1 = ctx2.make_type_byts(b"key1");
            let key2 = ctx2.make_type_byts(b"key2");

            assert_eq!(rec.get(&key1).copied(), Some(Type::Int(12)));
            assert_eq!(rec.get(&key2).copied(), Some(Type::Int(5)));
        })
    }
}
