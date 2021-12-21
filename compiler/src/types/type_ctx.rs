use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, VecDeque},
    hash::Hash,
    ops::Deref,
};

use bumpalo::Bump;
use ouroboros::self_referencing;
use rustc_hash::{FxHashMap, FxHashSet};

use super::{CtxBox, Record, RecordHandle, Type, Union, UnionHandle};
use crate::{
    id::{Counter, RegisterId, Tag, UniqueRecordId},
    types::BytsHandle,
};

/// A datastructure used to keep track of registers and records.
///
/// A key component of this implementation is that it provides facilities for
/// modifying and mutating records in a cyclic fashion, rather than with indices
/// into hashmaps. This key difference adds a ton of complexity in exchange for
/// ease of use and less logic errors. To truly understand and appreciate this,
/// the remainder of this documentation will explain the difference in
/// implementation.
///
/// # Index Implementation
///
/// An implementation of a [`TypeCtx`] based on indices may look as follows:
///
/// ```no_run
/// # use std::collections::HashMap;
/// #
/// struct TypeCtx {
///     registers: HashMap<usize, Type>,
///     records: HashMap<usize, Record>,
/// }
///
/// enum Type {
///     Int,
///     Record(usize),
/// }
///
/// struct Record {
///     fields: HashMap<String, Type>,
/// }
/// ```
///
/// As is apparent, the concept is extremely simple to implement. However, there
/// are some significant drawbacks in usability:
///
/// - A correct [`PartialEq`] implementation for `Type` cannot exist, as it does
///   not have enough information to perform deep equality between records (it
///   must obtain a reference to a [`TypeCtx`])
///
/// - It is extremely error prone to ensure that the index being given to
///   `records` is valid to prevent a panic at runtime
///
/// - It is unergonomic and error prone to convert a `Type` to a `&mut Record`,
///   as the inner `usize` must be assumed to be valid for any `TypeCtx` that is
///   being used to look up the record. This can either silently succeed, as the
///   record ID may happen to be valid, or panic.
///
/// Overall, these issues can definitely be worked around, but an ergonomic
/// solution is much preferred.
///
/// # This Implementation
///
/// The implementation of [`TypeCtx`] as in the below code can conceptually be
/// thought of as follows:
///
/// ```no_run
/// # use std::collections::HashMap;
/// #
/// struct TypeCtx {
///     registers: HashMap<usize, Type>,
/// }
///
/// enum Type {
///     Int,
///     Record(&mut Record),
/// }
///
/// struct Record {
///     fields: HashMap<String, Type>,
/// }
/// ```
///
/// As is visible from the conceptual overview, by having the `Record` be usable
/// within the `Type`, it makes it much more ergonomic to do the following:
///
/// - Pattern match on records, or other values properly
/// - Implement deep equality
///
/// # This Implementation Details
///
/// Unfortunately, the above conceptual overview is completely invalid code.
/// Firstly, `Type`s cannot hold a mutable reference to a `Record`, but must
/// provide mutable access to a Record to add more fields at a later date. To
/// fix this, [`RefCell`] is used:
///
/// ```no_run
/// enum Type {
///     Record(RefCell<Record>)
/// }
/// ```
///
/// However, this is not sufficient, as we must be able to make and share
/// multiple `Type`s. Thus, this implementation uses [`bumpalo`] and
/// [`ouroboros`] to create a bump-allocated arena within the [`TypeCtx`] and
/// allocate the [`RefCell`]s into that, providing back a reference that's valid
/// for the duration of the bump allocator (which will live as long as the
/// [`TypeCtx`]). This can be seen as follows:
///
/// ```no_run
/// enum Type<'ctx> {
///     Record(&'ctx RefCell<Record>)
/// }
/// ```
///
/// Lastly, due to the need to implement [`std::hash::Hash`], a
/// [`std::pin::Pin`] and [`Box`] is used to use the _address of the record_ as
/// the hash. This prevents cyclicity problems when deeply hashing a record (as
/// it is possible for them to be cyclic).
pub struct TypeCtx<T: Tag = crate::id::LiftedCtx, K = RegisterId<T>>(TypeCtxImpl<T, K>);

/// # Safety
///
/// The error we run into when attempting to send this to another thread, can be
/// reproduced with the following code example below:
///
/// ```no_run
/// # use std::cell::RefCell;
/// # use std::sync::Mutex;
/// #
/// struct Problematic<'ctx> {
///     problem: &'ctx RefCell<Problematic<'ctx>>,
/// }
///
/// fn problem<'ctx>() {
///     // no worky
///     is_send::<Mutex<Problematic<'ctx>>>()
/// }
///
/// fn is_send<T: Send>() {}
/// ```
///
/// However, we know that for a given [`TypeCtx`] that anything created within
/// it will *stay* within it, and that the [`RefCell`] will be "closed" if we
/// own a [`TypeCtx`] that we want to send across threads. Thus, we choose to
/// implement [`Send`].
unsafe impl<T: Tag> Send for TypeCtx<T> {}

impl<T: Tag, K> TypeCtx<T, K> {
    /// Constructs a new, blank [`TypeCtx`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let type_ctx = TypeCtx::<NoContext>::new();
    /// ```
    pub fn new() -> Self {
        let ctx_impl = TypeCtxImplBuilder {
            arena: Default::default(),
            lookup_builder: |_| Default::default(),
            handle_types_builder: |_| Default::default(),
        }
        .build();

        Self(ctx_impl)
    }
}

impl<T: Tag, K> Default for TypeCtx<T, K> {
    fn default() -> Self {
        Self::new()
    }
}

#[self_referencing]
pub struct TypeCtxImpl<T: Tag, K> {
    arena: Bump,
    #[borrows(arena)]
    #[not_covariant]
    lookup: FxHashMap<K, Type<'this, T>>,
    #[borrows(arena)]
    #[not_covariant]
    /// Records only the types that require the use of handles
    handle_types: Vec<Type<'this, T>>,
}

impl<T: Tag, K> TypeCtx<T, K> {
    /// Immutably borrows the data inside of the [`TypeCtx`] for the duration of
    /// the [`run`] closure.
    ///
    /// # Necessity
    ///
    /// Unfortunately, [`TypeCtx`] cannot be used without a closure to borrow
    /// its contents. This is because it is a self referential struct, and
    /// [`ouroboros`] forces this closure pattern to support it.
    ///
    /// Fortuantely, much effort has been put in to make the usage of the
    /// context within the closure as ergonomic as possible.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let ctx = TypeCtx::<NoContext>::new();
    ///
    /// let has_default_register = ctx.borrow(|ctx| {
    ///     ctx.get(&Default::default()).is_some()
    /// });
    ///
    /// assert_eq!(has_default_register, false);
    /// ```
    pub fn borrow<R>(&self, run: impl FnOnce(TypeCtxImmut<T, K>) -> R) -> R {
        self.0.with(|it| {
            run(TypeCtxImmut {
                lookup: it.lookup,
                handle_types: it.handle_types,
                arena: it.arena,
            })
        })
    }

    /// Mutably borrows the data inside of the [`TypeCtx`] for the duration of
    /// the [`run`] closure.
    ///
    /// # Necessity
    ///
    /// Unfortunately, [`TypeCtx`] cannot be used without a closure to borrow
    /// its contents. This is because it is a self referential struct, and
    /// [`ouroboros`] forces this closure pattern to support it.
    ///
    /// Fortuantely, much effort has been put in to make the usage of the
    /// context within the closure as ergonomic as possible.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let mut ctx = TypeCtx::<NoContext>::new();
    ///
    /// let inserted_any = ctx.borrow_mut(|mut ctx| {
    ///     ctx.insert(Default::default(), Type::Any);
    ///     matches!(ctx.get(&Default::default()), Type::Any)
    /// });
    ///
    /// assert!(inserted_any);
    /// ```
    pub fn borrow_mut<R>(&mut self, run: impl FnOnce(TypeCtxMut<T, K>) -> R) -> R {
        self.0.with_mut(|it| {
            run(TypeCtxMut {
                lookup: it.lookup,
                handle_types: it.handle_types,
                arena: it.arena,
            })
        })
    }
}

// SAFETY: the layout of this must be the same as [`TypeCtxMut`]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TypeCtxImmut<'borrow, 'arena, T: Tag, K> {
    lookup: &'borrow FxHashMap<K, Type<'arena, T>>,
    handle_types: &'borrow Vec<Type<'arena, T>>,
    arena: &'arena Bump,
}

// SAFETY: the layout of this must be the same as [`TypeCtxImmut`]
#[repr(C)]
pub struct TypeCtxMut<'borrow, 'arena, T: Tag, K> {
    lookup: &'borrow mut FxHashMap<K, Type<'arena, T>>,
    handle_types: &'borrow mut Vec<Type<'arena, T>>,
    arena: &'arena Bump,
}

impl<'borrow, 'arena, T: Tag, K> Deref for TypeCtxMut<'borrow, 'arena, T, K> {
    type Target = TypeCtxImmut<'borrow, 'arena, T, K>;

    fn deref(&self) -> &Self::Target {
        // SAFETY:
        // `TypeCtxMut` and `TypeCtxImmut` have the same layout, and the #[repr(C)]
        // attribute is placed to ensure that their layout will be the exact same.
        // The contents of the data being transmuted are:
        // - immutable borrow => immutable borrow
        // - mutable borrow   => immutable borrow
        // which is fine to do.
        unsafe { std::mem::transmute(self) }
    }
}

impl<'borrow, 'arena, T: Tag, K> TypeCtxMut<'borrow, 'arena, T, K>
where
    K: Eq + Hash,
{
    /// Sets the type of a register to the type specified.
    ///
    /// # Examples
    ///
    /// ```should_panic
    /// # use crate::id::NoContext;
    /// let mut type_ctx = TypeCtx::<NoContext>::new();
    /// let register = RegisterId::new();
    ///
    /// type_ctx.insert(register, |_| Type::Any); // ok
    /// type_ctx.insert(register, |_| Type::Bytes); // not ok, must retain SSA form
    /// ```
    #[track_caller]
    pub fn insert(&mut self, key: K, value: Type<'arena, T>) {
        match value {
            Type::Union(union) => {}
            _ => {}
        }

        let no_value_present = matches!(self.lookup.insert(key, value), None);
        assert!(no_value_present, "should not overwrite register");
    }

    /// Gets the type of a register.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let mut ctx = TypeCtx::<NoContext>::new();
    /// let register = RegisterId::new();
    ///
    /// ctx.insert(register, Type::Any);
    /// assert!(type_ctx.get_mut(&register, |v| v.unwrap().is_same_kind(&Type::<NoContext>::Any)));
    /// ```
    pub fn get_mut(&mut self, key: &K) -> Option<&mut Type<'arena, T>> {
        self.lookup.get_mut(key)
    }

    /// Given a [`Type`] contained within another [`TypeCtx`], this method will
    /// duplicate that type into the current arena, so that it is usable as a
    /// [`Type`] within the current arena.
    ///
    /// To duplicate types, this uses the [`TypeDuplication`] struct to clone a
    /// single type. If cloning multiple types, it is more efficient to reuse
    /// the [`TypeDuplication`] struct by creating it yourself.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut ctx1 = TypeCtx::new();
    ///
    /// let register = RegisterId::new();
    /// let typ1 = ctx1.make_type_bytes([1, 2, 3]);
    /// ctx1.insert(register, typ);
    ///
    /// let mut ctx2 = TypeCtx::new();
    ///
    /// ctx1.borrow_mut(|ctx1| {
    ///     ctx2.borrow_mut(|ctx2| {
    ///         let typ2 = ctx2.duplicate_type(ctx1.get(&register));
    ///         ctx2.insert(register, typ2);
    ///     });
    /// });
    /// ```
    pub fn duplicate_type<'distant_arena>(
        &mut self,
        other: Type<'distant_arena, T>,
    ) -> Type<'arena, T> {
        TypeDuplication::new(self).duplicate_type(other)
    }
}

/// A struct that duplicates types from one [`TypeCtx`] to another. This records
/// which types have already been duplicated into the other arena, and prevents
/// duplicate work from being done. This anti-reduplication feature is necessary
/// to ensure that cyclic records can be duplicated, and also yields performance
/// benefits when cloning multiple types.
pub struct TypeDuplication<'borrow, 'arena, 'distant_arena, 'type_ctx, T: Tag, K> {
    type_ctx: &'type_ctx mut TypeCtxMut<'borrow, 'arena, T, K>,
    /// Using the [`Eq`] trait of [`Type`]s, this checks for referential level
    /// equality between types before deciding to clone them. This is cheap and
    /// effective.
    duplications: FxHashMap<Type<'distant_arena, T>, Type<'arena, T>>,
}

impl<'b, 'a, 'd, 't, T: Tag, K: Hash + Eq> TypeDuplication<'b, 'a, 'd, 't, T, K> {
    /// Creates a new instance of a [`TypeDuplication`] struct.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut ctx = TypeCtx::new();
    ///
    /// ctx.borrow_mut(|ctx| {
    ///     let dup = TypeDuplication::new(ctx);
    /// });
    /// ```
    pub fn new(type_ctx: &'t mut TypeCtxMut<'b, 'a, T, K>) -> Self {
        Self {
            type_ctx,
            duplications: Default::default(),
        }
    }

    /// Duplicates a type from one [`TypeCtx`] to another, and doesn't
    /// re-duplicate types that have already been duplicated within the lifetime
    /// of this [`TypeDuplication`].
    pub fn duplicate_type(&mut self, typ: Type<'d, T>) -> Type<'a, T> {
        macro_rules! cache {
            ($x:expr) => {
                if let Some(result) = self.duplications.get(&$x) {
                    return *result;
                }
            };
        }

        match typ {
            // simple types
            Type::Any => Type::Any,
            Type::Nothing => Type::Nothing,
            Type::Bytes => Type::Bytes,
            Type::Number => Type::Number,
            Type::Boolean => Type::Boolean,
            Type::Atom(x) => Type::Atom(x),
            Type::Int(x) => Type::Int(x),
            Type::Float(x) => Type::Float(x),
            Type::Bool(x) => Type::Bool(x),
            Type::FnPtr(x) => Type::FnPtr(x),
            Type::Byts(handle) => {
                cache!(typ);

                let dest = self.type_ctx.make_type_byts(&handle);
                self.duplications.insert(typ, dest);
                dest
            }
            Type::Record(handle) => {
                cache!(typ);

                let src_record = handle.borrow();

                // create the type and insert it early
                // this makes the recursive case fetch the type
                let record_typ =
                    (self.type_ctx).make_type_record(Record::new(src_record.unique_id()));
                self.duplications.insert(typ, record_typ);

                // duplicate the kvps of the record
                let mut dest_record = record_typ.unwrap_record().borrow_mut();
                for (key, value) in src_record.iter() {
                    let dest_key = self.duplicate_type(*key);
                    let dest_value = self.duplicate_type(*value);

                    dest_record.insert(dest_key, dest_value);
                }

                record_typ
            }
            Type::Union(handle) => {
                cache!(typ);

                let src_union = handle.borrow();

                // create the type and insert it early
                // this makes the recursive case fetch the type
                let union_typ = (self.type_ctx).make_type_union(Union::new(src_union.unique_id()));
                self.duplications.insert(typ, union_typ);

                // duplicate the elements of the union
                let mut dest_union = union_typ.unwrap_union().borrow_mut();
                dest_union.extend(src_union.iter().map(|t| self.duplicate_type(*t)));

                union_typ
            }
        }
    }
}

impl<'borrow, 'arena, T: Tag, K> TypeCtxImmut<'borrow, 'arena, T, K>
where
    K: Eq + Hash,
{
    /// Gets the type of a register.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let mut ctx = TypeCtx::<NoContext>::new();
    /// let register = RegisterId::new();
    ///
    /// ctx.insert(register, Type::Any);
    /// assert!(type_ctx.get(&register, |v| v.unwrap().is_same_kind(&Type::<NoContext>::Any)));
    /// ```
    pub fn get(&self, key: &K) -> Option<&Type<'arena, T>> {
        self.lookup.get(key)
    }
}

impl<'borrow, 'arena, T: Tag, K> TypeCtxImmut<'borrow, 'arena, T, K> {
    // the reason our methods implement `T -> Type` rather than a `T -> Handle<T>`
    // is because the caller *already owns* a T. if they wanna modify it, they
    // can do so while they still own it. they only time they'd want a handle to
    // it is when you wanna stuff it into a type.

    /// Constructs an instance of [`Type::Byts`] for this [`TypeCtx`]
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let mut ctx = TypeCtx::<NoContext>::new();
    ///
    /// let is_byts = ctx.borrow(|ctx| {
    ///     let byts: Type<_> = ctx.make_type_byts("asdf".as_bytes());
    ///     matches!(byts, Type::Byts(_))
    /// });
    ///
    /// assert!(is_byts)
    /// ```
    pub fn make_type_byts(&self, payload: &[u8]) -> Type<'arena, T> {
        use bumpalo::collections::Vec;

        let mut space_for_payload_in_bumpalo = Vec::with_capacity_in(payload.len(), self.arena);
        space_for_payload_in_bumpalo.extend_from_slice(payload);

        let payload = space_for_payload_in_bumpalo.into_boxed_slice();
        Type::Byts(BytsHandle(CtxBox::new_unsized(self.arena, payload)))
    }

    /// Constructs an instance of [`Type::Record`] for this [`TypeCtx`]
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let mut ctx = TypeCtx::<NoContext>::new();
    ///
    /// let is_record = ctx.borrow(|ctx| {
    ///     let record: Type<_> = ctx.make_type_record(ctx.make_record());
    ///     matches!(byts, Type::Record(_))
    /// });
    ///
    /// assert!(is_record)
    /// ```
    pub fn make_type_record(&self, record: Record<'arena, T>) -> Type<'arena, T> {
        Type::Record(RecordHandle(CtxBox::new(self.arena, RefCell::new(record))))
    }

    /// Constructs an instance of [`Type::Union`] for this [`TypeCtx`]
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let mut ctx = TypeCtx::<NoContext>::new();
    ///
    /// let is_union = ctx.borrow(|ctx| {
    ///     let union: Type<_> = ctx.make_type_union(Union::new());
    ///     matches!(byts, Type::Union(_))
    /// });
    ///
    /// assert!(is_union)
    /// ```
    pub fn make_type_union(&self, union: Union<'arena, T>) -> Type<'arena, T> {
        Type::Union(UnionHandle(CtxBox::new(self.arena, RefCell::new(union))))
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &Type<'arena, T>)> {
        self.lookup.iter()
    }
}

/// [https://github.com/mTvare6/hello-world.rs/blob/f06a4f596061f2e84beb6cdb472cfb0b8f738c85/src/main.rs#L321-L324]
#[test]
#[allow(clippy::eq_op)]
fn solarsystem_level_enterprise_test() {
    assert_eq!(1, 1);
}
