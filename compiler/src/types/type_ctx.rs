use std::{cell::RefCell, ops::Deref};

use bumpalo::Bump;
use ouroboros::self_referencing;
use rustc_hash::FxHashMap;

use super::{CtxBox, Record, RecordHandle, Type};
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
pub struct TypeCtx<T: Tag = crate::id::LiftedCtx>(TypeCtxImpl<T>);

/// # Safety
///
/// The error we run into when attempting to send this to another thread, can be
/// reproduced with the following code example below:
///
/// ```no_run
/// struct Problematic<'ctx> {
///     problem: &'ctx std::cell::RefCell<Problematic<'ctx>>,
/// }
///
/// fn problem<'ctx>() {
///     // no worky
///     is_send::<std::sync::Mutex<Problematic<'ctx>>>()
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

fn x() {
    let id = |x| x;

    consume(&id);
}

fn consume(it: &dyn FnOnce(String) -> String) {
    //
}

impl<T: Tag> TypeCtx<T> {
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
            unique_allocation_id_counter: Default::default(),
            arena: Default::default(),
            registers_builder: |_| Default::default(),
        }
        .build();

        Self(ctx_impl)
    }
}

impl<T: Tag> Default for TypeCtx<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[self_referencing]
pub struct TypeCtxImpl<T: Tag> {
    unique_allocation_id_counter: Counter<UniqueRecordId<T>>,
    arena: Bump,
    #[borrows(arena)]
    #[not_covariant]
    registers: FxHashMap<RegisterId<T>, Type<'this, T>>,
}

impl<T: Tag> TypeCtx<T> {
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
    pub fn borrow<R>(&self, run: impl FnOnce(TypeCtxImmut<T>) -> R) -> R {
        self.0.with(|it| {
            run(TypeCtxImmut {
                registers: it.registers,
                arena: it.arena,
                unique_allocation_id_counter: it.unique_allocation_id_counter,
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
    pub fn borrow_mut<R>(&mut self, run: impl FnOnce(TypeCtxMut<T>) -> R) -> R {
        self.0.with_mut(|it| {
            run(TypeCtxMut {
                registers: it.registers,
                arena: it.arena,
                unique_allocation_id_counter: it.unique_allocation_id_counter,
            })
        })
    }
}

// SAFETY: the layout of this must be the same as [`TypeCtxMut`]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct TypeCtxImmut<'borrow, 'arena, T: Tag> {
    registers: &'borrow FxHashMap<RegisterId<T>, Type<'arena, T>>,
    arena: &'arena Bump,
    unique_allocation_id_counter: &'borrow Counter<UniqueRecordId<T>>,
}

// SAFETY: the layout of this must be the same as [`TypeCtxImmut`]
#[repr(C)]
pub struct TypeCtxMut<'borrow, 'arena, T: Tag> {
    registers: &'borrow mut FxHashMap<RegisterId<T>, Type<'arena, T>>,
    arena: &'arena Bump,
    unique_allocation_id_counter: &'borrow Counter<UniqueRecordId<T>>,
}

impl<'borrow, 'arena, T: Tag> Deref for TypeCtxMut<'borrow, 'arena, T> {
    type Target = TypeCtxImmut<'borrow, 'arena, T>;

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

impl<'borrow, 'arena, T: Tag> TypeCtxMut<'borrow, 'arena, T> {
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
    pub fn insert(&mut self, key: RegisterId<T>, value: Type<'arena, T>) {
        let no_value_present = matches!(self.registers.insert(key, value), None);
        assert!(no_value_present, "should not overwrite register");
    }
}

impl<'borrow, 'arena, T: Tag> TypeCtxImmut<'borrow, 'arena, T> {
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
    pub fn get(&self, key: &RegisterId<T>) -> Option<&Type<'arena, T>> {
        self.registers.get(key)
    }

    /// Constructs a [`Record`] in this [`TypeCtx`]
    pub fn make_record(&self) -> Record<'arena, T> {
        Record::new(self.unique_allocation_id_counter.next())
    }

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
}

/// [https://github.com/mTvare6/hello-world.rs/blob/f06a4f596061f2e84beb6cdb472cfb0b8f738c85/src/main.rs#L321-L324]
#[test]
#[allow(clippy::eq_op)]
fn solarsystem_level_enterprise_test() {
    assert_eq!(1, 1);
}
