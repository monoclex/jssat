use std::{cell::RefCell, marker::PhantomData, pin::Pin};

use bumpalo::{boxed::Box, Bump};
use ouroboros::self_referencing;
use ref_cast::RefCast;
use rustc_hash::FxHashMap;

use super::{Record, Type};
use crate::id::{RecordId, RegisterId, Tag};

/// Datastructure used to keep track of the types of registers.
pub struct TypeCtx<T: Tag>(TypeCtxImpl<T>);

#[self_referencing]
pub struct TypeCtxImpl<T: Tag> {
    arena: Bump,
    #[borrows(arena)]
    #[not_covariant]
    // #[covariant]
    registers: FxHashMap<RegisterId<T>, Type<'this, T>>,
}

impl<T: Tag> TypeCtx<T> {
    /// Constructs a new, blank [`TypeCtx`].
    ///
    /// # Examples
    ///
    /// ```
    /// let type_ctx = TypeCtx::new();
    /// ```
    pub fn new() -> Self {
        Self(TypeCtxImpl::new(Bump::new(), |_| Default::default()))
    }

    /// Gets the type of a register.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let mut type_ctx = TypeCtx::<NoContext>::new();
    /// let register = RegisterId::new();
    ///
    /// type_ctx.insert(register, Type::Any);
    /// assert!(type_ctx.get(&register, |v| v.unwrap().is_same_kind(&Type::<NoContext>::Any)));
    /// ```
    pub fn get<R>(
        &self,
        key: &RegisterId<T>,
        on_value: impl FnOnce(Option<&Type<'_, T>>) -> R,
    ) -> R {
        self.0
            .with_registers(|registers| on_value(registers.get(key)))
    }

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
    pub fn insert(
        &mut self,
        key: RegisterId<T>,
        value: impl for<'a> FnOnce(TypeBuilder<'a, T>) -> Type<'a, T>,
    ) {
        self.0.with_mut(|ctx| {
            let value = value(TypeBuilder {
                arena: ctx.arena,
                _tag: PhantomData,
            });
            let no_value_present = matches!(ctx.registers.insert(key, value), None);
            assert!(no_value_present, "should not overwrite register");
        });
    }
}

fn usage<T: Tag>(key: &RegisterId<T>, ctx: &mut TypeCtx<T>) {
    ctx.get(key, |v| match v {
        Some(Type::Record(record)) => {
            let a = &*record.borrow();
        }
        _ => panic!(),
    });
}

pub struct TypeBuilder<'ctx, T: Tag> {
    arena: &'ctx Bump,
    _tag: PhantomData<T>,
}

impl<'c, T: Tag> TypeBuilder<'c, T> {
    pub fn make_byts(self, payload: &[u8]) -> Type<'c, T> {
        let mut bytes = bumpalo::collections::Vec::with_capacity_in(payload.len(), self.arena);
        bytes.extend_from_slice(payload);
        let payload = bytes.into_boxed_slice();

        let pinned = Pin::new(payload);
        let pin = self.arena.alloc(pinned);
        Type::Byts(pin)
    }

    pub fn make_record(self, record: Record<'c, T>) -> Type<'c, T> {
        let pinned = Box::pin_in(RefCell::new(record), self.arena);
        let pin = self.arena.alloc(pinned);
        Type::Record(pin)
    }
}

impl<T: Tag> Default for TypeCtx<T> {
    fn default() -> Self {
        Self::new()
    }
}
