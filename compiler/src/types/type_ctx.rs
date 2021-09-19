use super::{AssociatedType, Type, TypeCtx};
use crate::id::{RegisterId, Tag};

impl<T: Tag> TypeCtx<T> {
    /// Constructs a new, blank [`TypeCtx`].
    ///
    /// # Examples
    ///
    /// ```
    /// let type_ctx = TypeCtx::new();
    /// ```
    pub fn new() -> Self {
        Self {
            registers: Default::default(),
        }
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
    /// assert!(type_ctx.get(&register).unwrap().is_same_kind(&Type::<NoContext>::Any))
    /// ```
    pub fn get(&self, key: &RegisterId<T>) -> Option<AssociatedType<T>> {
        self.registers.get(key).map(|t| AssociatedType(t, self))
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
    /// type_ctx.insert(register, Type::Any); // ok
    /// type_ctx.insert(register, Type::Bytes); // not ok, must retain SSA form
    /// ```
    #[track_caller]
    pub fn insert(&mut self, key: RegisterId<T>, value: Type<T>) {
        let no_value_present = matches!(self.registers.insert(key, value), None);
        assert!(no_value_present, "should not overwrite register");
    }
}

impl<T: Tag> Default for TypeCtx<T> {
    fn default() -> Self {
        Self::new()
    }
}
