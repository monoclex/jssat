//! Contains data structures that are necessary to memorize information about
//! records - their history of transitions, taking a view on them, and narrowing
//! that view based on facts. All terminology is briefly explained here, but
//! explained in depth at [the corresonding blog post][blog post]
//!
//! [blog post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/
#![allow(warnings)]

use derive_more::Display;
use std::collections::VecDeque;
use std::fmt::Display;
use std::sync::{Arc, Mutex};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::id::{Counter, LiftedCtx, SymbolicCtx};
use crate::isa::{InternalSlot, RecordKey, TrivialItem};
use crate::UnwrapNone;

type AllocationId = crate::id::AllocationId<LiftedCtx>;
type ConstantId = crate::id::ConstantId<SymbolicCtx>;
type RegisterId = crate::id::RegisterId<LiftedCtx>;
type ShapeId = crate::id::ShapeId<SymbolicCtx>;
/// The ID of a function whose argument types are not yet known.
type DynFnId = crate::id::FunctionId<LiftedCtx>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReturnType {
    Void,
    Value(RegisterType),
    Never,
}

impl ReturnType {
    pub fn map<F>(self, map: F) -> ReturnType
    where
        F: FnOnce(RegisterType) -> RegisterType,
    {
        match self {
            ReturnType::Value(v) => ReturnType::Value(map(v)),
            other => other,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum RegisterType {
    Any,
    Trivial(TrivialItem),
    Bytes,
    Byts(ConstantId),
    Number,
    Int(i64),
    Boolean,
    Bool(bool),
    FnPtr(DynFnId),
    Record(AllocationId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ShapeKey {
    Str(ConstantId),
    Slot(InternalSlot),
}

pub type ShapeValueType = RegisterType;

#[derive(Clone, Debug, Default)]
pub struct Shape {
    // TODO: private this (it should not be public)
    pub(crate) fields: FxHashMap<ShapeKey, ShapeValueType>,
}

impl Shape {
    pub fn new_with(&self, key: ShapeKey, value: ShapeValueType) -> Shape {
        todo!()
    }

    pub fn get_typ(&self, key: ShapeKey) -> ShapeValueType {
        todo!()
    }
}

#[derive(Debug)]
pub enum LookingUp {
    Nothing,
    RecordKey(ShapeKey),
    Register(RegisterId),
    Constant(ConstantId),
}

#[derive(Debug, Clone)]
pub struct TypeBag {}

impl TypeBag {
    pub fn looking_up(&self) -> LookingUp {
        todo!()
    }

    pub fn new_record(&mut self, register: RegisterId) {
        todo!()
    }

    #[deprecated]
    pub fn append_shape(&mut self, register: RegisterId, shape: Shape) {
        todo!()
    }

    #[deprecated]
    pub fn push_shape(&mut self, alloc: AllocationId, shape: ShapeId) {
        todo!()
    }

    #[deprecated]
    pub fn new_shape(&mut self, shape: Shape) -> ShapeId {
        todo!()
    }

    pub fn assign_type(&mut self, register: RegisterId, typ: RegisterType) {
        todo!()
    }

    pub fn intern_constant(&mut self, payload: &[u8]) -> ConstantId {
        todo!()
    }

    pub fn get(&self, register: RegisterId) -> RegisterType {
        todo!()
    }

    #[deprecated]
    pub fn get_field_type(&self, shape: &Shape, key: RecordKey<LiftedCtx>) -> ShapeValueType {
        todo!()
    }

    #[deprecated]
    pub fn has_field(&self, shape: &Shape, key: RecordKey<LiftedCtx>) -> Option<bool> {
        todo!()
    }

    #[deprecated]
    pub fn record_shape(&self, register: RegisterId) -> &Shape {
        todo!()
    }

    pub fn get_fnptr(&self, register: RegisterId) -> DynFnId {
        todo!()
    }

    pub fn get_shape_id(&self, alloc: AllocationId) -> ShapeId {
        todo!()
    }

    #[deprecated]
    pub fn get_shape(&self, shape: ShapeId) -> &Shape {
        todo!()
    }

    #[deprecated]
    pub fn conv_key(&self, key: RecordKey<LiftedCtx>) -> ShapeKey {
        todo!()
    }

    pub fn unintern_const(&self, id: ConstantId) -> &Vec<u8> {
        todo!()
    }

    pub fn display(&self, register: RegisterId) -> String {
        DisplayContext {
            types: &self,
            shapes_shown: Vec::new(),
        }
        .display(register)
    }

    /// Given a set of registers, will pull out the types of those registers
    /// and create a new `TypeBag` containing only the types of the registers
    /// specified.
    ///
    /// This is useful for continuing execution of a function after a block.
    pub fn extract(&self, regs: &[RegisterId]) -> (Self, FxHashMap<AllocationId, AllocationId>) {
        todo!()
    }

    /// Given a set of registers, it will extract the type out of them and
    /// place those types into the register specified (RHS in the tuple)
    pub fn extract_map(
        &self,
        regs: impl Iterator<Item = (RegisterId, RegisterId)>,
    ) -> (Self, FxHashMap<AllocationId, AllocationId>) {
        todo!()
    }

    // TODO: deduplicate this code
    pub fn pull_type_into(
        &self,
        typ: RegisterType,
        into: &mut TypeBag,
        me_to_them_alloc_map: &FxHashMap<AllocationId, AllocationId>,
    ) -> RegisterType {
        todo!()
    }
}

impl Default for TypeBag {
    fn default() -> Self {
        todo!()
    }
}

impl PartialEq for TypeBag {
    /// Makes sure that every register pairing of two type bags are the same.
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

pub struct DisplayContext<'types> {
    types: &'types TypeBag,
    shapes_shown: Vec<ShapeId>,
}

impl DisplayContext<'_> {
    pub fn display(&mut self, register: RegisterId) -> String {
        let mut s = String::new();

        todo!();

        s
    }

    fn display_typ(&mut self, w: &mut String, reg_typ: &RegisterType) -> std::fmt::Result {
        use std::fmt::Write;

        match *reg_typ {
            RegisterType::Any
            | RegisterType::Bytes
            | RegisterType::Number
            | RegisterType::Boolean => write!(w, "{:?}", reg_typ)?,
            RegisterType::Trivial(t) => write!(w, "{:?}", t)?,
            RegisterType::Byts(p) => {
                w.push_str("Bytes(");
                self.display_cnst(w, p)?;
                w.push(')');
            }
            RegisterType::Int(v) => write!(w, "Int({})", v)?,
            RegisterType::Bool(v) => write!(w, "Boolean({})", v)?,
            RegisterType::FnPtr(f) => write!(w, "FnPtr(@{})", f)?,
            RegisterType::Record(r) => {
                todo!()
            }
        };

        Ok(())
    }

    fn display_cnst(&self, w: &mut String, cnst: ConstantId) -> std::fmt::Result {
        use std::fmt::Write;

        let payload = todo!();

        if let Ok(str) = std::str::from_utf8(payload) {
            write!(w, "{:?}", str)?;
            return Ok(());
        }

        let (pre, bytes, post) = unsafe { payload.align_to() };

        if pre.is_empty() && post.is_empty() {
            if let Ok(s) = String::from_utf16(bytes) {
                write!(w, "{:?}", s)?;
                return Ok(());
            }
        }

        write!(w, "{:?}", payload)
    }
}
