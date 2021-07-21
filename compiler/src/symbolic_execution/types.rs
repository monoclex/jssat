use rustc_hash::FxHashMap;

use crate::id::{LiftedCtx, SymbolicCtx};
use crate::isa::{InternalSlot, RecordKey, TrivialItem};
use crate::UnwrapNone;

type AllocationId = crate::id::AllocationId<SymbolicCtx>;
type FunctionId = crate::id::FunctionId<SymbolicCtx>;
type ConstantId = crate::id::ConstantId<SymbolicCtx>;
type RegisterId = crate::id::RegisterId<SymbolicCtx>;
type ShapeId = crate::id::ShapeId<SymbolicCtx>;
/// The ID of a function whose argument types are not yet known.
type DynFnId = crate::id::FunctionId<LiftedCtx>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegisterType {
    Any,
    Trivial(TrivialItem),
    Bytes,
    Number,
    Boolean,
    FnPtr(DynFnId),
    Record(AllocationId),
}

pub enum ShapeKey {
    Str(ConstantId),
    Slot(InternalSlot),
}

pub type ShapeValueType = RegisterType;

// pub enum ShapeValueType {
//     Any,
//     Trivial(TrivialItem),
//     Bytes,
//     Number,
//     Boolean,
//     FnPtr(DynFnId),
//     // NOTE: this might seem to make more sense to be `Record(ShapeId)`, but in
//     // practice this doesnt help.
//     Record(AllocationId),
// }

pub struct Shape {
    fields: FxHashMap<ShapeKey, ShapeValueType>,
}

pub struct TypeBag {
    registers: FxHashMap<RegisterId, RegisterType>,
    shapes: FxHashMap<ShapeId, Shape>,
}

impl TypeBag {
    pub fn assign_type(&mut self, register: RegisterId, typ: RegisterType) {
        self.registers
            .insert(register, typ)
            .expect_none("should not have duplicate type for register");
    }
}

impl Default for TypeBag {
    fn default() -> Self {
        TypeBag {
            registers: Default::default(),
            shapes: Default::default(),
        }
    }
}
