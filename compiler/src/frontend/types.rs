use crate::id::*;
use rustc_hash::FxHashMap;

use super::type_annotater::ValueType;

#[derive(Debug, Clone)]
pub struct RecordShape {
    key_value_map: FxHashMap<RecordKey, ValueType>,
}

#[derive(Debug, Clone)]
pub enum RecordKey {
    String,
    Str(Vec<u8>),
}

impl RecordKey {
    pub fn is_const(&self) -> bool {
        matches!(self, RecordKey::Str(_))
    }
}

#[derive(Debug, Clone)]
pub struct RegMap<C> {
    registers: FxHashMap<RegisterId<C>, ValueType>,
    allocations: FxHashMap<AllocationId<C>, Vec<ShapeId<C>>>,
    shapes: FxHashMap<ShapeId<C>, RecordShape>,
}

impl<C: ContextTag> RegMap<C> {
    pub fn insert(&mut self, register: RegisterId<C>, typ: ValueType) {
        if let ValueType::Record(alloc) = &typ {
            debug_assert!(self.allocations.contains_key(&alloc.map_context()));
        }

        self.registers.insert(register, typ);
    }

    pub fn extend(&mut self, other: RegMap<C>) {
        self.registers.extend(other.registers);

        for (k, v) in other.allocations {
            self.allocations
                .entry(k)
                .or_insert_with(Default::default)
                .extend(v);
        }

        self.shapes.extend(other.shapes);
    }

    pub fn get(&self, register: RegisterId<C>) -> &ValueType {
        self.registers.get(&register).unwrap()
    }

    pub fn get_shape(&self, allocation: AllocationId<C>) -> &RecordShape {
        let shapes = self.allocations.get(&allocation).unwrap();
        let shape_id = shapes.last().unwrap();
        self.shapes.get(shape_id).unwrap()
    }

    pub fn is_const(&self, register: RegisterId<C>) -> bool {
        self.is_const_typ(self.get(register))
    }

    pub fn is_const_typ(&self, typ: &ValueType) -> bool {
        match typ {
            ValueType::Any
            | ValueType::Runtime
            | ValueType::String
            | ValueType::Number
            | ValueType::Pointer(_)
            | ValueType::Word
            | ValueType::Boolean => false,
            ValueType::ExactInteger(_) | ValueType::ExactString(_) | ValueType::Bool(_) => true,
            &ValueType::Record(allocation) => self.is_const_shape(allocation.map_context()),
        }
    }

    pub fn is_const_shape(&self, allocation: AllocationId<C>) -> bool {
        let shape = self.get_shape(allocation);
        for (k, v) in shape.key_value_map.iter() {
            if k.is_const() && self.is_const_typ(v) {
                continue;
            } else {
                return false;
            }
        }
        true
    }

    /// States whether the type is "simple" or not. Simple types are extremely
    /// cheap to rebuild at any moment in the IR, and are considered cheaper to
    /// build than to pass around. Passing them around is considered "expensive"
    /// because then we're using more registers than necessary, which cause
    /// performance deficits because the more registers we use the more likely
    /// we'll need to spill onto the stack to generate a function.
    pub fn is_simple(&self, register: RegisterId<C>) -> bool {
        match self.get(register) {
            ValueType::Runtime
            | ValueType::ExactInteger(_)
            | ValueType::Bool(_)
            | ValueType::ExactString(_) => true,
            ValueType::Record(_) => todo!(),
            _ => false,
        }
    }
}

impl<C> Default for RegMap<C> {
    fn default() -> Self {
        Self {
            registers: Default::default(),
            allocations: Default::default(),
            shapes: Default::default(),
        }
    }
}
