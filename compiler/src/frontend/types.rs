use crate::{id::*, poor_hashmap::PoorMap};
use rustc_hash::FxHashMap;

use super::type_annotater::{InvocationArgs, ValueType};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RecordShape {
    map: FxHashMap<ShapeKey, ValueType>,
}

impl RecordShape {
    pub fn add_prop(&self, key: ShapeKey, value: ValueType) -> RecordShape {
        let mut key_value_map = self.map.clone();
        key_value_map.insert(key, value);
        RecordShape { map: key_value_map }
    }

    pub fn type_at_key<'me>(&'me self, key: &ShapeKey) -> &'me ValueType {
        self.map.get(key).unwrap()
    }

    pub fn union<C: ContextTag>(&self, other: &RecordShape, reg_map: &RegMap<C>) -> RecordShape {
        let mut map = self.map.clone();
        let mut unmerged = vec![];

        // both maps should contain the same properties
        for (k, v) in other.map.iter() {
            match map.get_mut(k) {
                Some(_) => {
                    unmerged.push((k, v));
                }
                None => {
                    map.insert(k.clone(), v.clone());
                }
            };
        }

        // now we're left with conflicts
        for (k, v) in unmerged {
            let v_dest = map.get(k).unwrap();

            if v == v_dest {
                // there is no conflict, we're good
            } else {
                todo!("cannot unify different props yet")
            }
        }

        RecordShape { map }
    }

    pub fn fields(&self) -> impl Iterator<Item = (&ShapeKey, &ValueType)> {
        self.map.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ShapeKey {
    String,
    Str(Vec<u8>),
    InternalSlot(&'static str),
}

impl ShapeKey {
    pub fn is_const(&self) -> bool {
        matches!(self, ShapeKey::Str(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegMap<C: ContextTag> {
    registers: FxHashMap<RegisterId<C>, ValueType>,
    allocation_id_gen: AllocationId<NoContext>,
    allocations: FxHashMap<AllocationId<NoContext>, Vec<ShapeId<C>>>,
    shape_id_gen: ShapeId<C>,
    shapes: FxHashMap<ShapeId<C>, RecordShape>,
}

impl<C: ContextTag> RegMap<C> {
    pub fn insert(&mut self, register: RegisterId<C>, typ: ValueType) {
        if let ValueType::Record(alloc) = &typ {
            debug_assert!(
                self.allocations.contains_key(&alloc.map_context()),
                "failed assertion: {:?}",
                &self
            );
        }

        self.registers.insert(register, typ);
    }

    pub fn insert_alloc(&mut self) -> AllocationId<NoContext> {
        let shape = self.insert_shape(RecordShape::default());

        let alloc_id = self.allocation_id_gen.next_and_mut();
        self.allocations.insert(alloc_id, vec![shape]);
        alloc_id
    }

    pub fn alloc_alloc(&mut self, alloc_id: AllocationId<NoContext>) {
        self.allocations.insert(alloc_id, vec![]);
    }

    pub fn insert_shape(&mut self, shape: RecordShape) -> ShapeId<C> {
        let shape_id = self.shape_id_gen.next_and_mut();
        self.shapes.insert(shape_id, shape);
        shape_id
    }

    pub fn assign_new_shape(&mut self, allocation: AllocationId<NoContext>, shape: ShapeId<C>) {
        self.allocations.get_mut(&allocation).unwrap().push(shape);
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

    pub fn get_shape(&self, allocation: AllocationId<NoContext>) -> &RecordShape {
        let shapes = self.allocations.get(&allocation).unwrap();
        self.get_shape_by_id(shapes.last().unwrap())
    }

    pub fn get_shape_by_id(&self, shape_id: &ShapeId<C>) -> &RecordShape {
        self.shapes.get(shape_id).unwrap()
    }

    pub fn registers<'me>(&'me self) -> impl Iterator<Item = RegisterId<C>> + 'me {
        self.registers.keys().copied()
    }

    pub fn allocations(
        &self,
    ) -> impl Iterator<Item = (&AllocationId<NoContext>, &Vec<ShapeId<C>>)> {
        self.allocations.iter()
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

    pub fn is_const_shape(&self, allocation: AllocationId<NoContext>) -> bool {
        let shape = self.get_shape(allocation);
        for (k, v) in shape.map.iter() {
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
        self.is_simple_typ(self.get(register))
    }

    pub fn is_simple_typ(&self, typ: &ValueType) -> bool {
        match typ {
            ValueType::Runtime
            | ValueType::ExactInteger(_)
            | ValueType::Bool(_)
            | ValueType::ExactString(_) => true,
            ValueType::Record(alloc) => {
                let shape = self.get_shape(*alloc);
                self.is_simple_shape(shape)
            }
            _ => false,
        }
    }

    fn is_simple_shape(&self, shape: &RecordShape) -> bool {
        shape
            .fields()
            .fold(true, |current, (_, v)| self.is_simple_typ(v) && current)
    }

    pub fn prepare_invocation<C2: ContextTag>(
        &self,
        arguments: impl Iterator<Item = (RegisterId<C>, RegisterId<C2>)>,
    ) -> InvocationArgs<C2> {
        let mut map = RegMap::default();
        let mut alloc_map = AllocIdMap::default();

        for (argument, target) in arguments {
            let typ = self.get(argument).clone();
            let typ = self.map_type(typ, &mut map, &mut alloc_map);

            map.insert(target, typ);
        }

        InvocationArgs(map)
    }

    fn map_type<C2: ContextTag>(
        &self,
        typ: ValueType,
        target: &mut RegMap<C2>,
        alloc_map: &mut AllocIdMap<NoContext, NoContext>,
    ) -> ValueType {
        match typ {
            ValueType::Any
            | ValueType::Runtime
            | ValueType::String
            | ValueType::ExactString(_)
            | ValueType::Number
            | ValueType::ExactInteger(_)
            | ValueType::Boolean
            | ValueType::Bool(_)
            | ValueType::Pointer(_)
            | ValueType::Word => typ,
            ValueType::Record(orig_alloc_id) => {
                let (alloc_id, is_new) = alloc_map.map_is_new(orig_alloc_id);

                if is_new {
                    // if the type is new, we need to make a shape and allocation things
                    target.alloc_alloc(alloc_id);
                    let current_shape = self.get_shape(orig_alloc_id);
                    let mut new_shape = RecordShape::default();

                    for (k, v) in current_shape.fields() {
                        // TODO: this is a pitfall
                        new_shape = new_shape
                            .add_prop(k.clone(), self.map_type(v.clone(), target, alloc_map));
                    }

                    let shape_id = target.insert_shape(new_shape);
                    target.assign_new_shape(alloc_id, shape_id);
                }

                ValueType::Record(alloc_id)
            }
        }
    }

    pub fn duplicate_with_allocations<C2: ContextTag>(&self) -> RegMap<C2> {
        let mut target = RegMap::default();

        target.allocation_id_gen = self.allocation_id_gen.clone();
        target.allocations = self
            .allocations
            .clone()
            .into_iter()
            .map(|(k, v)| {
                (
                    k,
                    v.into_iter().map(|s| s.map_context()).collect::<Vec<_>>(),
                )
            })
            .collect();
        target.shape_id_gen = self.shape_id_gen.map_context().clone();
        target.shapes = self
            .shapes
            .clone()
            .into_iter()
            .map(|(k, v)| (k.map_context(), v))
            .collect();

        target
    }
}

impl<C: ContextTag> Default for RegMap<C> {
    fn default() -> Self {
        Self {
            registers: Default::default(),
            allocation_id_gen: Default::default(),
            allocations: Default::default(),
            shape_id_gen: Default::default(),
            shapes: Default::default(),
        }
    }
}

// /// A [`RegMap`], but that derives [`PartialEq`] and [`Eq`] to make it suitable
// /// for use in comparisons.
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct RegMapView<C: ContextTag> {
//     registers: PoorMap<RegisterId<C>, ValueType>,
//     allocation_id_gen: AllocationId<NoContext>,
//     allocations: FxHashMap<AllocationId<NoContext>, Vec<ShapeId<C>>>,
//     shape_id_gen: ShapeId<C>,
//     shapes: FxHashMap<ShapeId<C>, RecordShape>,
// }
