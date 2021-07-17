use std::{collections::VecDeque, iter::FromIterator};

use crate::{id::*, poor_hashmap::PoorMap};
use petgraph::{visit::EdgeRef, Directed, EdgeDirection};
use rustc_hash::{FxHashMap, FxHashSet};

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

    pub fn wipe(&mut self) {
        self.map.clear();
    }

    pub fn remove_prop(&mut self, key: &ShapeKey) {
        self.map.remove(key);
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
                todo!("cannot unify different props yet: {:?}, {:?}", v, v_dest)
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
        matches!(self, ShapeKey::Str(_) | ShapeKey::InternalSlot(_))
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
    pub fn gen_id(&mut self) -> RegisterId<C> {
        let mut none = RegisterId::default();
        for (k, _) in self.registers.iter() {
            if k.value() >= none.value() {
                none = RegisterId::new_with_value(k.value() + 1);
            }
        }
        none
    }

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

    pub fn remove_alloc(&mut self, id: AllocationId<NoContext>) {
        self.allocations.remove(&id);
        // not having an allocation is really bad
        // an empty reord is better
        let empty_rec_shape = self.insert_shape(RecordShape::default());
        self.allocations.insert(id, vec![empty_rec_shape]);
    }

    pub fn get(&self, register: RegisterId<C>) -> &ValueType {
        self.registers.get(&register).unwrap()
    }

    pub fn get_shape(&self, allocation: AllocationId<NoContext>) -> &RecordShape {
        self.get_shape_by_id(self.get_shape_id_of_alloc(allocation))
    }

    pub fn try_get_shape(&self, allocation: AllocationId<NoContext>) -> Option<&RecordShape> {
        self.try_get_shape_by_id(self.get_shape_id_of_alloc(allocation))
    }

    pub fn get_shapes(&self, allocation: AllocationId<NoContext>) -> &Vec<ShapeId<C>> {
        self.allocations.get(&allocation).unwrap()
    }

    pub fn get_shape_id_of_alloc(&self, allocation: AllocationId<NoContext>) -> &ShapeId<C> {
        let shapes = self.allocations.get(&allocation).unwrap();
        shapes.last().unwrap()
    }

    pub fn get_shape_by_id(&self, shape_id: &ShapeId<C>) -> &RecordShape {
        self.try_get_shape_by_id(shape_id).unwrap()
    }

    pub fn try_get_shape_by_id(&self, shape_id: &ShapeId<C>) -> Option<&RecordShape> {
        self.shapes.get(shape_id)
    }

    pub fn get_shape_by_id_mut(&mut self, shape_id: &ShapeId<C>) -> &mut RecordShape {
        self.shapes.get_mut(shape_id).unwrap()
    }

    pub fn registers<'me>(&'me self) -> impl Iterator<Item = RegisterId<C>> + 'me {
        self.registers.keys().copied()
    }

    pub fn allocations(&self) -> Vec<(AllocationId<NoContext>, &Vec<ShapeId<C>>)> {
        // TODO: this is a hack that iterates over the structs in order of least dependencies to most dependencies
        // this combats structs that have an inner struct but are defined first, although it doesn't combat
        // cyclic structs. cyclicity is to-be-handled: soon:tm: :)
        type Graph = petgraph::graph::DiGraph<AllocationId<NoContext>, ()>;
        let mut g = Graph::new();

        let mut map = FxHashMap::default();
        let mut map2 = FxHashMap::default();

        // make allocations into nodes
        for (k, v) in self.allocations.iter() {
            let node = g.add_node(*k);
            map.insert(*k, node);
            map2.insert(node, *k);
        }

        // edge from every allocation to the allocations it uses
        for (k, v) in self.allocations.iter() {
            let src_node = map.get(k).unwrap();
            for s in v.iter() {
                for (rk, rv) in self.get_shape(*k).fields() {
                    if let ValueType::Record(a) = rv {
                        let target_node = map.get(a).unwrap();
                        g.add_edge(*src_node, *target_node, ());
                    }
                }
            }
        }

        // now we need to iterate through this and get the least dependency structs to
        // the ones with most deps
        // someone called this a "post-order depth first search"

        // now there is no root node so we start everywhere, climb up, and add that to a list of root nodes
        let mut roots = FxHashSet::default();

        // start everywhere on the graph
        for (_, n) in map.iter() {
            let mut n = *n;
            // while there is a parent edge
            while let Some(edge) = g.edges_directed(n, EdgeDirection::Incoming).next() {
                // climb up it
                n = edge.source();
            }
            // note: we throw away some of the edges when climbing up
            // that's because since we start everywhere, we're going to start at that parent edge too
            // so it doesn't matter

            // anyways once we're here, we now have the root node
            roots.insert(n);
        }

        // now we have all roots
        // treat this as a series of nodes we need to find the children to
        // we'll go through every node, find the children, and try to find more children
        // then we'll insert the node back into the queue
        let mut search = VecDeque::from_iter(roots);
        let mut searched = Vec::new();

        while let Some(n) = search.pop_front() {
            for edge in g.edges_directed(n, EdgeDirection::Outgoing) {
                search.push_back(edge.target());
            }
            searched.push(n);
        }

        // remove dup nodes
        let mut remove_dups = FxHashSet::default();
        let mut srechd = Vec::new();
        for i in searched {
            if remove_dups.insert(i) {
                srechd.push(i);
            }
        }

        srechd
            .iter()
            .rev()
            .map(|n| {
                let alloc = *map2.get(n).unwrap();
                let shapes = self.allocations.get(&alloc).unwrap();
                (alloc, shapes)
            })
            .collect()
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
            ValueType::ExactInteger(_)
            | ValueType::ExactString(_)
            | ValueType::Bool(_)
            | ValueType::FnPtr(_)
            | ValueType::Null
            | ValueType::Undefined => true,
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
            | ValueType::Word
            | ValueType::Undefined
            | ValueType::Null
            | ValueType::FnPtr(_) => typ,
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
