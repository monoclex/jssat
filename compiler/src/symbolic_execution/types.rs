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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, Default)]
pub struct Shape {
    // TODO: private this (it should not be public)
    pub(crate) fields: FxHashMap<ShapeKey, ShapeValueType>,
}

impl Shape {
    pub fn new_with(&self, key: ShapeKey, value: ShapeValueType) -> Shape {
        let mut new = Shape {
            fields: self.fields.clone(),
        };
        new.fields.insert(key, value);
        new
    }

    pub fn get_typ(&self, key: ShapeKey) -> ShapeValueType {
        *self.fields.get(&key).unwrap()
    }
}

#[derive(Debug)]
pub enum LookingUp {
    Nothing,
    ShapeKey(ShapeKey),
    Register(RegisterId),
    Constant(ConstantId),
}

#[derive(Debug, Clone)]
pub struct TypeBag {
    registers: FxHashMap<RegisterId, RegisterType>,
    alloc_counter: Counter<AllocationId>,
    alloc_shapes: FxHashMap<AllocationId, Vec<ShapeId>>,
    shape_counter: Counter<ShapeId>,
    shapes: FxHashMap<ShapeId, Shape>,
    const_counter: Counter<ConstantId>,
    consts: FxHashMap<ConstantId, Vec<u8>>,
    pub looking_up: Arc<Mutex<LookingUp>>,
}

impl TypeBag {
    pub fn new_record(&mut self, register: RegisterId) {
        let alloc = self.alloc_counter.next();
        let shape = Shape::default();
        let shape_id = self.new_shape(shape);
        self.push_shape(alloc, shape_id);
        self.assign_type(register, RegisterType::Record(alloc));
    }

    pub fn append_shape(&mut self, register: RegisterId, shape: Shape) {
        let id = self.new_shape(shape);
        if let RegisterType::Record(a) = self.get(register) {
            self.push_shape(a, id);
        } else {
            panic!("attempted to get record shape on non-record");
        }
    }

    pub fn push_shape(&mut self, alloc: AllocationId, shape: ShapeId) {
        self.alloc_shapes
            .entry(alloc)
            .or_insert_with(Default::default)
            .push(shape);
    }

    fn alloc_allocation(&mut self) -> AllocationId {
        self.alloc_counter.next()
    }

    pub fn new_shape(&mut self, shape: Shape) -> ShapeId {
        // TODO: maybe intern the shape to prevent duplicates?
        // profiling would need to be done if that's worth it at all
        let id = self.shape_counter.next();
        self.shapes
            .insert(id, shape)
            .expect_none("should not have duplicate shapes");
        id
    }

    pub fn assign_type(&mut self, register: RegisterId, typ: RegisterType) {
        self.registers
            .insert(register, typ)
            .expect_none("should not have duplicate type for register");
    }

    pub fn intern_constant(&mut self, payload: &[u8]) -> ConstantId {
        for (id, v) in self.consts.iter() {
            if v == payload {
                return *id;
            }
        }

        let id = self.const_counter.next();
        self.consts.insert(id, payload.to_vec());
        id
    }

    fn with_looking_up<F: FnOnce() -> R, R>(&self, looking_this_up: LookingUp, action: F) -> R {
        let mut looking_up = (self.looking_up.try_lock()).expect("should be contentionless");
        *looking_up = looking_this_up;
        drop(looking_up);
        let result = action();
        let mut looking_up = (self.looking_up.try_lock()).expect("should be contentionless");
        *looking_up = LookingUp::Nothing;
        drop(looking_up);
        result
    }

    pub fn get(&self, register: RegisterId) -> RegisterType {
        self.with_looking_up(LookingUp::Register(register), || {
            *self.registers.get(&register).unwrap()
        })
    }

    pub fn get_field_type(&self, shape: &Shape, key: RecordKey<LiftedCtx>) -> ShapeValueType {
        let key = self.conv_key(key);
        self.with_looking_up(LookingUp::ShapeKey(key), || shape.get_typ(key))
    }

    pub fn record_shape(&self, register: RegisterId) -> &Shape {
        if let RegisterType::Record(a) = self.get(register) {
            let shape_id = self.get_shape_id(a);
            self.get_shape(shape_id)
        } else {
            panic!("attempted to get record shape on non-record");
        }
    }

    pub fn get_fnptr(&self, register: RegisterId) -> DynFnId {
        if let RegisterType::FnPtr(f) = self.get(register) {
            f
        } else {
            panic!("attempted to get function pointer of non-fnptr");
        }
    }

    pub fn get_shape_id(&self, alloc: AllocationId) -> ShapeId {
        let shape_history = self.alloc_shapes.get(&alloc).unwrap();
        debug_assert!(!shape_history.is_empty());

        *shape_history.last().unwrap()
    }

    pub fn get_shape(&self, shape: ShapeId) -> &Shape {
        self.shapes.get(&shape).unwrap()
    }

    pub fn conv_key(&self, key: RecordKey<LiftedCtx>) -> ShapeKey {
        match key {
            RecordKey::Prop(r) => match self.get(r) {
                RegisterType::Byts(s) => ShapeKey::Str(s),
                RegisterType::Any
                | RegisterType::Trivial(_)
                | RegisterType::Bytes
                | RegisterType::Number
                | RegisterType::Int(_)
                | RegisterType::Boolean
                | RegisterType::Bool(_)
                | RegisterType::FnPtr(_)
                | RegisterType::Record(_) => panic!("unsupported key at this time"),
            },
            RecordKey::Slot(s) => ShapeKey::Slot(s),
        }
    }

    pub fn unintern_const(&self, id: ConstantId) -> &Vec<u8> {
        self.with_looking_up(LookingUp::Constant(id), || self.consts.get(&id).unwrap())
    }

    pub fn display(&self, register: RegisterId) -> String {
        let mut s = String::new();

        match self.registers.get(&register) {
            Some(t) => match self.display_typ(&mut s, t) {
                Ok(_) => {}
                Err(e) => {
                    let reason = e.to_string();
                    s.push_str(&reason);
                }
            },
            None => s.push('?'),
        };

        s
    }

    fn display_typ(&self, w: &mut String, reg_typ: &RegisterType) -> std::fmt::Result {
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
                w.push_str("{ ");

                let shape = (self.alloc_shapes.get(&r))
                    .and_then(|h| h.last())
                    .and_then(|id| self.shapes.get(id));

                if let Some(shape) = shape {
                    for (key, typ) in shape.fields.iter() {
                        match *key {
                            ShapeKey::Str(s) => self.display_cnst(w, s)?,
                            ShapeKey::Slot(s) => write!(w, "[[{}]]", s)?,
                        };

                        w.push_str(": ");

                        self.display_typ(w, typ)?;
                        w.push_str(", ");
                    }
                } else {
                    w.push('?');
                }

                w.push_str(" }");
            }
        };

        Ok(())
    }

    fn display_cnst(&self, w: &mut String, cnst: ConstantId) -> std::fmt::Result {
        use std::fmt::Write;

        let payload = match self.consts.get(&cnst) {
            Some(p) => p,
            None => {
                w.push('?');
                return Ok(());
            }
        };

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

    /// Given a set of registers, will pull out the types of those registers
    /// and create a new `TypeBag` containing only the types of the registers
    /// specified.
    ///
    /// This is useful for continuing execution of a function after a block.
    pub fn extract(&self, regs: &[RegisterId]) -> Self {
        self.extract_map(regs.iter().map(|r| (*r, *r)))
    }

    /// Given a set of registers, it will extract the type out of them and
    /// place those types into the register specified (RHS in the tuple)
    pub fn extract_map(&self, regs: impl Iterator<Item = (RegisterId, RegisterId)>) -> Self {
        // TODO: coudl clean this up but EH!
        let mut new = TypeBag::default();

        let mut alloc_map = FxHashMap::default();
        let mut need_to_shape = VecDeque::new();

        for (s_reg, d_reg) in regs {
            let typ = self.get(s_reg);

            // TODO: don't duplicate this code with the hunk at the bottom
            let new_tp = match typ {
                RegisterType::Record(a) => {
                    need_to_shape.push_back(a);
                    let new_a = new.alloc_allocation();

                    alloc_map.insert(a, new_a).expect_none("");
                    RegisterType::Record(new_a)
                }
                RegisterType::Byts(c) => {
                    RegisterType::Byts(new.intern_constant(self.unintern_const(c)))
                }
                other => other,
            };

            new.assign_type(d_reg, new_tp);
        }

        let mut shape_map = FxHashMap::default();

        while let Some(alloc_id) = need_to_shape.pop_front() {
            let shape_id = self.get_shape_id(alloc_id);

            let mapped_alloc_id = *alloc_map.get(&alloc_id).unwrap();

            if let Some(id) = shape_map.get(&shape_id) {
                new.push_shape(mapped_alloc_id, *id);
                continue;
            }

            let mut new_shape = Shape::default();
            let shape = self.get_shape(shape_id);

            for (&k, &v) in shape.fields.iter() {
                let k = match k {
                    ShapeKey::Str(c) => ShapeKey::Str(new.intern_constant(self.unintern_const(c))),
                    ShapeKey::Slot(s) => ShapeKey::Slot(s),
                };

                // TOPDO: don't duplicate this code with the hunk at the top
                let v = match v {
                    RegisterType::Record(a) => {
                        if let Some(a) = alloc_map.get(&a) {
                            RegisterType::Record(*a)
                        } else {
                            let new_alloc_id = new.alloc_allocation();
                            need_to_shape.push_back(a);
                            alloc_map.insert(a, new_alloc_id);
                            RegisterType::Record(new_alloc_id)
                        }
                    }
                    RegisterType::Byts(c) => {
                        RegisterType::Byts(new.intern_constant(self.unintern_const(c)))
                    }
                    other => other,
                };

                new_shape.fields.insert(k, v);
            }

            let new_shape_id = new.new_shape(new_shape);
            shape_map.insert(shape_id, new_shape_id);
            new.push_shape(mapped_alloc_id, new_shape_id);
        }

        new
    }

    // TODO: deduplicate this code
    pub fn pull_type_into(&self, typ: RegisterType, into: &mut TypeBag) -> RegisterType {
        let mut alloc_map = FxHashMap::default();
        let mut need_to_shape = VecDeque::new();

        // TODO: don't duplicate this code with the hunk at the bottom
        let new_tp = match typ {
            RegisterType::Record(a) => {
                need_to_shape.push_back(a);
                let new_a = into.alloc_allocation();

                alloc_map.insert(a, new_a);
                RegisterType::Record(new_a)
            }
            RegisterType::Byts(c) => {
                RegisterType::Byts(into.intern_constant(self.unintern_const(c)))
            }
            other => other,
        };

        let mut shape_map = FxHashMap::default();

        while let Some(alloc_id) = need_to_shape.pop_front() {
            let shape_id = self.get_shape_id(alloc_id);

            let mapped_alloc_id = *alloc_map.get(&alloc_id).unwrap();

            if let Some(id) = shape_map.get(&shape_id) {
                into.push_shape(mapped_alloc_id, *id);
                continue;
            }

            let mut new_shape = Shape::default();
            let shape = self.get_shape(shape_id);

            for (&k, &v) in shape.fields.iter() {
                let k = match k {
                    ShapeKey::Str(c) => ShapeKey::Str(into.intern_constant(self.unintern_const(c))),
                    ShapeKey::Slot(s) => ShapeKey::Slot(s),
                };

                // TOPDO: don't duplicate this code with the hunk at the top
                let v = match v {
                    RegisterType::Record(a) => {
                        if let Some(a) = alloc_map.get(&a) {
                            RegisterType::Record(*a)
                        } else {
                            let new_alloc_id = into.alloc_allocation();
                            need_to_shape.push_back(a);
                            alloc_map.insert(a, new_alloc_id);
                            RegisterType::Record(new_alloc_id)
                        }
                    }
                    RegisterType::Byts(c) => {
                        RegisterType::Byts(into.intern_constant(self.unintern_const(c)))
                    }
                    other => other,
                };

                new_shape.fields.insert(k, v);
            }

            let new_shape_id = into.new_shape(new_shape);
            shape_map.insert(shape_id, new_shape_id);
            into.push_shape(mapped_alloc_id, new_shape_id);
        }

        new_tp
    }
}

impl Default for TypeBag {
    fn default() -> Self {
        TypeBag {
            registers: Default::default(),
            alloc_counter: Default::default(),
            alloc_shapes: Default::default(),
            shape_counter: Default::default(),
            shapes: Default::default(),
            const_counter: Default::default(),
            consts: Default::default(),
            looking_up: Arc::new(Mutex::new(LookingUp::Nothing)),
        }
    }
}

impl PartialEq for TypeBag {
    /// Makes sure that every register pairing of two type bags are the same.
    fn eq(&self, other: &Self) -> bool {
        if self.registers.len() != other.registers.len() {
            return false;
        }

        fn try_helper(me: &TypeBag, you: &TypeBag) -> Option<()> {
            let mut shapes_must_match = VecDeque::new();

            for (reg, typ) in me.registers.iter() {
                let oth_typ = you.registers.get(reg)?;

                match (typ, oth_typ) {
                    (RegisterType::Record(a), RegisterType::Record(b)) => {
                        let a = me.get_shape_id(*a);
                        let b = you.get_shape_id(*b);
                        shapes_must_match.push_back((a, b));
                    }
                    (RegisterType::Byts(a), RegisterType::Byts(b)) => {
                        if me.unintern_const(*a) == you.unintern_const(*b) {
                            continue;
                        } else {
                            return None;
                        }
                    }
                    (a, b) if a == b => {
                        continue;
                    }
                    _ => return None,
                }
            }

            let mut equal_shapes = FxHashSet::default();

            while let Some((a, b)) = shapes_must_match.pop_front() {
                // we may have made sure these two shapes have existed before
                if !equal_shapes.insert((a, b)) {
                    continue;
                }

                // now make sure that those shapes were actually equal
                let a = me.get_shape(a);
                let b = you.get_shape(b);
                if a.fields.len() != b.fields.len() {
                    return None;
                }

                for (idx, typ) in a.fields.iter() {
                    let oth_typ = b.fields.get(idx)?;

                    // TODO: dedup this?
                    match (typ, oth_typ) {
                        (RegisterType::Record(a), RegisterType::Record(b)) => {
                            let a = me.get_shape_id(*a);
                            let b = you.get_shape_id(*b);
                            shapes_must_match.push_back((a, b));
                        }
                        (RegisterType::Byts(a), RegisterType::Byts(b)) => {
                            if me.unintern_const(*a) == you.unintern_const(*b) {
                                continue;
                            } else {
                                return None;
                            }
                        }
                        (a, b) if a == b => {
                            continue;
                        }
                        _ => return None,
                    }
                }
            }

            Some(())
        }

        try_helper(self, other).map(|_| true).unwrap_or(false)
    }
}
