//! Contains data structures that are necessary to memorize information about
//! records - their history of transitions, taking a view on them, and narrowing
//! that view based on facts. All terminology is briefly explained here, but
//! explained in depth at [the corresonding blog post][blog post]
//!
//! [blog post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

use derive_more::{Deref, DerefMut, Display};
use std::sync::{Arc, Mutex};
use string_interner::{StringInterner, Symbol};

use rustc_hash::FxHashMap;

use crate::id::{IdCompat, LiftedCtx, SymbolicCtx};
use crate::isa::{InternalSlot, TrivialItem};
use crate::UnwrapNone;

type AllocationId = crate::id::AllocationId<LiftedCtx>;
type ConstantId = crate::id::ConstantId<SymbolicCtx>;
type RegisterId = crate::id::RegisterId<LiftedCtx>;
/// The ID of a function whose argument types are not yet known.
type DynFnId = crate::id::FunctionId<LiftedCtx>;
type UnionId = crate::id::UnionId<LiftedCtx>;
type WorkRecordKey = crate::isa::RecordKey<LiftedCtx>;

#[derive(Clone, Copy, Hash)]
enum RecordKey {
    Key(RegisterType),
    Slot(InternalSlot),
}

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
    Union(UnionId),
}

#[derive(Debug, Clone, Copy)]
pub enum LookingUpStatus {
    Nothing,
    RecordKey(WorkRecordKey),
    Register(RegisterId),
    Constant(ConstantId),
}

#[derive(Default)]
struct LookingUp {
    status: Arc<Mutex<LookingUpStatus>>,
}

impl LookingUp {
    pub fn get(&self) -> LookingUpStatus {
        match self.status.try_lock() {
            Ok(guard) => *guard,
            Err(_) => panic!("error getting lookup status"),
        }
    }

    pub fn set(&self, new: LookingUpStatus) {
        match self.status.try_lock() {
            Ok(mut guard) => *guard = new,
            Err(_) => panic!("error setting lookup status"),
        }
    }
}

impl Clone for LookingUp {
    fn clone(&self) -> Self {
        let current_status = self.get();
        assert!(
            matches!(current_status, LookingUpStatus::Nothing),
            "should not be looking up anything while cloning"
        );

        Default::default()
    }
}

impl Default for LookingUpStatus {
    fn default() -> Self {
        Self::Nothing
    }
}

#[derive(Clone, Default)]
struct RecordBag {
    counter: AllocationId,
    records: FxHashMap<AllocationId, Union<Facts<Fact>>>,
}

#[derive(Clone, Deref, DerefMut, PartialEq, Eq)]
struct Union<T>(Vec<T>);

#[derive(Clone, Deref, DerefMut)]
struct Facts<T>(Vec<T>);

#[derive(Clone)]
enum Fact {
    Set {
        key: RecordKey,
        value: RegisterType,
        inst_idx: usize,
    },
    Remove {
        key: RecordKey,
        inst_idx: usize,
    },
}

impl Fact {
    fn key(&self) -> RecordKey {
        match self {
            Fact::Set { key, .. } | Fact::Remove { key, .. } => *key,
        }
    }

    fn inst_idx(&self) -> usize {
        match self {
            Fact::Set { inst_idx, .. } | Fact::Remove { inst_idx, .. } => *inst_idx,
        }
    }
}

impl RecordBag {
    pub fn new_record(&mut self) -> AllocationId {
        let id = self.counter.next_and_mut();
        self.records
            .insert(id, Union(vec![Facts(vec![])]))
            .expect_free();
        id
    }

    pub fn record_facts_of<F>(
        &self,
        record: AllocationId,
        field: RecordKey,
        keys_eq: F,
    ) -> Union<&Fact>
    where
        F: Fn(RecordKey, RecordKey) -> bool,
    {
        let record = self.records.get(&record).unwrap();

        let mut field_facts = vec![];
        for facts in record.iter() {
            for fact in facts.iter().rev() {
                if keys_eq(fact.key(), field) {
                    field_facts.push(fact);
                    break;
                }
            }
        }

        Union(field_facts)
    }

    pub fn record_fact_remove(&mut self, record: AllocationId, field: RecordKey, inst_idx: usize) {
        let record = self.records.get_mut(&record).unwrap();

        for facts in record.iter_mut() {
            facts.push(Fact::Remove {
                key: field,
                inst_idx,
            });
        }
    }

    pub fn record_fact_set(
        &mut self,
        record: AllocationId,
        field: RecordKey,
        value: RegisterType,
        inst_idx: usize,
    ) {
        let record = self.records.get_mut(&record).unwrap();

        for facts in record.iter_mut() {
            facts.push(Fact::Set {
                key: field,
                value,
                inst_idx,
            });
        }
    }

    pub fn record_has_field<F>(
        &self,
        record: AllocationId,
        field: RecordKey,
        keys_eq: F,
    ) -> Option<bool>
    where
        F: Fn(RecordKey, RecordKey) -> bool,
    {
        let record = self.records.get(&record).unwrap();
        debug_assert!(record.len() >= 1);

        // a record:
        // - definitively has a key `Some(true)`
        // - definitively does not have a key `Some(false)`
        // - may or may not have a key `None`

        let facts = record.iter();

        // for every list of facts
        let mut fact_paths_that_have_key = facts.map(|facts| {
            // determine if this line of facts has a value at the key
            let is_set = (facts.iter().rev())
                .find(|f| keys_eq(f.key(), field))
                .map(|f| matches!(f, Fact::Set { .. }));

            match is_set {
                // if it does not have an entry for this key,
                // it definitively does not have the key
                None => false,
                // if it has a `set` entry for the key,
                // it definitively has have the key
                Some(true) => true,
                // if it has a `remove` entry for the key,
                // it definitively does not have the key
                Some(false) => false,
            }
        });

        // for a record to definitively have or not have a key,
        // all lines of facts must return the same answer on whether or not it has a key

        // get the initial line of facts
        let initial = fact_paths_that_have_key.next().unwrap();

        // check if all other lines of facts are the same
        let all_same = fact_paths_that_have_key.fold(initial, |a, b| a == b);

        // if any other line of facts is different,
        if !all_same {
            // we both have and don't have a key
            None
        } else {
            // we definitively either have or don't have a key
            Some(initial)
        }
    }
}

pub struct Subset<'a> {
    original: &'a mut TypeBag,
    child: TypeBag,
    pub inst_idx: usize,
    mapping: (),
}

impl<'a> Subset<'a> {
    /// Produces the representation of the subset typebag
    pub fn child(&self) -> TypeBag {
        self.child.clone()
    }

    /// Given a register type in a `type_bag` which is expected to be similar to
    /// this subset's `child`, this will perform all necessary work to
    pub fn update_reg(&mut self, type_bag: &TypeBag, target_reg: RegisterId) {
        todo!()
    }

    /// Given a single type in a, Deref `type_bag` which is expected to be similar to
    /// this subset's `child`, this will perform all necessary work to bring
    /// that type into the scope of this subset.
    pub fn update_typ(&mut self, type_bag: &TypeBag, typ: RegisterType) -> RegisterType {
        todo!()
    }
}

#[derive(Default, Clone)]
pub struct UnionInterner {
    counter: UnionId,
    union_map: FxHashMap<UnionId, Union<RegisterType>>,
}

impl UnionInterner {
    fn intern(&mut self, union: Union<RegisterType>) -> UnionId {
        debug_assert!(union.len() >= 2);
        for (k, v) in self.union_map.iter() {
            if v == &union {
                return *k;
            }
        }

        let id = self.counter.next_and_mut();
        self.union_map.insert(id, union).expect_free();
        id
    }

    fn unintern(&self, id: UnionId) -> &Union<RegisterType> {
        let union = self.union_map.get(&id).unwrap();
        debug_assert!(union.len() >= 2);
        union
    }
}

#[derive(Clone)]
pub struct TypeBag {
    registers: FxHashMap<RegisterId, RegisterType>,
    records: RecordBag,
    unions: UnionInterner,
    constants: StringInterner<ConstantId>,
    status: LookingUp,
}

impl TypeBag {
    pub fn looking_up(&self) -> LookingUpStatus {
        self.status.get()
    }

    pub fn new_record(&mut self, register: RegisterId) {
        let id = self.records.new_record();
        self.registers
            .insert(register, RegisterType::Record(id))
            .expect_free();
    }

    pub fn record_get_field(&mut self, record: RegisterId, field: WorkRecordKey) -> RegisterType {
        let typ = self.get(record);

        let record = match typ {
            RegisterType::Record(id) => id,
            _ => panic!("not of type record"),
        };

        let field = match field {
            WorkRecordKey::Prop(register) => RecordKey::Key(self.get(register)),
            WorkRecordKey::Slot(slot) => RecordKey::Slot(slot),
        };

        let keys_eq = |a, b| {
            use RecordKey::*;
            match (a, b) {
                (Slot(a), Slot(b)) => a == b,
                (Key(a), Key(b)) => self.typ_eq(a, b),
                _ => false,
            }
        };

        let facts = self.records.record_facts_of(record, field, keys_eq);

        // possible lists of facts => the return type (or action)
        // - [] -- empty union, meaning no facts => Any
        // - [set K => V] -- simple, expected case => V
        // - [set K_1 => V_1, set K_2 => V_2] -- divergent case => V_1 | V_2
        // - [remove K] -- does not exist => perform an error
        //   ^ the removal of a key in a record in JSSAT is defined as an error,
        //     while languages above JSSAT IR may have their own semantics

        if facts.len() == 0 {
            return RegisterType::Any;
        }

        let mut types = Vec::new();
        for fact in facts.iter() {
            match fact {
                Fact::Remove { .. } => {
                    panic!("attempted to get field of record that does not exist")
                }
                Fact::Set { value, .. } => types.push(*value),
            };
        }

        if types.len() == 1 {
            types[0]
        } else {
            let union_id = self.unions.intern(Union(types));
            RegisterType::Union(union_id)
        }
    }

    pub fn record_set_field(
        &mut self,
        record: RegisterId,
        field: WorkRecordKey,
        value: Option<RegisterType>,
        inst_idx: usize,
    ) {
        let typ = self.get(record);

        let record = match typ {
            RegisterType::Record(id) => id,
            _ => panic!("not of type record"),
        };

        let field = match field {
            WorkRecordKey::Prop(register) => RecordKey::Key(self.get(register)),
            WorkRecordKey::Slot(slot) => RecordKey::Slot(slot),
        };

        match value {
            Some(value) => self.records.record_fact_set(record, field, value, inst_idx),
            None => self.records.record_fact_remove(record, field, inst_idx),
        };
    }

    pub fn record_has_field(&self, record: RegisterId, field: WorkRecordKey) -> Option<bool> {
        let typ = self.get(record);

        let record = match typ {
            RegisterType::Record(id) => id,
            _ => panic!("not of type record"),
        };

        let field = match field {
            WorkRecordKey::Prop(register) => RecordKey::Key(self.get(register)),
            WorkRecordKey::Slot(slot) => RecordKey::Slot(slot),
        };

        let keys_eq = |a, b| {
            use RecordKey::*;
            match (a, b) {
                (Slot(a), Slot(b)) => a == b,
                (Key(a), Key(b)) => self.typ_eq(a, b),
                _ => false,
            }
        };

        self.records.record_has_field(record, field, keys_eq)
    }

    pub fn assign_type(&mut self, register: RegisterId, typ: RegisterType) {
        self.registers.insert(register, typ).expect_free();
    }

    pub fn intern_constant(&mut self, payload: &[u8]) -> ConstantId {
        // we don't really want or care about strigns specifically
        // but the API wants strings
        // TODO: is this safe?
        let str = unsafe { std::str::from_utf8_unchecked(payload) };
        self.constants.get_or_intern(str)
    }

    pub fn unintern_const(&self, id: ConstantId) -> &[u8] {
        self.constants.resolve(id).unwrap().as_bytes()
    }

    pub fn get(&self, register: RegisterId) -> RegisterType {
        self.status.set(LookingUpStatus::Register(register));
        let typ = *self.registers.get(&register).unwrap();
        self.status.set(LookingUpStatus::Nothing);
        typ
    }

    pub fn get_fnptr(&self, register: RegisterId) -> DynFnId {
        if let RegisterType::FnPtr(f) = self.get(register) {
            f
        } else {
            panic!("expected `get_fnptr` to give a register that's a fnptr");
        }
    }

    pub fn display(&self, register: RegisterId) -> String {
        DisplayContext {
            types: self,
            records_shown: Vec::new(),
        }
        .display(register)
    }

    pub fn subset(
        &mut self,
        src_args: &[RegisterId],
        target_args: &[RegisterId],
        inst_idx: usize,
    ) -> Subset {
        debug_assert_eq!(src_args.len(), target_args.len());
        todo!()
    }

    pub fn typ_eq(&self, a: RegisterType, b: RegisterType) -> bool {
        todo!()
    }
}

impl Default for TypeBag {
    fn default() -> Self {
        TypeBag {
            registers: Default::default(),
            unions: Default::default(),
            records: Default::default(),
            constants: StringInterner::new(),
            status: Default::default(),
        }
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
    records_shown: Vec<AllocationId>,
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
            RegisterType::Record(r) => todo!(),
            RegisterType::Union(_) => todo!(),
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

impl Symbol for ConstantId {
    fn to_usize(self) -> usize {
        self.raw_value()
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        ConstantId::try_new_with_value_raw_const(int)
    }
}
