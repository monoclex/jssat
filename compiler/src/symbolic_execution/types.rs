//! Contains data structures that are necessary to memorize information about
//! records - their history of transitions, taking a view on them, and narrowing
//! that view based on facts. All terminology is briefly explained here, but
//! explained in depth at [the corresonding blog post][blog post]
//!
//! [blog post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

use derive_more::{Deref, DerefMut, Display};
use lasso::{Key, Rodeo};
use std::sync::{Arc, Mutex};
use swc_common::pass::All;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::collections::FxBiHashMap;
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

impl RecordKey {
    fn into_record_key_eq(self) -> RecordKeyEq {
        match self {
            RecordKey::Key(a) => RecordKeyEq::Key(a),
            RecordKey::Slot(a) => RecordKeyEq::Slot(a),
        }
    }
}

/// usually you don't want equality because to compare if two register tpyes
/// are equal you often need to look them up into a type bag, this is because
/// atm there are no guarantees that two different constant ids don't point to
/// the same constant or that two union ids don't point to different unions
///
/// but to show that i don't care that much about that at the moment, this is
/// a sort of hack to make it explicit "hey im aware what im doing is djumb"
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
enum RecordKeyEq {
    Key(RegisterType),
    Slot(InternalSlot),
}

impl RecordKeyEq {
    fn into_record_key(self) -> RecordKey {
        match self {
            RecordKeyEq::Key(a) => RecordKey::Key(a),
            RecordKeyEq::Slot(a) => RecordKey::Slot(a),
        }
    }
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

impl PartialOrd for RegisterType {
    /// Order's the type of a register based on whether a register type is a
    /// subtype of another. The following subtypes are defined:
    ///
    /// ```text
    /// forall t . t :> t
    /// forall t . Any :> t
    /// Bytes :> Byts
    /// Number :> Int
    /// Boolean :> Bool
    /// ```
    ///
    /// Any undefined relation produces `None`.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::{self, *};
        use RegisterType::*;

        fn algo(lhs: &RegisterType, rhs: &RegisterType) -> Option<Ordering> {
            // forall t . t :> t
            if lhs == rhs {
                return Some(Equal);
            }

            // forall t . Any :> t
            if let Any = rhs {
                return Some(match lhs {
                    Any => Equal,
                    _ => Less,
                });
            }

            // Bytes :> Byts
            if let Bytes = lhs {
                if let Byts(_) = rhs {
                    return Some(Greater);
                }
            }

            // Number :> Int
            if let Number = lhs {
                if let Int(_) = rhs {
                    return Some(Greater);
                }
            }

            // Boolean :> Bool
            if let Boolean = lhs {
                if let Bool(_) = rhs {
                    return Some(Greater);
                }
            }

            None
        }

        algo(self, other).or_else(|| algo(other, self).map(Ordering::reverse))
    }
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
        inst_idx: InstIdx,
    },
    Remove {
        key: RecordKey,
        inst_idx: InstIdx,
    },
}

impl Fact {
    fn key(&self) -> RecordKey {
        match self {
            Fact::Set { key, .. } | Fact::Remove { key, .. } => *key,
        }
    }

    fn inst_idx(&self) -> InstIdx {
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

    /// F : (the field : RecordKey, a fact's key : RecordKey) -> true if should include fact, false if not
    ///
    /// the ordering of F's params is specified so that functions that want to
    /// check for subtying relationships can
    pub fn record_facts_of<F>(
        &self,
        record: AllocationId,
        field: RecordKey,
        should_include_fact: F,
    ) -> Union<&Fact>
    where
        F: Fn(RecordKey, RecordKey) -> bool,
    {
        let record = self.records.get(&record).unwrap();

        let mut field_facts = vec![];
        for facts in record.iter() {
            for fact in facts.iter().rev() {
                if should_include_fact(field, fact.key()) {
                    field_facts.push(fact);
                    break;
                }
            }
        }

        if field_facts.is_empty() {
            println!("we do it aagain buddy");
            for facts in record.iter() {
                for fact in facts.iter().rev() {
                    if should_include_fact(field, fact.key()) {
                        field_facts.push(fact);
                        break;
                    }
                }
            }
        }

        debug_assert!(!field_facts.is_empty(), "should have field facts");
        Union(field_facts)
    }

    pub fn record_fact_remove(
        &mut self,
        record: AllocationId,
        field: RecordKey,
        inst_idx: InstIdx,
    ) {
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
        inst_idx: InstIdx,
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
        let any_different = fact_paths_that_have_key
            .map(|has| initial == has)
            .any(|has| has == false);

        // if any other line of facts is different,
        if any_different {
            // we both have and don't have a key
            None
        } else {
            // we definitively either have or don't have a key
            Some(initial)
        }
    }

    pub fn record_keys(&self, id: AllocationId) -> FxHashSet<RecordKeyEq> {
        let facts = self.records.get(&id).unwrap();

        let keys = (facts.iter())
            .flat_map(|facts| facts.iter())
            .map(|fact| fact.key().into_record_key_eq())
            .collect::<FxHashSet<_>>();

        keys
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ChildRecordInfo {
    id: AllocationId,
    initial_facts: usize,
}

/// we need a number to determine when a fact was added so that optimization
/// passes acting on the list of facts can know at the instruction it's looking
/// at if a fact is true or false
///
/// when calling functions, each record in the typebag has an initial fact set up
/// so that's represented by `prologue`, and during execution if types change
/// *after* an instruction that's represented with `inst`
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum InstIdx {
    Prologue,
    Inst(usize),
    /// only needed as a hack really but oh well
    Epilogue,
}

struct Syncer<'a, R> {
    inst_idx: InstIdx,
    src: &'a TypeBag,
    dest: &'a mut TypeBag,
    resolve: R,
}

trait SyncResolver {
    /// record id in src -> record id in dest
    /// if `None`, that means generate a new record id
    fn map_record_id(&self, id: AllocationId) -> Option<AllocationId>;

    /// if a `None` was returned this will be called with `id` and the generated id
    fn save_record_id(&mut self, src_id: AllocationId, gen_id: AllocationId);
}

// TODO: the resolvers need to be aware of src_fact_init and dest_fact_init
// so that they don't inline already known facts, probably?
// e.g.
// src : [a => 1, b => 2], dest : []
//   sync src -> dest
// src : prev, dest : [a => 1, b => 2]
//   run code
// src : prev, dest : prev ++ [c => 3]
//   sync dest -> src
// src : [a => 1, b => 2, a => 1, b => 2, c => 3], dest : prev
// or something^
struct ResolveLeft<'a>(&'a mut FxBiHashMap<AllocationId, AllocationId>);
struct ResolveRight<'a>(&'a mut FxBiHashMap<AllocationId, AllocationId>);

impl<'a> SyncResolver for ResolveLeft<'a> {
    fn map_record_id(&self, id: AllocationId) -> Option<AllocationId> {
        self.0.get_by_left(&id).cloned()
    }
    fn save_record_id(&mut self, src_id: AllocationId, gen_id: AllocationId) {
        self.0.insert(src_id, gen_id).expect_free();
    }
}
impl<'a> SyncResolver for ResolveRight<'a> {
    fn map_record_id(&self, id: AllocationId) -> Option<AllocationId> {
        self.0.get_by_right(&id).cloned()
    }
    fn save_record_id(&mut self, src_id: AllocationId, gen_id: AllocationId) {
        self.0.insert(gen_id, src_id).expect_free();
    }
}

impl<'a, R: SyncResolver> Syncer<'a, R> {
    pub fn sync_type(&mut self, typ: RegisterType) -> RegisterType {
        match typ {
            RegisterType::Any
            | RegisterType::Trivial(_)
            | RegisterType::Bytes
            | RegisterType::Number
            | RegisterType::Int(_)
            | RegisterType::Boolean
            | RegisterType::Bool(_)
            | RegisterType::FnPtr(_) => typ,
            RegisterType::Byts(id) => {
                let payload = self.src.unintern_const(id);
                let id = self.dest.intern_constant(payload);
                RegisterType::Byts(id)
            }
            RegisterType::Union(u) => {
                let union = self.src.unions.unintern(u);
                let id = self.dest.unions.intern(union.clone());
                RegisterType::Union(id)
            }
            RegisterType::Record(id) => {
                let dest_id = match self.resolve.map_record_id(id) {
                    Some(id) => {
                        // TODO: is this correct logic? maybe we might actually need to sync *more*,
                        // in which case that would be as simple as writing logic to check if the
                        // two records are completely equal on both ends
                        return RegisterType::Record(id);
                    }
                    None => {
                        let gen_id = self.dest.records.new_record();
                        self.resolve.save_record_id(id, gen_id);
                        gen_id
                    }
                };

                // we want to sync up the record from the src to the dest
                // a record is a list of facts, so we want to propagate these facts into the child
                //
                // algo steps:
                // 1. get list of keys
                //    this way we can determine what facts to pull into the child record
                // 2. for each key
                //    a. get history of facts from key
                //    b. discard facts that don't change anything about the record's behavior
                //    c. union all facts into a single type
                //    d. save (key, type) into record
                // 3. return record

                let keys = self.src.records.record_keys(id);

                for key in keys.into_iter().map(|k| k.into_record_key()) {
                    let value_type = match self.src.value_type_at_record_key(id, key) {
                        None => continue,
                        Some(t) => self.sync_type(t),
                    };

                    self.dest
                        .records
                        .record_fact_set(dest_id, key, value_type, self.inst_idx);
                }

                RegisterType::Record(dest_id)
            }
        }
    }
}

pub struct Subset<'a> {
    original: &'a mut TypeBag,
    child: TypeBag,
    // // { original register id |-> child register id }
    // reg_map: FxBiHashMap<RegisterId, RegisterId>,
    // { original record id |-> child record id }
    rec_map: FxBiHashMap<AllocationId, AllocationId>,
    // { record id in original |-> number of facts present at subset creation }
    src_fact_init: FxHashMap<AllocationId, usize>,
    // { record id in dest |-> number of facts present at subset creation }
    dest_fact_init: FxHashMap<AllocationId, usize>,
}

impl<'a> Subset<'a> {
    /// Produces the representation of the subset typebag
    pub fn child(&self) -> TypeBag {
        self.child.clone()
    }

    fn self_to_child_syncer<'b>(&'b mut self, inst_idx: InstIdx) -> Syncer<'b, ResolveLeft> {
        Syncer {
            inst_idx,
            src: self.original,
            dest: &mut self.child,
            resolve: ResolveLeft(&mut self.rec_map),
        }
    }

    fn child_to_self_syncer<'b>(
        &'b mut self,
        child: &'b TypeBag,
        inst_idx: InstIdx,
    ) -> Syncer<'b, ResolveRight> {
        Syncer {
            inst_idx,
            src: child,
            dest: &mut self.original,
            resolve: ResolveRight(&mut self.rec_map),
        }
    }

    // this might just be update_reg
    fn sync_to_child(&mut self, src_reg: RegisterId, child_reg: RegisterId, inst_idx: InstIdx) {
        let typ = self.original.get(src_reg);

        let res_typ = self.self_to_child_syncer(inst_idx).sync_type(typ);

        self.child.assign_type(child_reg, res_typ);
    }

    fn sync_from_child(&mut self, child: &TypeBag, dest: RegisterId, inst_idx: InstIdx) {
        let typ = child.get(dest);
        self.child_to_self_syncer(child, inst_idx).sync_type(typ);
    }

    /// Given a register type in a `type_bag` which is expected to be similar to
    /// this subset's `child`, this will perform all necessary work to bring
    /// that type into subset
    pub fn update_reg(&mut self, type_bag: &TypeBag, target_reg: RegisterId, inst_idx: InstIdx) {
        self.sync_from_child(type_bag, target_reg, inst_idx)
    }

    /// Given a single type in a, Deref `type_bag` which is expected to be similar to
    /// this subset's `child`, this will perform all necessary work to bring
    /// that type into the scope of this subset.
    pub fn update_typ(
        &mut self,
        type_bag: &TypeBag,
        typ: RegisterType,
        inst_idx: InstIdx,
    ) -> RegisterType {
        self.child_to_self_syncer(type_bag, inst_idx).sync_type(typ)
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

#[derive(Deref, DerefMut)]
struct CloneRodeo<T>(Rodeo<T>);

impl<T: Key> Default for CloneRodeo<T> {
    fn default() -> Self {
        CloneRodeo(Rodeo::new())
    }
}

impl<T: Key + Default> Clone for CloneRodeo<T> {
    fn clone(&self) -> Self {
        let mut new = Rodeo::new();

        for (k, v) in self.iter() {
            let k_new = new.get_or_intern(v);
            assert!(k == k_new, "need keys on rodeos to match");
        }

        CloneRodeo(new)
    }
}

#[derive(Clone)]
pub struct TypeBag {
    registers: FxHashMap<RegisterId, RegisterType>,
    records: RecordBag,
    unions: UnionInterner,
    constants: CloneRodeo<ConstantId>,
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
        inst_idx: InstIdx,
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
        self.constants.resolve(&id).as_bytes()
    }

    pub fn try_get(&self, register: RegisterId) -> Option<RegisterType> {
        self.registers.get(&register).cloned()
    }

    #[track_caller]
    pub fn get(&self, register: RegisterId) -> RegisterType {
        self.status.set(LookingUpStatus::Register(register));
        let typ = self.try_get(register).unwrap();
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
        inst_idx: InstIdx,
    ) -> Subset {
        debug_assert_eq!(src_args.len(), target_args.len());

        if src_args.is_empty() {
            return Subset {
                original: self,
                child: Default::default(),
                rec_map: Default::default(),
                src_fact_init: Default::default(),
                dest_fact_init: Default::default(),
            };
        }

        let mut subset = Subset {
            original: self,
            child: Default::default(),
            rec_map: Default::default(),
            src_fact_init: Default::default(),
            dest_fact_init: Default::default(),
        };

        for (src_reg, child_reg) in src_args.iter().zip(target_args.iter()) {
            subset.sync_to_child(*src_reg, *child_reg, InstIdx::Prologue);
        }

        subset
    }

    pub fn typ_eq(&self, a: RegisterType, b: RegisterType) -> bool {
        let mut record_constraints = vec![];
        let mut union_constraints = vec![];

        let is_maybe_equal =
            self.maybe_equal(&self, a, b, &mut record_constraints, &mut union_constraints);

        if !is_maybe_equal {
            return false;
        }

        self.solve_constraints(&self, &mut record_constraints, &mut union_constraints)
    }

    pub fn is_subtype(&self, subset: RegisterType, superset: RegisterType) -> bool {
        subset <= superset
    }

    /// true - maybe equal, false - definitely not equal
    fn maybe_equal(
        &self,
        other: &TypeBag,
        self_reg_type: RegisterType,
        other_reg_type: RegisterType,
        record_constraints: &mut Vec<(AllocationId, AllocationId)>,
        union_constraints: &mut Vec<(UnionId, UnionId)>,
    ) -> bool {
        use RegisterType::*;

        let maybe_equal = match (self_reg_type, other_reg_type) {
            // -> true  => maybe equal
            // -> false => definitely not equal
            (Record(a), Record(b)) => {
                record_constraints.push((a, b));
                true
            }
            (Union(a), Union(b)) => {
                union_constraints.push((a, b));
                true
            }
            (Byts(a), Byts(b)) => self.unintern_const(a) == other.unintern_const(b),
            (a, b) => a == b,
        };

        maybe_equal
    }

    /// true if all constraints can be resolve, false if not
    fn solve_constraints(
        &self,
        other: &TypeBag,
        record_constraints: &mut Vec<(AllocationId, AllocationId)>,
        union_constraints: &mut Vec<(UnionId, UnionId)>,
    ) -> bool {
        let mut solved_records = FxHashSet::default();
        let mut solved_unions = FxHashSet::default();

        while !record_constraints.is_empty() || !union_constraints.is_empty() {
            while let Some(constraint) = record_constraints.pop() {
                // because we don't use `solved_records`, it doesn't matter at which point within
                // this loop we add the records to `solved_records`
                //
                // thus, we do it at the beginning to prevent duplicate work in-case we are working
                // on an already solved constraint
                if !solved_records.insert(constraint) {
                    continue;
                }

                let (me, them) = constraint;

                if !self.rec_eq(other, me, them, record_constraints, union_constraints) {
                    return false;
                }
            }

            while let Some(constraint) = union_constraints.pop() {
                if !solved_unions.insert(constraint) {
                    continue;
                }

                let (me, them) = constraint;

                if !self.union_eq(other, me, them, record_constraints, union_constraints) {
                    return false;
                }
            }
        }

        // all constraints were solved
        true
    }

    fn rec_eq(
        &self,
        other: &TypeBag,
        self_rec: AllocationId,
        other_rec: AllocationId,
        record_constraints: &mut Vec<(AllocationId, AllocationId)>,
        union_constraints: &mut Vec<(UnionId, UnionId)>,
    ) -> bool {
        let self_keys = self.records.record_keys(self_rec);
        let other_keys = other.records.record_keys(other_rec);

        if self_keys.len() != other_keys.len() {
            return false;
        }

        for key in self_keys {
            if !other_keys.contains(&key) {
                return false;
            }

            let key = key.into_record_key();
            let my_value = self.value_type_at_record_key(self_rec, key);
            let other_value = other.value_type_at_record_key(other_rec, key);

            let maybe_equal = match (my_value, other_value) {
                (Some(a), Some(b)) => {
                    self.maybe_equal(other, a, b, record_constraints, union_constraints)
                }
                (None, None) => true,
                _ => false,
            };

            if !maybe_equal {
                return false;
            }
        }

        true
    }

    fn union_eq(
        &self,
        other: &TypeBag,
        self_union: UnionId,
        other_union: UnionId,
        record_constraints: &mut Vec<(AllocationId, AllocationId)>,
        union_constraints: &mut Vec<(UnionId, UnionId)>,
    ) -> bool {
        todo!()
    }

    fn value_type_at_record_key(&self, id: AllocationId, key: RecordKey) -> Option<RegisterType> {
        let is_subtype = |a, b| {
            // a : field
            // b : comparing against

            // if our key is "asdf"
            // we also want to find keys of `String`
            // thus, we want to find *supertypes*,
            // i.e. b :> a or a <: b

            use RecordKey::*;
            match (a, b) {
                (Slot(a), Slot(b)) => a == b,
                // a <: b
                (Key(a), Key(b)) => self.is_subtype(a, b),
                _ => false,
            }
        };

        let has_key = self
            .records
            .record_keys(id)
            .contains(&key.into_record_key_eq());
        debug_assert!(
            has_key,
            "the `record_keys` of `id` should have the key `key`"
        );

        let facts = self.records.record_facts_of(id, key, is_subtype);

        // facts : [most recent, ..., least recent]
        {
            // assertion for the above constraint
            let mut idx = InstIdx::Prologue;
            for fact in facts.iter() {
                debug_assert!(idx <= fact.inst_idx());
                idx = fact.inst_idx();
            }
        }

        debug_assert!(
            facts.len() > 0,
            "for the record to have a key there must be facts about it"
        );

        // we lose any relevent facts as soon as we delete something
        let facts = (facts.iter()).take_while(|fact| matches!(fact, Fact::Set { .. }));
        // facts : [set a => 1, remove a, set a => 2]
        // =>      [set a => 1]

        // the type of the value is the union of all of the facts
        let field_types = facts
            .map(|f| match f {
                Fact::Set {
                    key: fact_key,
                    value,
                    ..
                } => {
                    debug_assert!(
                        fact_key.into_record_key_eq() == key.into_record_key_eq(),
                        "should only be operating on facts of key type key"
                    );

                    *value
                }
                Fact::Remove { .. } => unreachable!(),
            })
            .collect::<Vec<_>>();

        match field_types.len() {
            // no need to set a field if it doesn't have any type
            // ^ in that case, facts : [remove a] => facts : []
            0 => None,
            // only a single type, no need to union anything
            1 => Some(field_types[0]),
            _ => {
                todo!("todo: support unions, but need to somehow intern unions while &self")
                // let union_id = self.unions.intern(todo!("union all types"));
                // Some(RegisterType::Union(union_id))
            }
        }
    }
}

impl Default for TypeBag {
    fn default() -> Self {
        TypeBag {
            registers: Default::default(),
            unions: Default::default(),
            records: Default::default(),
            constants: Default::default(),
            status: Default::default(),
        }
    }
}

impl PartialEq for TypeBag {
    /// Makes sure that every register pairing of two type bags are the same.
    fn eq(&self, other: &Self) -> bool {
        if self.registers.len() != other.registers.len() {
            return false;
        }

        let mut record_constraints = vec![];
        let mut union_constraints = vec![];

        for (key, value) in self.registers.iter() {
            let other_value = match other.registers.get(key) {
                Some(v) => v,
                None => return false,
            };

            let maybe_equal = self.maybe_equal(
                other,
                *value,
                *other_value,
                &mut record_constraints,
                &mut union_constraints,
            );

            if !maybe_equal {
                return false;
            }
        }

        self.solve_constraints(other, &mut record_constraints, &mut union_constraints)
    }
}

pub struct DisplayContext<'types> {
    types: &'types TypeBag,
    records_shown: Vec<AllocationId>,
}

impl DisplayContext<'_> {
    pub fn display(&mut self, register: RegisterId) -> String {
        let mut s = String::new();

        if let Some(typ) = self.types.try_get(register) {
            self.display_typ(&mut s, &typ).expect("to format");
        } else {
            s.push('?');
        }

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
            // TODO: display records and unions
            RegisterType::Record(r) => write!(w, "TODO: record {}", r)?,
            RegisterType::Union(u) => write!(w, "TODO: union {}", u)?,
        };

        Ok(())
    }

    fn display_cnst(&self, w: &mut String, cnst: ConstantId) -> std::fmt::Result {
        use std::fmt::Write;

        let payload = self.types.unintern_const(cnst);

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

unsafe impl Key for ConstantId {
    // fn to_usize(self) -> usize {
    fn into_usize(self) -> usize {
        self.raw_value() - 1
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        ConstantId::try_new_with_value_raw_const(int + 1)
    }
}
