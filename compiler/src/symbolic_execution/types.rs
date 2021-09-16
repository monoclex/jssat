//! [The corresonding blog post][blog post] is relevant to gain an understanding
//! of what the code istrying to model.
//!
//! [blog post]: https://sirjosh3917.com/posts/jssat-typing-objects-in-ssa-form/

use derive_more::{Deref, DerefMut, Display};
use lasso::{Key, Rodeo};
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::collections::FxBiHashMap;
use crate::id::{IdCompat, LiftedCtx, SymbolicCtx, UniqueRecordId};
use crate::isa::{InternalSlot, TrivialItem};
use crate::UnwrapNone;

type AllocationId = crate::id::AllocationId<LiftedCtx>;
type ConstantId = crate::id::ConstantId<SymbolicCtx>;
type RegisterId = crate::id::RegisterId<LiftedCtx>;
/// The ID of a function whose argument types are not yet known.
type DynFnId = crate::id::FunctionId<LiftedCtx>;
type UnionId = crate::id::UnionId<LiftedCtx>;
type WorkRecordKey = crate::isa::RecordKey<LiftedCtx>;

#[derive(Clone, Copy, Hash, Debug)]
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
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
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
    records: FxHashMap<AllocationId, Record>,
}

#[derive(Clone)]
struct Record {
    unique_allocation_id: UniqueRecordId<SymbolicCtx>,
    fact_paths: Union<Facts<Fact>>,
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

impl Record {
    fn new(unique_allocation_id: UniqueRecordId<SymbolicCtx>) -> Self {
        Self {
            unique_allocation_id,
            fact_paths: Union(vec![Facts(vec![])]),
        }
    }
}

impl RecordBag {
    pub fn new_record(
        &mut self,
        unique_allocation_id: UniqueRecordId<SymbolicCtx>,
    ) -> AllocationId {
        let id = self.counter.next_and_mut();

        let record = Record::new(unique_allocation_id);

        self.records.insert(id, record).expect_free();
        id
    }

    pub fn record_unique_id(&self, record: AllocationId) -> UniqueRecordId<SymbolicCtx> {
        self.records.get(&record).unwrap().unique_allocation_id
    }

    pub fn try_all_facts(&self, record: AllocationId) -> Option<&Union<Facts<Fact>>> {
        self.records.get(&record).map(|r| &r.fact_paths)
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
    ) -> Union<Vec<&Fact>>
    where
        F: Fn(RecordKey, RecordKey) -> bool,
    {
        let field_facts = self.try_record_facts_of(record, field, should_include_fact);
        debug_assert!(!field_facts.is_empty(), "should have field facts");
        field_facts
    }

    pub fn try_record_facts_of<F>(
        &self,
        record: AllocationId,
        field: RecordKey,
        should_include_fact: F,
    ) -> Union<Vec<&Fact>>
    where
        F: Fn(RecordKey, RecordKey) -> bool,
    {
        let record = self.records.get(&record).unwrap();

        let mut all_field_facts = vec![];
        for facts in record.fact_paths.iter() {
            let mut field_facts = vec![];
            for fact in facts.iter().rev() {
                if should_include_fact(field, fact.key()) {
                    field_facts.push(fact);
                }
            }

            if !field_facts.is_empty() {
                all_field_facts.push(field_facts);
            }
        }

        Union(all_field_facts)
    }

    pub fn record_fact_remove(
        &mut self,
        record: AllocationId,
        field: RecordKey,
        inst_idx: InstIdx,
    ) {
        let record = self.records.get_mut(&record).unwrap();

        for facts in record.fact_paths.iter_mut() {
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

        for facts in record.fact_paths.iter_mut() {
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
        debug_assert!(record.fact_paths.len() >= 1);

        // a record:
        // - definitively has a key `Some(true)`
        // - definitively does not have a key `Some(false)`
        // - may or may not have a key `None`

        let facts = record.fact_paths.iter();

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
            .any(|has| !has);

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

        let keys = (facts.fact_paths.iter())
            .flat_map(|facts| facts.iter())
            .map(|fact| fact.key().into_record_key_eq())
            .collect::<FxHashSet<_>>();

        keys
    }

    pub fn only_record_keys(&self, id: AllocationId) -> FxHashSet<RecordKeyEq> {
        self.record_keys(id)
    }

    pub fn record_keys_w_inst_idx(
        &self,
        id: AllocationId,
        up_until: InstIdx,
    ) -> FxHashSet<(RecordKeyEq, InstIdx)> {
        let facts = self.records.get(&id).unwrap();
        let mut new_facts = FxHashSet::default();

        for key in self.record_keys(id) {
            if let Some(inst_idx) = (facts.fact_paths.iter())
                .flat_map(|facts| facts.iter())
                .map(|fact| fact.inst_idx())
                .filter(|i| *i <= up_until)
                .max()
            {
                new_facts.insert((key, inst_idx));
            }
        }

        new_facts
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
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Display)]
pub enum InstIdx {
    Prologue,
    #[display(fmt = "{0}", _0)]
    Inst(usize),
    /// only needed as a hack really but oh well
    Epilogue,
}

impl InstIdx {
    pub fn from_inst_len(instructions: usize) -> Self {
        match instructions {
            0 => InstIdx::Prologue,
            n => InstIdx::Inst(n - 1),
        }
    }

    pub fn back(self) -> Option<Self> {
        match self {
            InstIdx::Prologue => None,
            InstIdx::Inst(0) => Some(InstIdx::Prologue),
            InstIdx::Inst(n) => Some(InstIdx::Inst(n - 1)),
            InstIdx::Epilogue => None,
        }
    }
}

struct Syncer<'a, R> {
    inst_idx: InstIdx,
    up_until: InstIdx,
    src: &'a TypeBag,
    dest: &'a mut TypeBag,
    resolve: R,
}

trait SyncResolver {
    /// record id in src -> record id in dest
    /// if `None`, that means generate a new record id
    fn map_record_id(&mut self, id: AllocationId) -> Option<AllocationId>;

    fn sync_stats(&self, id: AllocationId) -> Option<&SyncStats>;

    /// if a `None` was returned this will be called with `id` and the generated id
    fn save_record_id(&mut self, src_id: AllocationId, gen_id: AllocationId);

    fn save_sync_stats(&mut self, id: AllocationId, stats: SyncStats);
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
struct ResolveLeft<'a>(
    &'a mut FxBiHashMap<AllocationId, AllocationId>,
    &'a mut FxHashMap<AllocationId, SyncStats>,
);
struct ResolveRight<'a>(
    &'a mut FxBiHashMap<AllocationId, AllocationId>,
    &'a mut FxHashMap<AllocationId, SyncStats>,
);

impl<'a> SyncResolver for ResolveLeft<'a> {
    fn map_record_id(&mut self, id: AllocationId) -> Option<AllocationId> {
        self.0.get_by_left(&id).cloned()
    }
    fn sync_stats(&self, id: AllocationId) -> Option<&SyncStats> {
        self.1.get(&id)
    }
    fn save_record_id(&mut self, src_id: AllocationId, gen_id: AllocationId) {
        self.0.insert(src_id, gen_id).expect_free();
    }
    fn save_sync_stats(&mut self, id: AllocationId, stats: SyncStats) {
        self.1.insert(id, stats);
    }
}
impl<'a> SyncResolver for ResolveRight<'a> {
    fn map_record_id(&mut self, id: AllocationId) -> Option<AllocationId> {
        self.0.get_by_right(&id).cloned()
    }
    fn sync_stats(&self, id: AllocationId) -> Option<&SyncStats> {
        self.1.get(&id)
    }
    fn save_record_id(&mut self, src_id: AllocationId, gen_id: AllocationId) {
        self.0.insert(gen_id, src_id).expect_free();
    }
    fn save_sync_stats(&mut self, id: AllocationId, stats: SyncStats) {
        self.1.insert(id, stats);
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
                    Some(id) => id,
                    None => {
                        let unique_id = self.src.record_unique_id(id);

                        let gen_id = self.dest.records.new_record(unique_id);
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

                let keys = self.src.records.record_keys_w_inst_idx(id, self.up_until);

                for (key, inst_idx) in keys.into_iter().map(|(k, i)| (k.into_record_key(), i)) {
                    // TODO: should be more explicit about the `>` thing being done here
                    if inst_idx > self.up_until {
                        continue;
                    }

                    let src_value_type =
                        match self.src.value_type_at_record_key(id, key, self.up_until) {
                            None => continue,
                            Some(t) => t,
                        };

                    let mut update_record = true;

                    if let Some(Some(current_typ)) =
                        self.dest
                            .try_value_type_at_record_key(dest_id, key, self.up_until)
                    {
                        let are_typs_equal =
                            self.src.typ_eq_oth(self.dest, src_value_type, current_typ);

                        // no reason to update the record
                        if are_typs_equal {
                            update_record = false;
                        }
                    }

                    // because we are just checking for equality via typ_eq_other,
                    // we may not have the same ID for constants, even thoug they are equal to eachother in value
                    // so we will force the record to be updated even if they're equal if they're both bytes
                    if !update_record && matches!(src_value_type, RegisterType::Byts(_)) {
                        update_record = true;
                    }

                    if update_record {
                        let dest_value_type = self.sync_type(src_value_type);

                        self.dest.records.record_fact_set(
                            dest_id,
                            key,
                            dest_value_type,
                            self.inst_idx,
                        );
                    }
                }

                RegisterType::Record(dest_id)
            }
        }
    }
}

struct SyncStats {
    // num_facts_at_last_sync: usize,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum MaybeMut<'a, T> {
    Imm(&'a T),
    Mut(&'a mut T),
}

impl<'a, T> MaybeMut<'a, T> {
    pub fn try_into_mut<'b>(&'b mut self) -> Option<&'b mut &'a mut T> {
        match self {
            MaybeMut::Imm(_) => None,
            MaybeMut::Mut(r) => Some(r),
        }
    }
}

impl<'a, T> Deref for MaybeMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeMut::Imm(r) => *r,
            MaybeMut::Mut(r) => *r,
        }
    }
}

pub struct Subset<'a> {
    original: MaybeMut<'a, TypeBag>,
    child: TypeBag,
    // // { original register id |-> child register id }
    // reg_map: FxBiHashMap<RegisterId, RegisterId>,
    // { original record id |-> child record id }
    rec_map: FxBiHashMap<AllocationId, AllocationId>,

    // { record id in original |-> number of facts present at subset creation }
    src_fact_init: FxHashMap<AllocationId, SyncStats>,
    // { record id in dest |-> number of facts present at subset creation }
    dest_fact_init: FxHashMap<AllocationId, SyncStats>,
}

impl<'a> Subset<'a> {
    /// Produces the representation of the subset typebag
    pub fn child(&self) -> TypeBag {
        self.child.clone()
    }

    fn self_to_child_syncer<'b>(
        &'b mut self,
        inst_idx: InstIdx,
        up_until: InstIdx,
    ) -> Syncer<'b, ResolveLeft> {
        Syncer {
            inst_idx,
            up_until,
            src: &self.original,
            dest: &mut self.child,
            resolve: ResolveLeft(&mut self.rec_map, &mut self.src_fact_init),
        }
    }

    fn child_to_self_syncer<'b>(
        &'b mut self,
        child: &'b TypeBag,
        inst_idx: InstIdx,
        up_until: InstIdx,
    ) -> Syncer<'b, ResolveRight> {
        let dest = self.original.try_into_mut().unwrap();
        Syncer {
            inst_idx,
            up_until,
            src: child,
            dest,
            resolve: ResolveRight(&mut self.rec_map, &mut self.dest_fact_init),
        }
    }

    // this might just be update_reg
    fn sync_to_child(
        &mut self,
        src_reg: RegisterId,
        child_reg: RegisterId,
        inst_idx: InstIdx,
        up_until: InstIdx,
    ) {
        let typ = self.original.get(src_reg);

        let res_typ = self.self_to_child_syncer(inst_idx, up_until).sync_type(typ);

        self.child.assign_type(child_reg, res_typ);
    }

    fn sync_from_child(
        &mut self,
        child: &TypeBag,
        dest: RegisterId,
        inst_idx: InstIdx,
        up_until: InstIdx,
    ) {
        let typ = child.get(dest);
        self.child_to_self_syncer(child, inst_idx, up_until)
            .sync_type(typ);
    }

    /// Given a register type in a `type_bag` which is expected to be similar to
    /// this subset's `child`, this will perform all necessary work to bring
    /// that type into subset
    pub fn update_reg(
        &mut self,
        type_bag: &TypeBag,
        target_reg: RegisterId,
        inst_idx: InstIdx,
        up_until: InstIdx,
    ) {
        self.sync_from_child(type_bag, target_reg, inst_idx, up_until)
    }

    /// Given a single type in a, Deref `type_bag` which is expected to be similar to
    /// this subset's `child`, this will perform all necessary work to bring
    /// that type into the scope of this subset.
    pub fn update_typ(
        &mut self,
        type_bag: &TypeBag,
        typ: RegisterType,
        inst_idx: InstIdx,
        up_until: InstIdx,
    ) -> RegisterType {
        self.child_to_self_syncer(type_bag, inst_idx, up_until)
            .sync_type(typ)
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
    pub(crate) registers: FxHashMap<RegisterId, RegisterType>,
    records: RecordBag,
    unions: UnionInterner,
    constants: CloneRodeo<ConstantId>,
    status: LookingUp,
}

impl TypeBag {
    pub fn looking_up(&self) -> LookingUpStatus {
        self.status.get()
    }

    pub(crate) fn all_registers(&self) -> Vec<RegisterId> {
        self.registers.iter().map(|(r, _)| *r).collect()
    }

    pub fn new_record(
        &mut self,
        register: RegisterId,
        unique_record_id: UniqueRecordId<SymbolicCtx>,
    ) {
        let id = self.records.new_record(unique_record_id);
        self.registers
            .insert(register, RegisterType::Record(id))
            .expect_free();
    }

    pub fn record_unique_id(&self, record: AllocationId) -> UniqueRecordId<SymbolicCtx> {
        self.records.record_unique_id(record)
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

        // because we don't care about constraining to an `up_until` point,
        // we can just take the first fact
        let facts = Union(
            facts
                .iter()
                .map(|v| {
                    debug_assert!(!v.is_empty(), "should have fax");
                    v[0]
                })
                .collect(),
        );

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

    // TODO: this function shouldn't exist,
    // there is a bug somewhere that causes the assertion in `unintern_const` to trigger
    // but at the time of writing i'm not here to fix that
    pub fn mayb_unintern_const(&self, id: ConstantId) -> Option<&[u8]> {
        if id.into_usize() >= self.constants.len() {
            return None;
        }

        Some(self.constants.resolve(&id).as_bytes())
    }

    pub fn unintern_const(&self, id: ConstantId) -> &[u8] {
        if id.into_usize() >= self.constants.len() {
            panic!("this will be terrible for the economy");
        }

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

    pub fn display(&self, register: RegisterId, inst_idx: InstIdx) -> String {
        DisplayContext {
            types: self,
            records_shown: Default::default(),
            inst_idx,
        }
        .display(register)
    }

    pub fn display_type(&self, register_type: RegisterType, inst_idx: InstIdx) -> String {
        DisplayContext {
            types: self,
            records_shown: Default::default(),
            inst_idx,
        }
        .display_type(&register_type)
    }

    pub fn subset(
        &mut self,
        src_args: &[RegisterId],
        target_args: &[RegisterId],
        up_until: InstIdx,
    ) -> Subset {
        TypeBag::subset_impl(MaybeMut::Mut(self), src_args, target_args, up_until)
    }

    pub fn subset_immut(
        &self,
        src_args: &[RegisterId],
        target_args: &[RegisterId],
        up_until: InstIdx,
    ) -> Subset {
        TypeBag::subset_impl(MaybeMut::Imm(self), src_args, target_args, up_until)
    }

    fn subset_impl<'a>(
        original: MaybeMut<'a, TypeBag>,
        src_args: &[RegisterId],
        target_args: &[RegisterId],
        up_until: InstIdx,
    ) -> Subset<'a> {
        debug_assert_eq!(src_args.len(), target_args.len());

        let mut subset = Subset {
            original,
            child: Default::default(),
            rec_map: Default::default(),
            src_fact_init: Default::default(),
            dest_fact_init: Default::default(),
        };

        if src_args.is_empty() {
            return subset;
        }

        for (src_reg, child_reg) in src_args.iter().zip(target_args.iter()) {
            subset.sync_to_child(*src_reg, *child_reg, InstIdx::Prologue, up_until);
        }

        subset
    }

    pub fn typ_eq(&self, a: RegisterType, b: RegisterType) -> bool {
        self.typ_eq_oth(self, a, b)
    }

    pub fn typ_eq_oth(
        &self,
        other: &TypeBag,
        typ_self: RegisterType,
        typ_other: RegisterType,
    ) -> bool {
        let mut record_constraints = vec![];
        let mut union_constraints = vec![];

        let is_maybe_equal = self.maybe_equal(
            other,
            typ_self,
            typ_other,
            &mut record_constraints,
            &mut union_constraints,
        );

        if !is_maybe_equal {
            return false;
        }

        self.solve_constraints(other, &mut record_constraints, &mut union_constraints)
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
                // only allow records that come from the same allocation to be considered equal
                // TODO: state the name of the phenomenom this comes from, "shape dilemma" or something
                // TODO: add unit test covering the necessity of this test
                if self.record_unique_id(a) != other.record_unique_id(b) {
                    false
                } else {
                    record_constraints.push((a, b));
                    true
                }
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
        // TODO: add unit test covering the necessity of this test
        if self.record_unique_id(self_rec) != other.record_unique_id(other_rec) {
            return false;
        }

        let self_keys = self.records.only_record_keys(self_rec);
        let other_keys = other.records.only_record_keys(other_rec);

        if self_keys.len() != other_keys.len() {
            return false;
        }

        for key in self_keys {
            if !other_keys.contains(&key) {
                return false;
            }

            let key = key.into_record_key();
            let my_value = self.value_type_at_record_key(self_rec, key, InstIdx::Epilogue);
            let other_value = other.value_type_at_record_key(other_rec, key, InstIdx::Epilogue);

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
        _other: &TypeBag,
        _self_union: UnionId,
        _other_union: UnionId,
        _record_constraints: &mut Vec<(AllocationId, AllocationId)>,
        _union_constraints: &mut Vec<(UnionId, UnionId)>,
    ) -> bool {
        todo!()
    }

    fn try_value_type_at_record_key(
        &self,
        id: AllocationId,
        key: RecordKey,
        up_until: InstIdx,
    ) -> Option<Option<RegisterType>> {
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

        let facts = self.records.try_record_facts_of(id, key, is_subtype);

        // facts : [most recent, ..., least recent]
        // assertion for the above:
        if cfg!(debug_assertions) {
            let mut idx = InstIdx::Prologue;
            for fact in facts.iter() {
                assert!(!fact.is_empty());
                let fact = fact[0];
                assert!(idx <= fact.inst_idx());
                idx = fact.inst_idx();
            }
        }

        if facts.len() == 0 {
            return None;
        }

        // constrain the facts we take to `up_until`
        let facts = facts
            .iter()
            .map(|f| {
                // filter out the facts too far in the future
                let mut f = f.iter().filter(|f| {
                    // // TODO: do we want `>=` or `>`?
                    // if inst_idx > self.up_until {
                    //     continue;
                    // }
                    let skip = f.inst_idx() > up_until;

                    // we only keep elements that return true
                    !skip
                });

                // the first fact will be the most recent one
                // and if there is no fact, then we shouldn't've gotten that chain of facts
                // thus ignore it
                f.next()
            })
            .flatten();

        // we lose any relevent facts as soon as we delete something
        let facts = facts.take_while(|fact| {
            let doesMatch = matches!(fact, Fact::Set { .. });
            doesMatch
        });
        // facts : [set a => 1, remove a, set a => 2]
        //       = [set a => 1]
        // facts : [set a => 1, set a => 2]
        //       = [set a => 1, set a => 2]

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
            0 => Some(None),
            // only a single type, no need to union anything
            1 => Some(Some(field_types[0])),
            _ => {
                todo!("todo: support unions, but need to somehow intern unions while &self")
                // let union_id = self.unions.intern(todo!("union all types"));
                // Some(RegisterType::Union(union_id))
            }
        }
    }

    fn value_type_at_record_key(
        &self,
        id: AllocationId,
        key: RecordKey,
        up_until: InstIdx,
    ) -> Option<RegisterType> {
        let has_key = self
            .records
            .only_record_keys(id)
            .contains(&key.into_record_key_eq());
        debug_assert!(
            has_key,
            "the `record_keys` of `id` should have the key `key`"
        );

        let result = self.try_value_type_at_record_key(id, key, up_until);

        result.expect("for the record to have a key there must be facts about it")
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
    records_shown: FxHashSet<AllocationId>,
    inst_idx: InstIdx,
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

    pub fn display_type(&mut self, reg_typ: &RegisterType) -> String {
        let mut s = String::new();
        self.display_typ(&mut s, &reg_typ).expect("to format");
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
            RegisterType::Union(u) => write!(w, "TODO: union {}", u)?,
            RegisterType::Record(r) => {
                let unique_id = self.types.record_unique_id(r);

                if !self.records_shown.insert(r) {
                    write!(w, "{}, #{} @ ...", r.raw_value(), unique_id)?;
                } else {
                    write!(w, "{}, #{} @ ", r.raw_value(), unique_id)?;

                    if let Some(fact_list) = self.types.records.try_all_facts(r) {
                        write!(w, "{{ ")?;

                        debug_assert!(!fact_list.is_empty());

                        let mut facts = fact_list.iter();

                        let first_fact_list = facts.next().unwrap();
                        self.display_facts(w, first_fact_list)?;

                        for more_facts in facts {
                            write!(w, " | ")?;
                            self.display_facts(w, more_facts)?;
                        }

                        write!(w, " }}")?;
                    } else {
                        write!(w, "{{ ? }}")?;
                    }
                }
            }
        };

        Ok(())
    }

    fn display_facts(&mut self, w: &mut String, facts: &Facts<Fact>) -> std::fmt::Result {
        if facts.len() == 0 {
            w.push_str("[]");
            return Ok(());
        }

        w.push('[');

        let inst_idx = self.inst_idx;
        let mut facts = facts.iter().filter(|f| f.inst_idx() <= inst_idx);

        if let Some(fact) = facts.next() {
            self.display_fact(w, fact)?;

            for fact in facts {
                w.push_str(", ");
                self.display_fact(w, fact)?;
            }
        }

        w.push(']');
        Ok(())
    }

    fn display_fact(&mut self, w: &mut String, fact: &Fact) -> std::fmt::Result {
        use std::fmt::Write;

        match fact {
            Fact::Set {
                key,
                value,
                inst_idx,
            } => {
                write!(w, "{}: set ", inst_idx)?;
                self.display_rec_key(w, key)?;
                w.push_str(" => ");
                self.display_typ(w, value)
            }
            Fact::Remove { key, inst_idx } => {
                write!(w, "{}: remove ", inst_idx)?;
                self.display_rec_key(w, key)
            }
        }
    }

    fn display_rec_key(&mut self, w: &mut String, key: &RecordKey) -> std::fmt::Result {
        use std::fmt::Write;

        match key {
            RecordKey::Key(k) => self.display_typ(w, k),
            RecordKey::Slot(s) => write!(w, "[[{}]]", s),
        }
    }

    fn display_cnst(&self, w: &mut String, cnst: ConstantId) -> std::fmt::Result {
        use std::fmt::Write;

        let payload = match self.types.mayb_unintern_const(cnst) {
            Some(c) => c,
            None => {
                write!(w, "<bugged>")?;
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
