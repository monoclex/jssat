//! just a hacky thing to bridge values from interpreter to domino

use rustc_hash::FxHashMap;

use crate::{
    isa::Atom,
    lifted::{FunctionId, RegisterId},
};

#[derive(Clone, Debug)]
pub enum SnapshotValue {
    Atom(Atom),
    // TODO: intern value of bytes
    Bytes(Vec<u8>),
    Number(i64),
    Boolean(bool),
    FnPtr(FunctionId),
    Record(usize),
    List(usize),
    Runtime,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SnapshotRecordKey {
    Atom(Atom),
    // TODO: intern
    Bytes(Vec<u8>),
    Number(i64),
    Boolean(bool),
    FnPtr(FunctionId),
}

#[derive(Clone)]
pub struct SnapshotRecord(pub FxHashMap<SnapshotRecordKey, SnapshotValue>);

#[derive(Clone)]
pub struct SnapshotList(pub Vec<SnapshotValue>);

#[derive(Clone, Default)]
pub struct ValueSnapshotArena {
    /// register id |-> index in value map
    pub registers: FxHashMap<RegisterId, SnapshotValue>,
    pub records: FxHashMap<usize, SnapshotRecord>,
    pub lists: FxHashMap<usize, SnapshotList>,
}
