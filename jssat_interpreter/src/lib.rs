//! A JSSAT IR interpreter. This is primarily used for unit testing.
//!
//! In order to ensure that the JSSAT IR builder APIs produce the correct code,
//! and that the interpreter correctly understands the produced code, a JSSAT
//! interpreter exists to run some JSSAT IR and then produce results about it.
//!
//! Performance is not necessarily a goal, although in the future that may be
//! reconsidered.

#![feature(lint_reasons)]

mod build;
pub use build::*;
use jssat_ir::UnwrapNone;

#[cfg(test)]
mod tests;

use domino::moment::MomentApi;

use std::convert::TryInto;
use std::panic::Location;

use derive_more::{Deref, DerefMut};
use gc::{custom_trace, BorrowError, BorrowMutError, Finalize, Gc, GcCell, Trace};
use rustc_hash::FxHashMap;
use thiserror::Error;

use jssat_ir::isa::{Atom, CompareType, ValueType};
use jssat_ir::lifted::Function;
use jssat_ir::{collections::StrictZip, isa::BinaryOperator};

use jssat_ir::{
    frontend::ir::Instruction,
    id::LiftedCtx,
    lifted::{
        ConstantId, EndInstruction, ExternalFunctionId, FunctionId, LiftedProgram, RegisterId,
    },
};

pub struct Interpreter<'parent> {
    alloc_id: usize,
    code: &'parent LiftedProgram,
    external_fns: &'parent FxHashMap<ExternalFunctionId, ExtFnImpl>,
    pub moment: MomentApi,
}

pub struct Edge {
    pub goes_in: bool,
    pub from: Option<FunctionId>,
    pub to: Option<FunctionId>,
}

/// Implement an external function call from within pure rust code for JSSAT
pub struct ExtFnImpl {
    function: fn(Vec<Value>) -> InstResult<Option<Value>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum RecordKey {
    Atom(Atom),
    // TODO: use GC'd Vec<u8>s to prevent excessive cloning
    Bytes(Vec<u8>),
    Number(i64),
    Boolean(bool),
    FnPtr(FunctionId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListKey {
    Index(usize),
}

#[derive(Clone, Debug, Trace, Finalize)]
pub enum Value {
    Atom(#[unsafe_ignore_trace] Atom),
    // TODO: use GC'd Vec<u8>s to prevent excessive cloning
    Bytes(Vec<u8>),
    Number(i64),
    Boolean(bool),
    FnPtr(#[unsafe_ignore_trace] FunctionId),
    Record(Gc<GcCell<Record>>),
    List(Gc<GcCell<List>>),
    Runtime,
}

impl Value {
    pub fn kind(&self) -> ValueType {
        match self {
            Value::Atom(_) => ValueType::Atom,
            Value::Bytes(_) => ValueType::Bytes,
            Value::Number(_) => ValueType::Number,
            Value::Boolean(_) => ValueType::Boolean,
            Value::FnPtr(_) => ValueType::FnPtr,
            Value::Record(_) => ValueType::Record,
            Value::List(_) => ValueType::List,
            Value::Runtime => ValueType::Runtime,
        }
    }
}

macro_rules! value_unwrap {
    ($name: ident, $kind: ident, $type: ty) => {
        #[track_caller]
        pub fn $name(&self) -> InstResult<$type> {
            match self {
                Self::$kind(value) => Ok(*value),
                _ => Err(InvalidType(Location::caller())),
            }
        }
    };
}

impl Value {
    value_unwrap!(try_into_atom, Atom, Atom);
    // TODO(refactor): would it be possible to make `value_unwrap!` macro detect
    //     if a type is trivially copyable, and if so, copy the value for us
    //     within the macro? that would allow us to re-use the macro
    // value_unwrap!(try_into_bytes, Bytes, Vec<u8>);
    value_unwrap!(try_into_number, Number, i64);
    value_unwrap!(try_into_boolean, Boolean, bool);
    value_unwrap!(try_into_fnptr, FnPtr, FunctionId);

    #[track_caller]
    pub fn try_into_bytes(&self) -> InstResult<&Vec<u8>> {
        match self {
            Self::Bytes(value) => Ok(value),
            _ => Err(InvalidType(Location::caller())),
        }
    }

    #[track_caller]
    pub fn try_into_record(&self) -> InstResult<gc::GcCellRef<Record>> {
        match self {
            // writing `Ok(record.borrow())` causes rustc to overflow
            // while trying to evaluate Add for OrderedFloat (...??? wtf????)
            //
            // implicitly solved by wrapping for error (we should anyways)
            Self::Record(record) => {
                let borrow = record.try_borrow().map_err(BorrowErrorWrapper::Immut)?;
                Ok(borrow)
            }
            _ => Err(InvalidType(Location::caller())),
        }
    }

    #[track_caller]
    pub fn try_into_record_mut(&self) -> InstResult<gc::GcCellRefMut<Record>> {
        match self {
            Self::Record(record) => {
                let borrow = record.try_borrow_mut().map_err(BorrowErrorWrapper::Mut)?;
                Ok(borrow)
            }
            _ => Err(InvalidType(Location::caller())),
        }
    }

    #[track_caller]
    pub fn try_into_list(&self) -> InstResult<gc::GcCellRef<List>> {
        match self {
            // writing `Ok(record.borrow())` causes rustc to overflow
            // while trying to evaluate Add for OrderedFloat (...??? wtf????)
            //
            // implicitly solved by wrapping for error (we should anyways)
            Self::List(list) => {
                let borrow = list.try_borrow().map_err(BorrowErrorWrapper::Immut)?;
                Ok(borrow)
            }
            _ => Err(InvalidType(Location::caller())),
        }
    }

    #[track_caller]
    pub fn try_into_list_mut(&self) -> InstResult<gc::GcCellRefMut<List>> {
        match self {
            Self::List(list) => {
                let borrow = list.try_borrow_mut().map_err(BorrowErrorWrapper::Mut)?;
                Ok(borrow)
            }
            _ => Err(InvalidType(Location::caller())),
        }
    }
}

#[derive(Deref, DerefMut, Debug, Finalize)]
pub struct Record {
    alloc_id: usize,
    #[deref]
    #[deref_mut]
    values: FxHashMap<RecordKey, Value>,
}

impl Record {
    pub fn new(alloc_id: usize) -> Self {
        Self {
            alloc_id,
            values: Default::default(),
        }
    }

    pub fn new_gc(alloc_id: usize) -> Gc<GcCell<Self>> {
        let me = Self::new(alloc_id);
        Gc::new(GcCell::new(me))
    }

    #[track_caller]
    pub fn try_get(&self, key: &RecordKey) -> InstResult<&Value> {
        self.get(key)
            .ok_or_else(|| InstErr::RecordDNCKey(key.clone(), Location::caller()))
    }

    #[track_caller]
    pub fn try_get_mut(&mut self, key: &RecordKey) -> InstResult<&mut Value> {
        self.get_mut(key)
            .ok_or_else(|| InstErr::RecordDNCKey(key.clone(), Location::caller()))
    }
}

unsafe impl Trace for Record {
    custom_trace! {
        this,
        {
            for (_, v) in this.values.iter() {
                mark(v);
            }
        }
    }
}

#[derive(Deref, DerefMut, Debug, Finalize, Trace)]
pub struct List {
    alloc_id: usize,
    #[deref]
    #[deref_mut]
    values: Vec<Value>,
}

impl List {
    pub fn new(alloc_id: usize) -> Self {
        Self {
            alloc_id,
            values: Default::default(),
        }
    }

    pub fn new_gc(alloc_id: usize) -> Gc<GcCell<Self>> {
        let me = Self::new(alloc_id);
        Gc::new(GcCell::new(me))
    }

    #[track_caller]
    pub fn try_get(&self, key: &ListKey) -> InstResult<&Value> {
        let ListKey::Index(idx) = key;

        self.get(*idx)
            .ok_or_else(|| InstErr::ListDNCKey(*key, Location::caller()))
    }

    #[track_caller]
    pub fn try_get_mut(&mut self, key: &ListKey) -> InstResult<&mut Value> {
        let ListKey::Index(idx) = key;

        self.get_mut(*idx)
            .ok_or_else(|| InstErr::ListDNCKey(*key, Location::caller()))
    }
}

pub fn ensure_arg_count(expected: usize, got: usize) -> InstResult<()> {
    if expected != got {
        return Err(NotEnoughArgs(expected, got, Location::caller()));
    }

    Ok(())
}

use jssat_ir::value_snapshot::*;

trait ValueSnapshotArenaExt {
    fn snapshot(&mut self, register: RegisterId, value: &Value);
    fn map_value(&mut self, value: &Value) -> SnapshotValue;
    fn map_key(&mut self, key: &RecordKey) -> SnapshotValue;
}

impl ValueSnapshotArenaExt for ValueSnapshotArena {
    fn snapshot(&mut self, register: RegisterId, value: &Value) {
        let value = self.map_value(value);
        self.registers.insert(register, value);
    }

    fn map_value(&mut self, value: &Value) -> SnapshotValue {
        match value {
            Value::Atom(x) => SnapshotValue::Atom(*x),
            Value::Bytes(x) => SnapshotValue::Bytes(x.clone()),
            Value::Number(x) => SnapshotValue::Number(*x),
            Value::Boolean(x) => SnapshotValue::Boolean(*x),
            Value::FnPtr(x) => SnapshotValue::FnPtr(*x),
            Value::Record(x) => {
                let record = x.borrow();

                if !self.records.contains_key(&record.alloc_id) {
                    // put in a temp nonsense value to prevent recursion
                    self.records
                        .insert(record.alloc_id, SnapshotRecord(Default::default()))
                        .expect_none("should not be reinserting");

                    let values = (record.values.iter())
                        .map(|(k, v)| (self.map_key(k), self.map_value(v)))
                        .collect::<FxHashMap<_, _>>();

                    self.records.insert(record.alloc_id, SnapshotRecord(values));
                }

                SnapshotValue::Record(record.alloc_id)
            }
            Value::List(x) => {
                let list = x.borrow();

                if !self.lists.contains_key(&list.alloc_id) {
                    // put in a temp nonsense value to prevent recursion
                    self.lists
                        .insert(list.alloc_id, SnapshotList(Default::default()))
                        .expect_none("should not be reinserting");

                    let values = (list.values.iter())
                        .map(|v| self.map_value(v))
                        .collect::<Vec<_>>();
                    self.lists.insert(list.alloc_id, SnapshotList(values));
                }

                SnapshotValue::List(list.alloc_id)
            }
            Value::Runtime => SnapshotValue::Runtime,
        }
    }

    // keep this `mut self` as we may want to intern bytes in the future
    fn map_key(&mut self, key: &RecordKey) -> SnapshotValue {
        match key {
            RecordKey::Atom(x) => SnapshotValue::Atom(*x),
            RecordKey::Bytes(x) => SnapshotValue::Bytes(x.clone()),
            RecordKey::Number(x) => SnapshotValue::Number(*x),
            RecordKey::Boolean(x) => SnapshotValue::Boolean(*x),
            RecordKey::FnPtr(x) => SnapshotValue::FnPtr(*x),
        }
    }
}

impl<'p> Interpreter<'p> {
    pub fn new(
        code: &'p LiftedProgram,
        ext_fns: &'p FxHashMap<ExternalFunctionId, ExtFnImpl>,
    ) -> Self {
        Interpreter {
            alloc_id: 0,
            code,
            external_fns: ext_fns,
            moment: MomentApi::new(code),
        }
    }

    fn next_alloc_id(&mut self) -> usize {
        let id = self.alloc_id;
        self.alloc_id += 1;
        id
    }

    #[allow(non_upper_case_globals)]
    pub fn execute_fn_id(&mut self, id: FunctionId, args: Vec<Value>) -> InstResult<Option<Value>> {
        const KiB: usize = 1024;
        const MiB: usize = 1024 * KiB;
        self.moment.enter(id);
        let results = stacker::maybe_grow(32 * KiB, 4 * MiB, || self.execute_fn_id_impl(id, args));

        if results.is_ok() {
            self.moment.exit();
        }

        results
    }

    fn execute_fn_id_impl(
        &mut self,
        id: FunctionId,
        args: Vec<Value>,
    ) -> InstResult<Option<Value>> {
        let function = (self.code.functions)
            .get(&id)
            .expect("expected valid function id");

        ensure_arg_count(function.parameters.len(), args.len())?;

        let mut registers = FxHashMap::default();
        for (register, value) in function.parameters.iter().strict_zip(args) {
            registers.insert(*register, value);
        }

        let mut inst_exec = InstExec {
            registers,
            interpreter: self,
            function,
        };

        for (idx, inst) in function.instructions.iter().enumerate() {
            // only take a pre snapshot if it's a non-trivial instruction that
            // could mutate args, like call
            use jssat_ir::frontend::ir::InstructionData;
            if matches!(
                inst.data,
                InstructionData::CallVirt(_) | InstructionData::CallStatic(_)
            ) {
                let mut pre = ValueSnapshotArena::new();

                for r in inst.used_registers() {
                    pre.snapshot(r, inst_exec.registers.get(&r).unwrap());
                }

                inst_exec
                    .interpreter
                    .moment
                    .snapshot(idx, inst.source_map_idx, pre);
            }

            let result = inst_exec.exec(inst);

            let mut post = ValueSnapshotArena::new();

            if let Some(r) = inst.assigned_to() {
                // may not be assigned if instruction fails
                if let Some(v) = inst_exec.registers.get(&r) {
                    post.snapshot(r, v);
                }
            }

            for r in inst.used_registers() {
                post.snapshot(r, inst_exec.registers.get(&r).unwrap());
            }

            inst_exec
                .interpreter
                .moment
                .snapshot(idx, inst.source_map_idx, post);

            result?;
        }

        let inst_idx = function.instructions.len();
        let mut pre = ValueSnapshotArena::new();

        for r in function.end.used_registers() {
            pre.snapshot(r, inst_exec.registers.get(&r).unwrap());
        }

        (inst_exec.interpreter.moment).snapshot(inst_idx, None, pre);

        let (registers, result) = match &function.end {
            EndInstruction::Jump(i) => {
                let args = inst_exec.load_args(&i.0 .1)?;
                (inst_exec.registers, self.execute_fn_id(i.0 .0, args))
            }
            EndInstruction::JumpIf(i) => {
                let condition = inst_exec.get(i.condition)?;

                let condition = match condition {
                    Value::Boolean(cond) => *cond,
                    _ => {
                        return Err(InstErr::ConditionalNotBoolean(
                            condition.clone(),
                            Location::caller(),
                        ))
                    }
                };

                let jump = if condition { &i.if_so } else { &i.other };

                let args = inst_exec.load_args(&jump.1)?;
                (inst_exec.registers, self.execute_fn_id(jump.0, args))
            }
            EndInstruction::Return(i) => match i.0 {
                Some(register) => {
                    let value = inst_exec.get(register)?.clone();
                    (inst_exec.registers, Ok(Some(value)))
                }
                None => (inst_exec.registers, Ok(None)),
            },
        };

        let mut post = ValueSnapshotArena::new();

        for r in function.end.used_registers() {
            post.snapshot(r, registers.get(&r).unwrap());
        }

        self.moment.snapshot(inst_idx, None, post);

        result
    }
}

struct InstExec<'interpreter, 'code> {
    registers: FxHashMap<RegisterId, Value>,
    interpreter: &'interpreter mut Interpreter<'code>,
    function: &'code Function,
}

pub type InstResult<T> = Result<T, InstErr>;
type PanicLocation = &'static Location<'static>;

#[derive(Error, Debug)]
pub enum InstErr {
    #[error("Register does not exist: {}", .0)]
    RegisterDNE(RegisterId, PanicLocation),
    // TODO: wrap `Value` in something that formats better
    #[error("Invalid key used as record key: {:?}", .0)]
    InvalidRecKey(Value, PanicLocation),
    #[error("Invalid key used as list key: {:?}", .0)]
    InvalidListKey(Value, PanicLocation),
    // TODO: wrap `RecordKey` in  something that formats better
    #[error("Record does not contain key: {:?}", .0)]
    RecordDNCKey(RecordKey, PanicLocation),
    #[error("List does not contain key: {:?}", .0)]
    ListDNCKey(ListKey, PanicLocation),
    #[error("Invalid index for list: {:?}", .0)]
    ListInvalidIndex(i64, PanicLocation),
    #[error("Invalid index value for list: {:?}", .0)]
    ListInvalidIndexRegister(Value, PanicLocation),
    #[error("Constant does not exist: {}", .0)]
    ConstantDNE(ConstantId, PanicLocation),
    #[error("Unable to perform binary operation: {:?} `{}` {:?}", .0, .2, .1)]
    BinOpFail(Value, Value, BinaryOperator, PanicLocation),
    #[error("Unable to perform unary operation: `{}` {:?}", .1, .0)]
    UnaryOpFail(Value, &'static str, PanicLocation),
    #[error("Unable to call virtual function, as register is not a fnptr: {:?}", .0)]
    NonVirt(Value, PanicLocation),
    #[error("Expected function to return a value, but function returned void.")]
    ExpectedNonVoid(PanicLocation),
    #[error("Expected conditional value to be a boolean, but got: {:?}", .0)]
    ConditionalNotBoolean(Value, PanicLocation),
    #[error("An invalid amount of arguments were supplied: expected {}, got {}", .0, .1)]
    NotEnoughArgs(usize, usize, PanicLocation),
    #[error("External function does not exist: {}", .0)]
    ExtFnDNE(ExternalFunctionId, PanicLocation),
    // TODO: supply more information here?
    #[error("Invalid type of argument")]
    InvalidType(PanicLocation),
    #[error("An error occurred borrowing the record")]
    BorrowError(#[from] BorrowErrorWrapper),
    #[error("An assertion failed: {}", .0)]
    AssertionFailed(&'static str, PanicLocation),

    // TODO: support external function calls
    // they can be implemented by having some kind of rust function be paired
    // with an external function id, and then running that function when
    // the interpreter encounters it. the external function has to be given the
    // parameters it wants and then can do something with interpreter state idk
    #[error("Unable to perform external function calls at this time")]
    ExternalFnCall,

    #[error("An `unreachable` instruction, which is suppose to be unreachable, was reached.")]
    Unreachable(PanicLocation),
}

#[derive(Error, Debug)]
pub enum BorrowErrorWrapper {
    #[error("Immutable borrow error")]
    Immut(BorrowError),
    #[error("Mutable borrow error")]
    Mut(BorrowMutError),
}

use InstErr::*;

impl<'i, 'c> InstExec<'i, 'c> {
    pub fn exec(&mut self, inst: &Instruction<LiftedCtx, LiftedCtx>) -> InstResult<()> {
        if let Some(result) = inst.assigned_to() {
            assert!(
                !self.registers.contains_key(&result),
                "SSA form must be maintained"
            );
        }

        use jssat_ir::frontend::ir::InstructionData::*;
        match &inst.data {
            Comment(_) => {}
            NewRecord(i) => {
                self.registers.insert(
                    i.result,
                    Value::Record(Record::new_gc(self.interpreter.next_alloc_id())),
                );
            }
            RecordGet(i) => {
                let key = self.to_key(i.key)?;
                let record = self.get_record(i.record)?;
                let value = record.try_get(&key)?.clone();
                drop(record);
                self.registers.insert(i.result, value);
            }
            RecordSet(i) => {
                let key = self.to_key(i.key)?;

                match i.value {
                    Some(register) => {
                        let value = self.get(register)?.clone();
                        let mut record = self.get_record_mut(i.record)?;
                        record.insert(key, value);
                    }
                    None => {
                        let mut record = self.get_record_mut(i.record)?;
                        record.remove(&key);
                    }
                };
            }
            RecordHasKey(i) => {
                let key = self.to_key(i.key)?;
                let record = self.get_record(i.record)?;
                let has_key = record.contains_key(&key);
                drop(record);
                self.registers.insert(i.result, Value::Boolean(has_key));
            }
            GetFnPtr(i) => {
                self.registers.insert(i.result, Value::FnPtr(i.item));
            }
            CallStatic(i) => {
                self.call_fn(&i.args, i.calling, i.result)?;
            }
            CallExtern(i) => {
                let ext_fn = self.get_ext_fn(i.calling)?;
                let args = self.load_args(&i.args)?;
                let result = (ext_fn.function)(args)?;
                self.store_fn_result(result, i.result)?;
            }
            CallVirt(i) => {
                let value = self.get(i.calling)?;

                let fn_id = match value {
                    Value::FnPtr(fn_id) => *fn_id,
                    _ => return Err(NonVirt(value.clone(), Location::caller())),
                };

                self.call_fn(&i.args, fn_id, i.result)?;
            }
            MakeAtom(i) => {
                self.registers.insert(i.result, Value::Atom(i.item));
            }
            MakeBytes(i) => {
                let bytes = self.lookup_constant(i.item)?;
                self.registers.insert(i.result, Value::Bytes(bytes));
            }
            MakeInteger(i) => {
                self.registers.insert(i.result, Value::Number(i.item));
            }
            MakeBoolean(i) => {
                self.registers.insert(i.result, Value::Boolean(i.item));
            }
            BinOp(i) => {
                // TODO: for the and operator, usually that short circuits.
                // hopefully, frontends are extremely agnostic to this fact
                let lhs = self.get(i.lhs)?;
                let rhs = self.get(i.rhs)?;

                let fail = || {
                    Err(BinOpFail(
                        lhs.clone(),
                        rhs.clone(),
                        i.op,
                        Location::caller(),
                    ))
                };

                use jssat_ir::isa::BinaryOperator::*;
                use Value::*;
                let value = match i.op {
                    Add => match (lhs, rhs) {
                        (Bytes(lhs), Bytes(rhs)) => {
                            let mut combined = lhs.clone();
                            combined.extend(rhs);
                            Bytes(combined)
                        }
                        (Number(lhs), Number(rhs)) => Number(*lhs + *rhs),
                        _ => return fail(),
                    },
                    And => match (lhs, rhs) {
                        (Boolean(lhs), Boolean(rhs)) => Boolean(*lhs && *rhs),
                        _ => return fail(),
                    },
                    Or => match (lhs, rhs) {
                        (Boolean(lhs), Boolean(rhs)) => Boolean(*lhs || *rhs),
                        _ => return fail(),
                    },
                    Equals => Boolean(match (lhs, rhs) {
                        (Number(lhs), Number(rhs)) => lhs == rhs,
                        (Bytes(lhs), Bytes(rhs)) => lhs == rhs,
                        (Boolean(lhs), Boolean(rhs)) => lhs == rhs,
                        (Atom(lhs), Atom(rhs)) => lhs == rhs,
                        (Record(lhs), Record(rhs)) => Gc::ptr_eq(lhs, rhs),
                        (Record(_), Atom(_)) | (Atom(_), Record(_)) => false,
                        _ => return fail(),
                    }),
                    LessThan => Boolean(match (lhs, rhs) {
                        (Number(lhs), Number(rhs)) => lhs < rhs,
                        _ => return fail(),
                    }),
                };

                self.registers.insert(i.result, value);
            }
            Negate(i) => {
                let value = self.get(i.operand)?;

                let fail = || Err(UnaryOpFail(value.clone(), "!", Location::caller()));

                use Value::*;
                let value = match value {
                    Boolean(b) => Boolean(!*b),
                    Number(n) => Number(-*n),
                    _ => return fail(),
                };

                self.registers.insert(i.result, value);
            }
            Generalize(_) => {
                todo!("generalize not used yet")
            }
            Assert(i) => {
                let value = self.get(i.condition)?;
                let assertion = value.try_into_boolean()?;

                if !assertion {
                    return Err(AssertionFailed(i.message, Location::caller()));
                }
            }
            IsType(i) => {
                let value = self.get(i.value)?;
                let target_kind = match i.kind {
                    CompareType::Kind(kind) => kind,
                    CompareType::Register(register) => {
                        let value = self.get(register)?;
                        value.kind()
                    }
                };

                let is_type = match (value, target_kind) {
                    (Value::Atom(_), ValueType::Atom)
                    | (Value::Bytes(_), ValueType::Bytes)
                    | (Value::Number(_), ValueType::Number)
                    | (Value::Boolean(_), ValueType::Boolean)
                    | (Value::FnPtr(_), ValueType::FnPtr)
                    | (Value::Record(_), ValueType::Record)
                    | (Value::List(_), ValueType::List)
                    | (Value::Runtime, ValueType::Runtime) => Value::Boolean(true),
                    _ => Value::Boolean(false),
                };

                self.registers.insert(i.result, is_type);
            }
            NewList(i) => {
                self.registers.insert(
                    i.result,
                    Value::List(List::new_gc(self.interpreter.next_alloc_id())),
                );
            }
            ListGet(i) => {
                let key = self.list_to_key(i.key)?;
                let list = self.get_list(i.list)?;
                let value = list.try_get(&key)?.clone();
                drop(list);
                self.registers.insert(i.result, value);
            }
            ListSet(i) => {
                let ListKey::Index(idx) = self.list_to_key(i.key)?;

                match i.value {
                    Some(register) => {
                        let value = self.get(register)?.clone();
                        let mut list = self.get_list_mut(i.list)?;
                        list.insert(idx, value);
                    }
                    None => {
                        let mut list = self.get_list_mut(i.list)?;
                        list.remove(idx);
                    }
                };
            }
            ListHasKey(i) => {
                let ListKey::Index(idx) = self.list_to_key(i.key)?;
                let list = self.get_list(i.list)?;
                let has_key = list.len() > idx;
                drop(list);
                self.registers.insert(i.result, Value::Boolean(has_key));
            }
            ListLen(i) => {
                let list = self.get_list(i.list)?;
                let len = list.len();
                drop(list);
                self.registers.insert(i.result, Value::Number(len as i64));
            }
            GetRuntime(i) => {
                self.registers.insert(i.result, Value::Runtime);
            }
            Unreachable(_) => {
                return Err(InstErr::Unreachable(Location::caller()));
            }
        }

        Ok(())
    }

    #[track_caller]
    fn to_key(&self, key: jssat_ir::isa::RecordKey<LiftedCtx>) -> InstResult<RecordKey> {
        match key {
            jssat_ir::isa::RecordKey::Prop(register) => self.value_to_key(self.get(register)?),
            jssat_ir::isa::RecordKey::Atom(atom) => Ok(RecordKey::Atom(atom)),
            jssat_ir::isa::RecordKey::DynAtom(register) => {
                Ok(RecordKey::Atom(self.get(register)?.try_into_atom()?))
            }
        }
    }

    #[track_caller]
    fn list_to_key(&self, key: jssat_ir::isa::ListKey<LiftedCtx>) -> InstResult<ListKey> {
        match key {
            jssat_ir::isa::ListKey::Index(register) => {
                let value = self.get(register)?;
                let idx = if let Value::Number(idx) = value {
                    (*idx)
                        .try_into()
                        .map_err(|_| ListInvalidIndex(*idx, Location::caller()))?
                } else {
                    return Err(ListInvalidIndexRegister(value.clone(), Location::caller()));
                };

                Ok(ListKey::Index(idx))
            }
        }
    }

    #[track_caller]
    fn value_to_key(&self, value: &Value) -> InstResult<RecordKey> {
        Ok(match value {
            Value::Boolean(value) => RecordKey::Boolean(*value),
            Value::Bytes(value) => RecordKey::Bytes(value.clone()),
            Value::FnPtr(value) => RecordKey::FnPtr(*value),
            Value::Atom(value) => RecordKey::Atom(*value),
            Value::Number(value) => RecordKey::Number(*value),
            // TODO: support using records as keys, as this is possible
            // in python (and i think lua)
            Value::Record(_) => {
                return Err(InstErr::InvalidRecKey(value.clone(), Location::caller()))
            }
            Value::List(_) => {
                return Err(InstErr::InvalidRecKey(value.clone(), Location::caller()))
            }
            // will never be possible
            Value::Runtime => {
                return Err(InstErr::InvalidRecKey(value.clone(), Location::caller()))
            }
        })
    }

    #[track_caller]
    fn get(&self, register: RegisterId) -> InstResult<&Value> {
        self.registers
            .get(&register)
            .ok_or_else(|| RegisterDNE(register, Location::caller()))
    }

    #[track_caller]
    fn get_record(&self, register: RegisterId) -> InstResult<gc::GcCellRef<Record>> {
        let value = self.get(register)?;
        value.try_into_record()
    }

    #[track_caller]
    fn get_record_mut(&mut self, register: RegisterId) -> InstResult<gc::GcCellRefMut<Record>> {
        let value = self.get(register)?;
        value.try_into_record_mut()
    }

    #[track_caller]
    fn get_list(&self, register: RegisterId) -> InstResult<gc::GcCellRef<List>> {
        let value = self.get(register)?;
        value.try_into_list()
    }

    #[track_caller]
    fn get_list_mut(&mut self, register: RegisterId) -> InstResult<gc::GcCellRefMut<List>> {
        let value = self.get(register)?;
        value.try_into_list_mut()
    }

    #[track_caller]
    fn get_ext_fn(&self, ext_fn_id: ExternalFunctionId) -> InstResult<&ExtFnImpl> {
        // TODO: should we also check that the external function exists in
        // `self.interpreter.program.external_functions`?
        (self.interpreter.external_fns)
            .get(&ext_fn_id)
            .ok_or_else(|| ExtFnDNE(ext_fn_id, Location::caller()))
    }

    #[track_caller]
    fn lookup_constant(&self, constant: ConstantId) -> InstResult<Vec<u8>> {
        (self.interpreter.code.constants)
            .get(&constant)
            .map(|c| c.payload.clone())
            .ok_or_else(|| ConstantDNE(constant, Location::caller()))
    }

    #[track_caller]
    fn load_args(&self, args: &[RegisterId]) -> InstResult<Vec<Value>> {
        args.iter()
            .map(|register| self.get(*register))
            .fold(Ok(vec![]), |acc, value| {
                let mut elems = acc?;
                let value = value?.clone();
                elems.push(value);
                Ok(elems)
            })
    }

    #[track_caller]
    fn call_fn(
        &mut self,
        args: &[RegisterId],
        fn_id: FunctionId,
        register: Option<RegisterId>,
    ) -> InstResult<()> {
        let args = self.load_args(args)?;

        // TODO: maybe carry around the calling interpreter?
        // explicitly not using `Interpreter::new` to show that we are
        // explicitly relying on internal implementation details (that
        // making an interpreter is cheap and stateless)
        //
        // EDIT^: yeah interpreter shouldn't be stateless

        // in the future, we want to provide a stacktrace of function
        // execution when we come across an error
        //
        // A*: at this point we'd want more detailed error information
        // about the stack trace
        let result = self.interpreter.execute_fn_id(fn_id, args)?; // <- A*

        self.store_fn_result(result, register)
    }

    #[track_caller]
    fn store_fn_result(
        &mut self,
        result: Option<Value>,
        register: Option<RegisterId>,
    ) -> InstResult<()> {
        match (register, result) {
            (Some(register), Some(value)) => {
                self.registers.insert(register, value);
            }
            (Some(_), None) => return Err(ExpectedNonVoid(Location::caller())),
            (None, _) => {}
        }

        Ok(())
    }
}
