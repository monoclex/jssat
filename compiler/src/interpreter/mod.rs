//! A JSSAT IR interpreter. This is primarily used for unit testing.
//!
//! In order to ensure that the JSSAT IR builder APIs produce the correct code,
//! and that the interpreter correctly understands the produced code, a JSSAT
//! interpreter exists to run some JSSAT IR and then produce results about it.
//!
//! Performance is not necessarily a goal, although in the future that may be
//! reconsidered.

mod build;
pub use build::*;
use petgraph::graph::DiGraph;

#[cfg(test)]
mod tests;

use std::convert::TryInto;
use std::panic::Location;

use derive_more::{Deref, DerefMut};
use gc::{custom_trace, BorrowError, BorrowMutError, Finalize, Gc, GcCell, Trace};
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::isa::{Atom, CompareType, ValueType};
use crate::lifted::Function;
use crate::{collections::StrictZip, isa::BinaryOperator};

use crate::{
    frontend::ir::Instruction,
    id::LiftedCtx,
    lifted::{
        ConstantId, EndInstruction, ExternalFunctionId, FunctionId, LiftedProgram, RegisterId,
    },
};

pub struct Interpreter<'parent> {
    code: &'parent LiftedProgram,
    external_fns: &'parent FxHashMap<ExternalFunctionId, ExtFnImpl>,
    current_comment: FxHashMap<usize, crate::isa::Comment>,
    top: Option<FunctionId>,
    callstack_path: Vec<Edge>,
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
    // TODO: implement
    Symbol(()),
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

#[derive(Deref, DerefMut, Debug, Finalize, Default)]
pub struct Record(FxHashMap<RecordKey, Value>);

impl Record {
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
            for (_, v) in this.0.iter() {
                mark(v);
            }
        }
    }
}

#[derive(Deref, DerefMut, Debug, Finalize, Trace, Default)]
pub struct List(Vec<Value>);

impl List {
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

impl<'p> Interpreter<'p> {
    pub fn new(
        code: &'p LiftedProgram,
        ext_fns: &'p FxHashMap<ExternalFunctionId, ExtFnImpl>,
    ) -> Self {
        Interpreter {
            code,
            external_fns: ext_fns,
            current_comment: Default::default(),
            top: None,
            callstack_path: Vec::new(),
        }
    }

    pub fn execute_fn_id(&mut self, id: FunctionId, args: Vec<Value>) -> InstResult<Option<Value>> {
        let parent = self.top;

        let edge = Edge {
            goes_in: true,
            from: parent,
            to: Some(id),
        };
        self.callstack_path.push(edge);
        self.top = Some(id);

        const KiB: usize = 1024;
        const MiB: usize = 1024 * KiB;
        let results = stacker::maybe_grow(32 * KiB, 4 * MiB, || self.execute_fn_id_impl(id, args));

        if results.is_ok() {
            let edge = Edge {
                goes_in: false,
                from: Some(id),
                to: parent,
            };
            self.callstack_path.push(edge);
            self.top = parent;
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

        for inst in function.instructions.iter() {
            inst_exec.exec(inst)?;
        }

        match &function.end {
            EndInstruction::Jump(i) => {
                let args = inst_exec.load_args(&i.0 .1)?;
                self.execute_fn_id(i.0 .0, args)
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
                self.execute_fn_id(jump.0, args)
            }
            EndInstruction::Return(i) => match i.0 {
                Some(register) => {
                    let value = inst_exec.get(register)?;
                    Ok(Some(value.clone()))
                }
                None => Ok(None),
            },
        }
    }
}

impl<'p> Interpreter<'p> {
    pub fn print_callstack_depth(&self, mut depth: usize) -> String {
        if depth == 0 {
            depth = usize::MAX
        };

        let mut s = String::new();
        let mut indent = String::new();

        let name = |x| match x {
            Some(x) => match &self.code.functions.get(&x).unwrap().name {
                Some(y) => format!("{}({})", y, x),
                None => format!("{}", x),
            },
            None => "".to_string(),
        };
        let arrow = |x| if x { "->" } else { "<-" };

        for Edge { from, goes_in, to } in self.callstack_path.iter().rev().take(depth).rev() {
            let path = format!("{}{}{}", name(*from), arrow(*goes_in), name(*to));

            if !*goes_in {
                indent.pop();
            }

            s.push_str(&format!("{}{}\n", indent, path));

            if *goes_in {
                indent.push(' ');
            }
        }

        s
    }

    pub fn construct_callstack_graph(&self, mut depth: usize) -> String {
        if depth == 0 {
            depth = usize::MAX
        };
        let mut graph = DiGraph::new();

        struct DebugIsDisplay<T>(T);
        impl<T> std::fmt::Debug for DebugIsDisplay<T>
        where
            T: std::fmt::Display,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        let root = graph.add_node(DebugIsDisplay("ROOT".to_string()));

        let mut nodes = FxHashMap::default();

        let mut add_node = |id| {
            if nodes.get(&id).is_some() {
                return;
            }

            let body = self.code.functions.get(&id).unwrap();
            let code = crate::lifted::display(id, body);
            let node = graph.add_node(DebugIsDisplay(code));
            nodes.insert(id, node);
        };

        for Edge { from, to, .. } in self.callstack_path.iter().rev().take(depth).rev() {
            if let Some(id) = from {
                add_node(*id);
            }

            if let Some(id) = to {
                add_node(*id);
            }
        }

        let get_node = |id| {
            if let Some(id) = nodes.get(&id) {
                return *id;
            }

            unreachable!("wat");
        };

        for (
            edge_num,
            Edge {
                from,
                goes_in: _,
                to,
            },
        ) in self
            .callstack_path
            .iter()
            .enumerate()
            .rev()
            .take(depth)
            .rev()
        {
            let from = from.map(&get_node).unwrap_or(root);
            let to = to.map(&get_node).unwrap_or(root);

            graph.add_edge(from, to, edge_num);
        }

        let dot = petgraph::dot::Dot::with_config(&graph, &[petgraph::dot::Config::EdgeIndexLabel]);
        format!("{:?}", dot)
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
        #[cfg(debug_assertions)]
        if let Instruction::Comment(comment) = inst {
            self.interpreter
                .current_comment
                .insert(self.function.ir_fn_id.get_the_value(), *comment);
        }

        #[cfg(debug_assertions)]
        if let Some(c) = self
            .interpreter
            .current_comment
            .get(&self.function.ir_fn_id.get_the_value())
        {
            // println!(":: {:?}", c);
        }

        if let Some(result) = inst.assigned_to() {
            assert!(
                !self.registers.contains_key(&result),
                "SSA form must be maintained"
            );
        }

        use crate::frontend::ir::Instruction::*;
        match inst {
            Comment(_) => {}
            NewRecord(i) => {
                self.registers
                    .insert(i.result, Value::Record(Default::default()));
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

                use crate::isa::BinaryOperator::*;
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
                self.registers
                    .insert(i.result, Value::List(Default::default()));
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
    fn to_key(&self, key: crate::isa::RecordKey<LiftedCtx>) -> InstResult<RecordKey> {
        match key {
            crate::isa::RecordKey::Prop(register) => self.value_to_key(self.get(register)?),
            crate::isa::RecordKey::Atom(atom) => Ok(RecordKey::Atom(atom)),
        }
    }

    #[track_caller]
    fn list_to_key(&self, key: crate::isa::ListKey<LiftedCtx>) -> InstResult<ListKey> {
        match key {
            crate::isa::ListKey::Index(register) => {
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
