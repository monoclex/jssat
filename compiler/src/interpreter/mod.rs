//! A JSSAT IR interpreter. This is primarily used for unit testing.
//!
//! In order to ensure that the JSSAT IR builder APIs produce the correct code,
//! and that the interpreter correctly understands the produced code, a JSSAT
//! interpreter exists to run some JSSAT IR and then produce results about it.
//!
//! Performance is not necessarily a goal, although in the future that may be
//! reconsidered.

use std::{
    borrow::{Borrow, BorrowMut},
    ops::DerefMut,
    panic::Location,
};

use derive_more::{Deref, DerefMut};
use gc::{custom_trace, Finalize, Gc, GcCell, Trace};
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::{collections::StrictZip, isa::BinaryOperator};

use crate::{
    frontend::ir::Instruction,
    id::LiftedCtx,
    isa::{InternalSlot, TrivialItem},
    lifted::{ConstantId, EndInstruction, FunctionId, LiftedProgram, RegisterId},
};

pub struct Interpreter<'code> {
    code: &'code LiftedProgram,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum RecordKey {
    Slot(InternalSlot),
    Trivial(TrivialItem),
    // TODO: use GC'd Vec<u8>s to prevent excessive cloning
    Bytes(Vec<u8>),
    Number(i64),
    Boolean(bool),
    FnPtr(FunctionId),
}

#[derive(Clone, Debug, Trace, Finalize)]
pub enum Value {
    Trivial(#[unsafe_ignore_trace] TrivialItem),
    // TODO: use GC'd Vec<u8>s to prevent excessive cloning
    Bytes(Vec<u8>),
    Number(i64),
    Boolean(bool),
    FnPtr(#[unsafe_ignore_trace] FunctionId),
    Record(Gc<GcCell<Record>>),
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

impl<'c> Interpreter<'c> {
    pub fn new(code: &'c LiftedProgram) -> Self {
        Interpreter { code }
    }

    pub fn execute_fn_id(&self, id: FunctionId, args: Vec<Value>) -> InstResult<Value> {
        let function = (self.code.functions)
            .get(&id)
            .expect("expected valid function id");

        assert_eq!(function.parameters.len(), args.len());

        let mut registers = FxHashMap::default();
        for (register, value) in function.parameters.iter().strict_zip(args) {
            registers.insert(*register, value);
        }

        let mut inst_exec = InstExec {
            registers,
            program: self.code,
        };

        for inst in function.instructions.iter() {
            inst_exec.exec(inst)?;
        }

        match function.end {
            EndInstruction::Jump(_) => todo!(),
            EndInstruction::JumpIf(_) => todo!(),
            EndInstruction::Return(_) => todo!(),
        }
    }
}

struct InstExec<'code> {
    registers: FxHashMap<RegisterId, Value>,
    program: &'code LiftedProgram,
}

pub type InstResult<T> = Result<T, InstErr>;
type PanicLocation = &'static Location<'static>;

#[derive(Error, Debug)]
pub enum InstErr {
    #[error("Register does not exist: {}", .0)]
    RegisterDNE(RegisterId, PanicLocation),
    // TODO: wrap `Value` in something that formats better
    #[error("Invalid key used as register key: {:?}", .0)]
    InvalidRecKey(Value, PanicLocation),
    // TODO: wrap `RecordKey` in  something that formats better
    #[error("Record does not contain key: {:?}", .0)]
    RecordDNCKey(RecordKey, PanicLocation),
    #[error("Constant does not exist: {}", .0)]
    ConstantDNE(ConstantId, PanicLocation),
    #[error("Unable to perform binary operation: {:?} `{}` {:?}", .0, .2, .1)]
    BinOpFail(Value, Value, BinaryOperator, PanicLocation),
    #[error("Unable to perform unary operation: `{}` {:?}", .1, .0)]
    UnaryOpFail(Value, &'static str, PanicLocation),
    #[error("Unable to call virtual function, as register is not a fnptr: {:?}", .0)]
    NonVirt(Value, PanicLocation),

    // TODO: support external function calls
    // they can be implemented by having some kind of rust function be paired
    // with an external function id, and then running that function when
    // the interpreter encounters it. the external function has to be given the
    // parameters it wants and then can do something with interpreter state idk
    #[error("Unable to perform external function calls at this time")]
    ExternalFnCall,
}

use InstErr::*;

impl<'c> InstExec<'c> {
    pub fn exec(&mut self, inst: &Instruction<LiftedCtx, LiftedCtx>) -> InstResult<()> {
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
                self.call_fn(&i.args, i.calling, i.result);
            }
            CallExtern(i) => {
                return Err(ExternalFnCall);
            }
            CallVirt(i) => {
                let value = self.get(i.calling)?;

                let fn_id = match value {
                    Value::FnPtr(fn_id) => *fn_id,
                    _ => return Err(NonVirt(value.clone(), Location::caller())),
                };

                self.call_fn(&i.args, fn_id, i.result);
            }
            MakeTrivial(i) => {
                self.registers.insert(i.result, Value::Trivial(i.item));
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
                        (Trivial(lhs), Trivial(rhs)) => lhs == rhs,
                        _ => return fail(),
                    }),
                    LessThan => return fail(),
                };

                self.registers.insert(i.result, value);
            }
            Negate(i) => {
                let value = self.get(i.operand)?;

                let fail = || Err(UnaryOpFail(value.clone(), "!", Location::caller()));

                use Value::*;
                let value = match value {
                    Boolean(b) => Boolean(!*b),
                    _ => return fail(),
                };

                self.registers.insert(i.result, value);
            }
            Generalize(_) => {
                todo!("generalize not used yet")
            }
        }

        Ok(())
    }

    #[track_caller]
    fn to_key(&self, key: crate::isa::RecordKey<LiftedCtx>) -> InstResult<RecordKey> {
        match key {
            crate::isa::RecordKey::Prop(register) => self.value_to_key(self.get(register)?),
            crate::isa::RecordKey::Slot(slot) => Ok(RecordKey::Slot(slot)),
        }
    }

    #[track_caller]
    fn value_to_key(&self, value: &Value) -> InstResult<RecordKey> {
        Ok(match value {
            Value::Boolean(value) => RecordKey::Boolean(*value),
            Value::Bytes(value) => RecordKey::Bytes(value.clone()),
            Value::FnPtr(value) => RecordKey::FnPtr(*value),
            Value::Trivial(value) => RecordKey::Trivial(*value),
            Value::Number(value) => RecordKey::Number(*value),
            // TODO: support using records as keys, as this is possible
            // in python (and i think lua)
            Value::Record(_) => {
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
    fn get_mut(&mut self, register: RegisterId) -> InstResult<&mut Value> {
        self.registers
            .get_mut(&register)
            .ok_or_else(|| RegisterDNE(register, Location::caller()))
    }

    #[track_caller]
    fn get_record(&self, register: RegisterId) -> InstResult<gc::GcCellRef<Record>> {
        match self.get(register)? {
            // writing `Ok(record.borrow())` causes rustc to overflow
            // while trying to evaluate Add for OrderedFloat (...??? wtf????)
            Value::Record(record) => Ok(record.try_borrow().unwrap()),
            _ => todo!(),
        }
    }

    #[track_caller]
    fn get_record_mut(&mut self, register: RegisterId) -> InstResult<gc::GcCellRefMut<Record>> {
        match self.get_mut(register)? {
            Value::Record(record) => Ok(record.try_borrow_mut().unwrap()),
            _ => todo!(),
        }
    }

    #[track_caller]
    fn lookup_constant(&self, constant: ConstantId) -> InstResult<Vec<u8>> {
        (self.program.constants)
            .get(&constant)
            .map(|c| c.payload.clone())
            .ok_or_else(|| ConstantDNE(constant, Location::caller()))
    }

    #[track_caller]
    fn call_fn(
        &mut self,
        args: &[RegisterId],
        fn_id: FunctionId,
        store_result: Option<RegisterId>,
    ) -> InstResult<()> {
        let args = args.iter().map(|register| self.get(*register)).fold(
            Ok(vec![]),
            |mut acc, value| {
                let mut elems = acc?;
                let value = value?.clone();
                elems.push(value);
                Ok(elems)
            },
        )?;

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
        let interpreter = Interpreter { code: self.program };
        let result = interpreter.execute_fn_id(fn_id, args)?; // <- A*

        if let Some(register_result) = store_result {
            self.registers.insert(register_result, result);
        }

        Ok(())
    }
}
