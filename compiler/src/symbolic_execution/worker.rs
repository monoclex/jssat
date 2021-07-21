//! The file where the actual symbolic execution work gets done

use crate::frontend::ir;
use crate::id::*;
use crate::isa::TrivialItem;
use crate::lifted::{Function, LiftedProgram};
use crate::symbolic_execution::types::{RegisterType, ReturnType};

use super::{
    graph_system::{System, Worker},
    types::TypeBag,
    unique_id::UniqueFnIdShared,
};

pub struct SymbWorker<'program> {
    pub program: &'program LiftedProgram,
    pub func: &'program Function,
    pub id: <Self as Worker>::Id,
    pub fn_ids: UniqueFnIdShared,
    pub types: TypeBag,
    pub return_type: ReturnType,
}

impl<'p> Worker for SymbWorker<'p> {
    type Id = FunctionId<SymbolicCtx>;

    // TODO: the result of a worker should be a return type like Never
    // or something idk. maybe it's fine to leave it as self and hope that
    // callers only use the return type and not any other values
    type Result = Self;

    fn work(mut self, system: &impl System<Self>) -> Self::Result {
        let mut never_infected = false;

        for inst in self.func.instructions.iter() {
            match inst {
                ir::Instruction::Comment(_, _) => {}
                ir::Instruction::NewRecord(i) => {
                    self.types.new_record(i.result);
                }
                ir::Instruction::RecordGet(i) => {
                    let shape = self.types.record_shape(i.record);
                    let field_typ = shape.get_typ(self.types.conv_key(i.key));
                    self.types.assign_type(i.result, field_typ);
                }
                ir::Instruction::RecordSet(i) => {
                    let shape = self.types.record_shape(i.record);
                    let new_shape =
                        shape.new_with(self.types.conv_key(i.key), self.types.get(i.value));
                    self.types.append_shape(i.record, new_shape);
                }
                ir::Instruction::GetFnPtr(i) => {
                    self.types
                        .assign_type(i.result, RegisterType::FnPtr(i.function));
                }
                ir::Instruction::MakeTrivial(i) => {
                    self.types
                        .assign_type(i.result, RegisterType::Trivial(i.item));
                }
                ir::Instruction::MakeBytes(i) => {
                    let c = self.program.constants.get(&i.constant).unwrap();
                    let c = self.types.intern_constant(c.payload.clone());
                    self.types.assign_type(i.result, RegisterType::Byts(c));
                }
                ir::Instruction::MakeInteger(i) => {
                    self.types.assign_type(i.result, RegisterType::Int(i.value));
                }
                ir::Instruction::LessThan(i) => {
                    let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                    let res_typ = match (lhs, rhs) {
                        (RegisterType::Number, RegisterType::Number)
                        | (RegisterType::Number, RegisterType::Int(_))
                        | (RegisterType::Int(_), RegisterType::Number) => RegisterType::Boolean,
                        (RegisterType::Int(a), RegisterType::Int(b)) => RegisterType::Bool(a < b),
                        (a, b) => panic!("cannot less than for {:?} and {:?}", a, b),
                    };

                    self.types.assign_type(i.result, res_typ);
                }
                ir::Instruction::Equals(i) => {
                    let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                    // TODO: equals should be specified **for each type**
                    // ^ because i hate implicit conversions. makes debugging hard
                    let res_typ = match (lhs, rhs) {
                        (RegisterType::Number, RegisterType::Number)
                        | (RegisterType::Number, RegisterType::Int(_))
                        | (RegisterType::Int(_), RegisterType::Number) => RegisterType::Boolean,
                        (RegisterType::Int(a), RegisterType::Int(b)) => RegisterType::Bool(a == b),
                        (RegisterType::Bytes, RegisterType::Bytes)
                        | (RegisterType::Bytes, RegisterType::Byts(_))
                        | (RegisterType::Byts(_), RegisterType::Bytes) => RegisterType::Boolean,
                        (RegisterType::Byts(a), RegisterType::Byts(b)) => RegisterType::Bool(
                            self.types.unintern_const(a) == self.types.unintern_const(b),
                        ),
                        (RegisterType::Trivial(a), RegisterType::Trivial(b)) => {
                            RegisterType::Bool(a == b)
                        }
                        (a, b) => panic!("cannot equals for {:?} and {:?}", a, b),
                    };

                    self.types.assign_type(i.result, res_typ);
                }
                ir::Instruction::Negate(i) => {
                    let o = self.types.get(i.operand);

                    let res_typ = match o {
                        RegisterType::Boolean => RegisterType::Boolean,
                        RegisterType::Bool(b) => RegisterType::Bool(!b),
                        o => panic!("cannot negate for {:?}", o),
                    };

                    self.types.assign_type(i.result, res_typ);
                }
                ir::Instruction::Add(i) => {
                    let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                    let res_typ = match (lhs, rhs) {
                        (RegisterType::Number, RegisterType::Number)
                        | (RegisterType::Number, RegisterType::Int(_))
                        | (RegisterType::Int(_), RegisterType::Number) => RegisterType::Number,
                        (RegisterType::Int(a), RegisterType::Int(b)) => RegisterType::Int(a + b),
                        (a, b) => panic!("cannot add for {:?} and {:?}", a, b),
                    };

                    self.types.assign_type(i.result, res_typ);
                }
                ir::Instruction::CallVirt(i) => {
                    let fn_id = self.types.get_fnptr(i.fn_ptr);

                    // TODO: don't blatantly copy `CallStatic`
                    let types = self.types.extract(&i.args);
                    let id = self.fn_ids.id_of(fn_id, types);
                    let r = system.spawn(id);

                    match (i.result, r.return_type) {
                        (_, ReturnType::Never) => {
                            never_infected = true;
                            break;
                        }
                        (None, ReturnType::Void) => continue,
                        (None, ReturnType::Value(_)) => continue,
                        (Some(r), ReturnType::Value(t)) => {
                            self.types.assign_type(r, t);
                            continue;
                        }
                        // TODO: better error message
                        (a, b) => panic!("incompatible return state {:?} {:?}", a, b),
                    };
                }
                ir::Instruction::CallStatic(i) => {
                    let types = self.types.extract(&i.args);
                    let id = self.fn_ids.id_of(i.fn_id, types);
                    let r = system.spawn(id);

                    match (i.result, r.return_type) {
                        (_, ReturnType::Never) => {
                            never_infected = true;
                            break;
                        }
                        (None, ReturnType::Void) => continue,
                        (None, ReturnType::Value(_)) => continue,
                        (Some(r), ReturnType::Value(t)) => {
                            self.types.assign_type(r, t);
                            continue;
                        }
                        // TODO: better error message
                        (a, b) => panic!("incompatible return state {:?} {:?}", a, b),
                    };
                }
                ir::Instruction::CallExtern(i) => {
                    todo!("widening instructions and crud");
                }
            }
        }

        if never_infected {
            // TODO: write "unreachable" instruction
            self.return_type = ReturnType::Never;
        }

        match &self.func.end {
            crate::lifted::EndInstruction::Jump(i) => {
                let types = self.types.extract(&i.0 .1);
                let id = self.fn_ids.id_of(i.0 .0, types);
                let r = system.spawn(id);
                self.return_type = r.return_type;
            }
            crate::lifted::EndInstruction::JumpIf(i) => match self.types.get(i.condition) {
                RegisterType::Bool(true) => {
                    let types = self.types.extract(&i.if_so.1);
                    let id = self.fn_ids.id_of(i.if_so.0, types);
                    let r = system.spawn(id);
                    self.return_type = r.return_type;
                }
                RegisterType::Bool(false) => {
                    let types = self.types.extract(&i.other.1);
                    let id = self.fn_ids.id_of(i.other.0, types);
                    let r = system.spawn(id);
                    self.return_type = r.return_type;
                }
                RegisterType::Boolean => {
                    todo!()
                }
                r => unimplemented!("cannot use non-boolean register as conditional {:?}", r),
            },
            crate::lifted::EndInstruction::Return(i) => {
                match i.0 {
                    Some(r) => self.return_type = ReturnType::Value(self.types.get(r)),
                    None => self.return_type = ReturnType::Void,
                };
            }
        }

        self
    }
}
