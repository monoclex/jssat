//! The file where the actual symbolic execution work gets done

use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::frontend::assembler;
use crate::frontend::ir;
use crate::frontend::ir::Returns;
use crate::frontend::old_types;
use crate::frontend::type_annotater;
use crate::id::*;
use crate::isa::RecordKey;
use crate::isa::TrivialItem;
use crate::lifted;
use crate::lifted::{Function, LiftedProgram};
use crate::retag::CnstPassRetagger;
use crate::retag::ExtFnPassRetagger;
use crate::retag::FnPassRetagger;
use crate::retag::RegGenPassRetagger;
use crate::retag::RegGenRetagger;
use crate::retag::RegPassRetagger;
use crate::retag::RegRetagger;
use crate::symbolic_execution::types::{RegisterType, ReturnType};

use super::graph_system::Bogusable;
use super::{
    graph_system::{System, Worker},
    types::TypeBag,
    unique_id::UniqueFnIdShared,
};

#[derive(Clone, Copy, Debug)]
pub enum CurrentInstruction<'program> {
    None,
    Sequential(&'program ir::Instruction<LiftedCtx, LiftedCtx>),
    ControlFlow(&'program lifted::EndInstruction),
    Completed,
}

pub struct SymbWorker<'program> {
    pub program: &'program LiftedProgram,
    pub func: &'program Function,
    /// If this worker is from a `call`, then it's an entry function.
    /// Otherwise, it's part of a block.
    pub is_entry_fn: bool,
    pub id: <Self as Worker>::Id,
    pub fn_ids: UniqueFnIdShared,
    pub types: TypeBag,
    pub return_type: ReturnType,
    pub inst_on: CurrentInstruction<'program>,
    pub asm_ext_map: Arc<ExtFnPassRetagger<LiftedCtx, AssemblerCtx>>,
}

pub struct WorkerResults {
    pub is_entry_fn: bool,
    pub return_type: ReturnType,
    pub types: TypeBag,
    pub assembler_piece: assembler::Function,
}

impl WorkerResults {
    fn clone_ret_typ(&self, target_bag: &mut TypeBag) -> ReturnType {
        match self.return_type {
            ReturnType::Void => ReturnType::Void,
            ReturnType::Never => ReturnType::Never,
            ReturnType::Value(v) => ReturnType::Value(self.types.pull_type_into(v, target_bag)),
        }
    }
}

impl Bogusable for WorkerResults {
    fn bogus() -> Self {
        panic!("bogusability should not be required yet");
    }
}

impl<'p> Worker for SymbWorker<'p> {
    type Id = FunctionId<SymbolicCtx>;

    // TODO: the result of a worker should be a return type like Never
    // or something idk. maybe it's fine to leave it as self and hope that
    // callers only use the return type and not any other values
    type Result = WorkerResults;

    fn work(&mut self, system: &impl System<Self>) -> Self::Result {
        let mut never_infected = false;

        let mut blk = assembler::Block {
            register_types: Default::default(),
            parameters: vec![],
            instructions: vec![],
            // placeholder
            end: assembler::EndInstruction::Unreachable,
        };

        let mut asm_reg_map = RegPassRetagger::default();
        let mut asm_typs = old_types::RegMap::default();

        // in this code, we have <assember /> sections to seperate the parts
        // that build an assembler function from the symbolic execution parts.
        // this is because the assembler is going to be rewritten so i'd like
        // to cleanly see where all teh mess is
        // <assember>
        for p in self.func.parameters.iter() {
            // asm_reg_map.ignore_checks();
            blk.parameters.push(assembler::Parameter {
                typ: map_reg_assembler(&mut self.types, &mut asm_typs, *p),
                register: asm_reg_map.retag_new(*p),
            });
            // asm_reg_map.unignore_checks();
        }
        // </assember>

        for inst in self.func.instructions.iter() {
            self.inst_on = CurrentInstruction::Sequential(inst);

            match inst {
                ir::Instruction::Comment(_) => {}
                ir::Instruction::NewRecord(i) => {
                    self.types.new_record(i.result);
                }
                ir::Instruction::RecordGet(i) => {
                    let shape = self.types.record_shape(i.record);
                    let field_typ = self.types.get_field_type(shape, i.key);
                    self.types.assign_type(i.result, field_typ);
                }
                ir::Instruction::RecordSet(i) => {
                    let shape = self.types.record_shape(i.record);
                    let new_shape =
                        shape.new_with(self.types.conv_key(i.key), self.types.get(i.value));
                    self.types.append_shape(i.record, new_shape);
                }
                ir::Instruction::RecordHasKey(i) => {
                    let shape = self.types.record_shape(i.record);
                    let has_field = match self.types.has_field(shape, i.key) {
                        Some(b) => RegisterType::Bool(b),
                        None => RegisterType::Boolean,
                    };
                    self.types.assign_type(i.result, has_field);
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
                    let c = self.types.intern_constant(&c.payload);
                    self.types.assign_type(i.result, RegisterType::Byts(c));
                }
                ir::Instruction::MakeInteger(i) => {
                    self.types.assign_type(i.result, RegisterType::Int(i.value));
                }
                ir::Instruction::MakeBoolean(i) => {
                    self.types
                        .assign_type(i.result, RegisterType::Bool(i.value));
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
                        (RegisterType::Boolean, RegisterType::Boolean)
                        | (RegisterType::Boolean, RegisterType::Bool(_))
                        | (RegisterType::Bool(_), RegisterType::Boolean) => RegisterType::Boolean,
                        (RegisterType::Bool(a), RegisterType::Bool(b)) => {
                            RegisterType::Bool(a == b)
                        }
                        // TODO: should we allow equality like this?
                        (RegisterType::Trivial(_), RegisterType::Record(_)) => {
                            RegisterType::Bool(false)
                        }
                        (RegisterType::Record(_), RegisterType::Trivial(_)) => {
                            RegisterType::Bool(false)
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
                ir::Instruction::Or(i) => {
                    let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                    let res_typ = match (lhs, rhs) {
                        (RegisterType::Boolean, RegisterType::Boolean)
                        | (RegisterType::Bool(_), RegisterType::Boolean)
                        | (RegisterType::Boolean, RegisterType::Bool(_)) => RegisterType::Boolean,
                        (RegisterType::Bool(a), RegisterType::Bool(b)) => {
                            RegisterType::Bool(a || b)
                        }
                        (a, b) => panic!("cannot OR for {:?} and {:?}", a, b),
                    };

                    self.types.assign_type(i.result, res_typ);
                }
                ir::Instruction::And(i) => {
                    let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                    let res_typ = match (lhs, rhs) {
                        (RegisterType::Boolean, RegisterType::Boolean)
                        | (RegisterType::Bool(_), RegisterType::Boolean)
                        | (RegisterType::Boolean, RegisterType::Bool(_)) => RegisterType::Boolean,
                        (RegisterType::Bool(a), RegisterType::Bool(b)) => {
                            RegisterType::Bool(a && b)
                        }
                        (a, b) => panic!("cannot AND for {:?} and {:?}", a, b),
                    };

                    self.types.assign_type(i.result, res_typ);
                }
                ir::Instruction::CallVirt(i) => {
                    let fn_id = self.types.get_fnptr(i.fn_ptr);

                    // TODO: don't blatantly copy `CallStatic`
                    let src_fn = self.program.functions.get(&fn_id).unwrap();
                    debug_assert_eq!(src_fn.parameters.len(), i.args.len());
                    let map_args = (i.args.iter().copied()).zip(src_fn.parameters.iter().copied());
                    let types = self.types.extract_map(map_args);
                    let id = self.fn_ids.id_of(fn_id, types, true);
                    let r = system.spawn(id);

                    let return_type = r.clone_ret_typ(&mut self.types);

                    match (i.result, return_type) {
                        (_, ReturnType::Never) => {
                            never_infected = true;
                            break;
                        }
                        (None, ReturnType::Void) => {}
                        (None, ReturnType::Value(_)) => {}
                        (Some(r), ReturnType::Value(t)) => {
                            self.types.assign_type(r, t);
                        }
                        // TODO: better error message
                        (a, b) => panic!("incompatible return state {:?} {:?}", a, b),
                    };
                }
                ir::Instruction::CallStatic(i) => {
                    let src_fn = self.program.functions.get(&i.fn_id).unwrap();
                    debug_assert_eq!(src_fn.parameters.len(), i.args.len());
                    let map_args = (i.args.iter().copied()).zip(src_fn.parameters.iter().copied());
                    let types = self.types.extract_map(map_args);
                    let id = self.fn_ids.id_of(i.fn_id, types, true);
                    let r = system.spawn(id);

                    let return_type = r.clone_ret_typ(&mut self.types);

                    match (i.result, return_type) {
                        (_, ReturnType::Never) => {
                            never_infected = true;
                            break;
                        }
                        (None, ReturnType::Void) => {}
                        (None, ReturnType::Value(_)) => {}
                        (Some(r), ReturnType::Value(t)) => {
                            self.types.assign_type(r, t);
                        }
                        // TODO: better error message
                        (a, b) => panic!("incompatible return state {:?} {:?}", a, b),
                    };
                }
                ir::Instruction::CallExtern(i) => {
                    // TODO: ensure/make args are coercible into `fn_id`,
                    // although the `assembler` phase does this for us as of the time of writing
                    let ext_fn = self.program.external_functions.get(&i.fn_id).unwrap();

                    match (i.result, &ext_fn.returns) {
                        (Some(_), Returns::Void) => panic!("cannot assign `void` to register"),
                        (None, _) => {}
                        (Some(_), Returns::Value(_)) => {
                            todo!("handle return types of ext fns")
                        }
                    };
                }
            }

            // <assembler>
            let mut fn_retagger = FnPassRetagger::default();
            fn_retagger.ignore_checks();
            let mut c_retagger = CnstPassRetagger::default();
            c_retagger.ignore_checks();
            match inst.clone().retag(
                &mut asm_reg_map,
                &*self.asm_ext_map,
                &fn_retagger,
                &c_retagger,
            ) {
                ir::Instruction::Comment(i) => {
                    blk.instructions
                        .push(assembler::Instruction::Comment(i.message, i.location));
                }
                ir::Instruction::NewRecord(i) => {
                    let a = asm_typs.insert_alloc();
                    asm_typs.insert(i.result, type_annotater::ValueType::Record(a));
                    blk.instructions
                        .push(assembler::Instruction::RecordNew(i.result));
                }
                ir::Instruction::RecordGet(i) => {
                    if let type_annotater::ValueType::Record(a) = asm_typs.get(i.record) {
                        let shape = asm_typs.get_shape(*a);
                        let key = map_key_assembler(&asm_typs, i.key);
                        let t = shape.type_at_key(&key).clone();
                        asm_typs.insert(i.result, t);
                        blk.instructions.push(assembler::Instruction::RecordGet {
                            result: i.result,
                            record: i.record,
                            key: i.key,
                        });
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::RecordSet(i) => {
                    if let type_annotater::ValueType::Record(a) = *asm_typs.get(i.record) {
                        let shape = asm_typs.get_shape(a);
                        let value_typ = asm_typs.get(i.value).clone();
                        let key = map_key_assembler(&asm_typs, i.key);
                        let new_shape = shape.add_prop(key, value_typ);
                        let shape_id = asm_typs.insert_shape(new_shape);
                        asm_typs.assign_new_shape(a, shape_id);
                        blk.instructions.push(assembler::Instruction::RecordSet {
                            shape_id,
                            record: i.record,
                            key: i.key,
                            value: i.value,
                        });
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::RecordHasKey(i) => {
                    if let ir::Instruction::RecordHasKey(inst) = inst {
                        let result = self.types.get(inst.result);

                        match result {
                            RegisterType::Bool(b) => {
                                asm_typs.insert(i.result, type_annotater::ValueType::Bool(b));
                                blk.instructions
                                    .push(assembler::Instruction::MakeBoolean(i.result, b));
                            }
                            RegisterType::Boolean => {
                                todo!("cannot accomodate non-const field lookups")
                            }
                            _ => unreachable!("recordhaskey result should only be bool"),
                        }
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::GetFnPtr(i) => {
                    let asm_blk = BlockId::new_with_value_raw(i.function.raw_value());
                    asm_typs.insert(i.result, type_annotater::ValueType::FnPtr(asm_blk));
                    blk.instructions
                        .push(assembler::Instruction::MakeFnPtr(i.result, asm_blk));
                }
                ir::Instruction::CallStatic(i) => {
                    if let ir::Instruction::CallStatic(inst) = inst {
                        // TODO: dedup code
                        let src_fn = self.program.functions.get(&inst.fn_id).unwrap();
                        debug_assert_eq!(src_fn.parameters.len(), inst.args.len());
                        let map_args =
                            (inst.args.iter().copied()).zip(src_fn.parameters.iter().copied());
                        let types = self.types.extract_map(map_args);
                        let id = self.fn_ids.id_of(inst.fn_id, types, true);

                        // TODO: worry about `Never`
                        if let Some(result) = i.result {
                            let typ = self.types.get(inst.result.unwrap());
                            let typ = map_typ_assembler(&mut self.types, &mut asm_typs, typ);
                            asm_typs.insert(result, typ);
                        }

                        blk.instructions.push(assembler::Instruction::Call(
                            i.result,
                            assembler::Callable::Static(id.map_context()),
                            i.args,
                        ));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::CallExtern(i) => {
                    if let ir::Instruction::CallExtern(inst) = inst {
                        let func = self.program.external_functions.get(&inst.fn_id).unwrap();

                        // TODO: damn i need register ids to widen the src into
                        // and to do that i would need to maintain some register mapping crud
                        // AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA time to WING IT!!!!!!
                        let mut wing_it =
                            RegGenPassRetagger::new(RegisterId::new_with_value(69420));
                        // we don't actually need to map thigns but it wont compile unless i give it a type
                        // doing it like this rather than within the ...::<x>::new call for the above reason
                        wing_it.retag_new(RegisterId::<LiftedCtx>::default());

                        let mut call_insts = vec![];

                        debug_assert_eq!(inst.args.len(), func.parameters.len());
                        for (r, target_typ) in i.args.iter().zip(func.parameters.iter()) {
                            let src_vt = asm_typs.get(*r);
                            let target_vt = target_typ.clone().into_value_type();

                            if src_vt == &target_vt {
                                call_insts.push(*r);
                                continue;
                            } else {
                                let result = wing_it.gen();
                                call_insts.push(result);
                                blk.instructions.push(assembler::Instruction::Widen {
                                    result,
                                    input: *r,
                                    from: src_vt.clone(),
                                    to: target_vt,
                                });
                            }
                        }

                        // TODO: handle return types
                        assert!(matches!(i.result, None));

                        blk.instructions.push(assembler::Instruction::Call(
                            None,
                            assembler::Callable::Extern(i.fn_id),
                            call_insts,
                        ));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::CallVirt(i) => {
                    if let ir::Instruction::CallVirt(inst) = inst {
                        let func = self.types.get_fnptr(inst.fn_ptr);
                        // TODO: dedup code
                        let src_fn = self.program.functions.get(&func).unwrap();
                        debug_assert_eq!(src_fn.parameters.len(), inst.args.len());
                        let map_args =
                            (inst.args.iter().copied()).zip(src_fn.parameters.iter().copied());
                        let types = self.types.extract_map(map_args);
                        let id = self.fn_ids.id_of(func, types, true);

                        // TODO: worry about `Never`
                        if let Some(result) = i.result {
                            let typ = self.types.get(inst.result.unwrap());
                            let typ = map_typ_assembler(&mut self.types, &mut asm_typs, typ);
                            asm_typs.insert(result, typ);
                        }

                        blk.instructions.push(assembler::Instruction::Call(
                            i.result,
                            assembler::Callable::Static(id.map_context()),
                            i.args,
                        ));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::MakeTrivial(i) => {
                    let typ = map_typ_assembler(
                        &mut self.types,
                        &mut asm_typs,
                        RegisterType::Trivial(i.item),
                    );
                    asm_typs.insert(i.result, typ);
                    blk.instructions
                        .push(assembler::Instruction::MakeTrivial(i));
                }
                ir::Instruction::MakeBytes(i) => {
                    if let ir::Instruction::MakeBytes(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        if let RegisterType::Byts(c) = res_typ {
                            let bytes = self.types.unintern_const(c).clone();

                            blk.instructions.push(assembler::Instruction::MakeString(
                                i.result,
                                // this value is useless/ignored lol
                                c.map_context(),
                            ));

                            asm_typs
                                .insert(i.result, type_annotater::ValueType::ExactString(bytes));
                        }
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::MakeInteger(i) => {
                    if let ir::Instruction::MakeInteger(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        blk.instructions
                            .push(assembler::Instruction::MakeNumber(i.result, i.value));
                        asm_typs.insert(i.result, type_annotater::ValueType::ExactInteger(i.value));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::MakeBoolean(i) => {
                    if let ir::Instruction::MakeBoolean(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        blk.instructions
                            .push(assembler::Instruction::MakeBoolean(i.result, i.value));
                        asm_typs.insert(i.result, type_annotater::ValueType::Bool(i.value));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::LessThan(i) => {
                    if let ir::Instruction::LessThan(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        let b = match res_typ {
                            RegisterType::Bool(b) => {
                                blk.instructions
                                    .push(assembler::Instruction::MakeBoolean(i.result, b));
                                b
                            }
                            _ => todo!("assembler boilerplate cannot handle this atm"),
                        };

                        asm_typs.insert(i.result, type_annotater::ValueType::Bool(b));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::Equals(i) => {
                    if let ir::Instruction::Equals(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        let b = match res_typ {
                            RegisterType::Bool(b) => {
                                blk.instructions
                                    .push(assembler::Instruction::MakeBoolean(i.result, b));
                                b
                            }
                            _ => todo!("assembler boilerplate cannot handle this atm"),
                        };

                        asm_typs.insert(i.result, type_annotater::ValueType::Bool(b));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::Negate(i) => {
                    if let ir::Instruction::Negate(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        let b = match res_typ {
                            RegisterType::Bool(b) => {
                                blk.instructions
                                    .push(assembler::Instruction::MakeBoolean(i.result, b));
                                b
                            }
                            _ => todo!("assembler boilerplate cannot handle this atm"),
                        };

                        asm_typs.insert(i.result, type_annotater::ValueType::Bool(b));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::Add(i) => {
                    if let ir::Instruction::Add(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        let b = match res_typ {
                            RegisterType::Int(b) => {
                                blk.instructions
                                    .push(assembler::Instruction::MakeNumber(i.result, b));
                                b
                            }
                            _ => todo!("assembler boilerplate cannot handle this atm"),
                        };

                        asm_typs.insert(i.result, type_annotater::ValueType::ExactInteger(b));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::Or(i) => {
                    if let ir::Instruction::Or(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        let b = match res_typ {
                            RegisterType::Bool(b) => {
                                blk.instructions
                                    .push(assembler::Instruction::MakeBoolean(i.result, b));
                                b
                            }
                            _ => todo!("assembler boilerpalte cant hande this atm"),
                        };

                        asm_typs.insert(i.result, type_annotater::ValueType::Bool(b));
                    } else {
                        unreachable!();
                    }
                }
                ir::Instruction::And(i) => {
                    if let ir::Instruction::And(inst) = inst {
                        let res_typ = self.types.get(inst.result);

                        let b = match res_typ {
                            RegisterType::Bool(b) => {
                                blk.instructions
                                    .push(assembler::Instruction::MakeBoolean(i.result, b));
                                b
                            }
                            _ => todo!("assembler boilerpalte cant hande this atm"),
                        };

                        asm_typs.insert(i.result, type_annotater::ValueType::Bool(b));
                    } else {
                        unreachable!();
                    }
                }
            }
            // </assembler>
        }

        if never_infected {
            // TODO: write "unreachable" instruction
            self.return_type = ReturnType::Never;
        }

        self.inst_on = CurrentInstruction::ControlFlow(&self.func.end);
        let rs = match &self.func.end {
            crate::lifted::EndInstruction::Jump(i) => {
                let src_fn = self.program.functions.get(&i.0 .0).unwrap();
                debug_assert_eq!(src_fn.parameters.len(), i.0 .1.len());
                let map_args = (i.0 .1.iter().copied()).zip(src_fn.parameters.iter().copied());
                let types = self.types.extract_map(map_args);
                let id = self.fn_ids.id_of(i.0 .0, types, false);
                let r = system.spawn(id);
                self.return_type = r.clone_ret_typ(&mut self.types);
                // <assembler>
                blk.end = assembler::EndInstruction::Jump(assembler::BlockJump(
                    BlockId::new_with_value_raw(id.raw_value()),
                    i.0 .1.iter().map(|r| r.map_context()).collect(),
                ));
                // </assembler>
                vec![r]
            }
            crate::lifted::EndInstruction::JumpIf(i) => match self.types.get(i.condition) {
                RegisterType::Bool(true) => {
                    let src_fn = self.program.functions.get(&i.if_so.0).unwrap();
                    debug_assert_eq!(src_fn.parameters.len(), i.if_so.1.len());
                    let map_args =
                        (i.if_so.1.iter().copied()).zip(src_fn.parameters.iter().copied());
                    let types = self.types.extract_map(map_args);
                    let id = self.fn_ids.id_of(i.if_so.0, types, false);
                    let r = system.spawn(id);
                    self.return_type = r.clone_ret_typ(&mut self.types);
                    // <assembler>
                    blk.end = assembler::EndInstruction::Jump(assembler::BlockJump(
                        BlockId::new_with_value_raw(id.raw_value()),
                        i.if_so.1.iter().map(|r| r.map_context()).collect(),
                    ));
                    // </assembler>
                    vec![r]
                }
                RegisterType::Bool(false) => {
                    let src_fn = self.program.functions.get(&i.other.0).unwrap();
                    debug_assert_eq!(src_fn.parameters.len(), i.other.1.len());
                    let map_args =
                        (i.other.1.iter().copied()).zip(src_fn.parameters.iter().copied());
                    let types = self.types.extract_map(map_args);
                    let id = self.fn_ids.id_of(i.other.0, types, false);
                    let r = system.spawn(id);
                    self.return_type = r.clone_ret_typ(&mut self.types);
                    // <assembler>
                    blk.end = assembler::EndInstruction::Jump(assembler::BlockJump(
                        BlockId::new_with_value_raw(id.raw_value()),
                        i.other.1.iter().map(|r| r.map_context()).collect(),
                    ));
                    // </assembler>
                    vec![r]
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
                // <assembler>
                blk.end = assembler::EndInstruction::Return(i.0.map(|r| r.map_context()));
                // </assembler>
                vec![]
            }
        };

        // <assembler>
        let return_type = match self.return_type {
            ReturnType::Void => assembler::ReturnType::Void,
            ReturnType::Value(v) => {
                println!("i am getting it: {:#?}", self.id);
                assembler::ReturnType::Value(map_typ_assembler(&mut self.types, &mut asm_typs, v))
            }
            ReturnType::Never => assembler::ReturnType::Void,
        };
        let me_block_id = BlockId::new_with_value_raw(self.id.raw_value());
        let mut blocks = FxHashMap::default();
        blk.register_types = asm_typs;
        blocks.insert(me_block_id, blk);
        for r in rs {
            blocks.extend(r.assembler_piece.blocks.clone());
        }
        let func = assembler::Function {
            entry_block: me_block_id,
            blocks,
            return_type,
        };
        // </assembler>

        self.inst_on = CurrentInstruction::Completed;

        let types = self.types.clone();

        WorkerResults {
            is_entry_fn: self.is_entry_fn,
            return_type: self.return_type,
            types,
            assembler_piece: func,
        }
    }
}

// <assembler>
fn map_reg_assembler(
    types: &mut TypeBag,
    asm_typs: &mut old_types::RegMap<AssemblerCtx>,
    reg: RegisterId<LiftedCtx>,
) -> type_annotater::ValueType {
    let typ = types.get(reg);
    let t = map_typ_assembler(types, asm_typs, typ);
    asm_typs.insert(reg.map_context(), t.clone());
    t
}

fn map_key_assembler(
    asm_typs: &old_types::RegMap<AssemblerCtx>,
    key: RecordKey<AssemblerCtx>,
) -> old_types::ShapeKey {
    match key {
        RecordKey::Prop(r) => map_keyt_assembler(asm_typs, asm_typs.get(r)),
        RecordKey::Slot(s) => old_types::ShapeKey::InternalSlot(s),
    }
}

fn map_keyt_assembler(
    asm_typs: &old_types::RegMap<AssemblerCtx>,
    key: &type_annotater::ValueType,
) -> old_types::ShapeKey {
    match key {
        type_annotater::ValueType::String => old_types::ShapeKey::String,
        type_annotater::ValueType::ExactString(s) => old_types::ShapeKey::Str(s.clone()),
        type_annotater::ValueType::Any
        | type_annotater::ValueType::Runtime
        | type_annotater::ValueType::Number
        | type_annotater::ValueType::ExactInteger(_)
        | type_annotater::ValueType::Boolean
        | type_annotater::ValueType::Bool(_)
        | type_annotater::ValueType::Record(_)
        | type_annotater::ValueType::FnPtr(_)
        | type_annotater::ValueType::Null
        | type_annotater::ValueType::Undefined => unimplemented!("unsupported conversion"),
    }
}

fn map_typ_assembler(
    types: &mut TypeBag,
    asm_typs: &mut old_types::RegMap<AssemblerCtx>,
    typ: RegisterType,
) -> type_annotater::ValueType {
    match typ {
        RegisterType::Any => type_annotater::ValueType::Any,
        RegisterType::Trivial(t) => match t {
            TrivialItem::Runtime => type_annotater::ValueType::Runtime,
            TrivialItem::Null => type_annotater::ValueType::Null,
            TrivialItem::Undefined => type_annotater::ValueType::Undefined,
            TrivialItem::Empty => todo!("trivial `Empty` not mapped yett"),
            _ => todo!("unsupported trivial item"),
        },
        RegisterType::Bytes => type_annotater::ValueType::String,
        RegisterType::Byts(c) => {
            type_annotater::ValueType::ExactString(types.unintern_const(c).clone())
        }
        RegisterType::Number => type_annotater::ValueType::Number,
        RegisterType::Int(v) => type_annotater::ValueType::ExactInteger(v),
        RegisterType::Boolean => type_annotater::ValueType::Boolean,
        RegisterType::Bool(b) => type_annotater::ValueType::Bool(b),
        RegisterType::FnPtr(f) => {
            type_annotater::ValueType::FnPtr(BlockId::new_with_value_raw(f.raw_value()))
        }
        RegisterType::Record(orig_a) => {
            let a = orig_a.map_context();
            if asm_typs.allocations.get(&a).is_some() {
                return type_annotater::ValueType::Record(a);
            }

            let mut new_shape = old_types::RecordShape::default();

            let shape_id = types.get_shape_id(orig_a);
            let shape = types.get_shape(shape_id).clone();

            // insert into `asm_typs` asap to solve self referential objects
            let shape_id = shape_id.map_context();
            asm_typs.allocations.insert(a, vec![shape_id]);

            for (k, v) in shape.fields.iter() {
                let k = match *k {
                    crate::symbolic_execution::types::ShapeKey::Str(s) => {
                        old_types::ShapeKey::Str(types.unintern_const(s).clone())
                    }
                    crate::symbolic_execution::types::ShapeKey::Slot(s) => {
                        old_types::ShapeKey::InternalSlot(s)
                    }
                };

                let v = map_typ_assembler(types, asm_typs, *v);
                new_shape = new_shape.add_prop(k, v);
            }

            asm_typs.shapes.insert(shape_id, new_shape);

            type_annotater::ValueType::Record(a)
        }
    }
}
// </assembler>
