//! The file where the actual symbolic execution work gets done

use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::frontend::ir;
use crate::frontend::ir::Returns;
use crate::id::*;
use crate::lifted;
use crate::lifted::{Function, LiftedProgram};
use crate::retag::ExtFnPassRetagger;
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
    pub never_infected: bool,
}

pub struct WorkerResults {
    pub is_entry_fn: bool,
    pub return_type: ReturnType,
    pub types: TypeBag,
}

impl WorkerResults {
    // TODO: there is probably a bug with this method in regards to object types
    // since the allocation map isn't used to map records into the source typebag
    fn clone_ret_typ(
        &self,
        target_bag: &mut TypeBag,
        me_to_target_typs: &FxHashMap<AllocationId<LiftedCtx>, AllocationId<LiftedCtx>>,
    ) -> ReturnType {
        let mut rev_alloc_map = me_to_target_typs
            .iter()
            .map(|(k, v)| (*v, *k))
            .collect::<FxHashMap<_, _>>();

        match self.return_type {
            ReturnType::Void => ReturnType::Void,
            ReturnType::Never => ReturnType::Never,
            ReturnType::Value(v) => {
                ReturnType::Value(self.types.pull_type_into(v, target_bag, &rev_alloc_map))
            }
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
                    match self.types.get(i.record) {
                        RegisterType::Record(_) => {
                            let shape = self.types.record_shape(i.record);
                            let has_field = match self.types.has_field(shape, i.key) {
                                Some(b) => RegisterType::Bool(b),
                                None => RegisterType::Boolean,
                            };
                            self.types.assign_type(i.result, has_field);
                        }
                        _ => {
                            // TODO: this should DEFINITELY not be here
                            // we should have PROPER type instructions but i'm LAZY
                            // so i'm adidng a shortcut for recordhaskey to fail if it's
                            // not a record
                            self.types.assign_type(i.result, RegisterType::Bool(false));
                        }
                    };
                }
                ir::Instruction::GetFnPtr(i) => {
                    self.types
                        .assign_type(i.result, RegisterType::FnPtr(i.item));
                }
                ir::Instruction::MakeTrivial(i) => {
                    self.types
                        .assign_type(i.result, RegisterType::Trivial(i.item));
                }
                ir::Instruction::MakeBytes(i) => {
                    let c = self.program.constants.get(&i.item).unwrap();
                    let c = self.types.intern_constant(&c.payload);
                    self.types.assign_type(i.result, RegisterType::Byts(c));
                }
                ir::Instruction::MakeInteger(i) => {
                    self.types.assign_type(i.result, RegisterType::Int(i.item));
                }
                ir::Instruction::MakeBoolean(i) => {
                    self.types.assign_type(i.result, RegisterType::Bool(i.item));
                }
                ir::Instruction::BinOp(_) => todo!(),
                // ir::Instruction::LessThan(i) => {
                //     let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                //     let res_typ = match (lhs, rhs) {
                //         (RegisterType::Number, RegisterType::Number)
                //         | (RegisterType::Number, RegisterType::Int(_))
                //         | (RegisterType::Int(_), RegisterType::Number) => RegisterType::Boolean,
                //         (RegisterType::Int(a), RegisterType::Int(b)) => RegisterType::Bool(a < b),
                //         (a, b) => panic!("cannot less than for {:?} and {:?}", a, b),
                //     };

                //     self.types.assign_type(i.result, res_typ);
                // }
                // ir::Instruction::Equals(i) => {
                //     let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                //     // TODO: equals should be specified **for each type**
                //     // ^ because i hate implicit conversions. makes debugging hard
                //     let res_typ = match (lhs, rhs) {
                //         (RegisterType::Number, RegisterType::Number)
                //         | (RegisterType::Number, RegisterType::Int(_))
                //         | (RegisterType::Int(_), RegisterType::Number) => RegisterType::Boolean,
                //         (RegisterType::Int(a), RegisterType::Int(b)) => RegisterType::Bool(a == b),
                //         (RegisterType::Bytes, RegisterType::Bytes)
                //         | (RegisterType::Bytes, RegisterType::Byts(_))
                //         | (RegisterType::Byts(_), RegisterType::Bytes) => RegisterType::Boolean,
                //         (RegisterType::Byts(a), RegisterType::Byts(b)) => RegisterType::Bool(
                //             self.types.unintern_const(a) == self.types.unintern_const(b),
                //         ),
                //         (RegisterType::Trivial(a), RegisterType::Trivial(b)) => {
                //             RegisterType::Bool(a == b)
                //         }
                //         (RegisterType::Boolean, RegisterType::Boolean)
                //         | (RegisterType::Boolean, RegisterType::Bool(_))
                //         | (RegisterType::Bool(_), RegisterType::Boolean) => RegisterType::Boolean,
                //         (RegisterType::Bool(a), RegisterType::Bool(b)) => {
                //             RegisterType::Bool(a == b)
                //         }
                //         // TODO: should we allow equality like this?
                //         (RegisterType::Trivial(_), RegisterType::Record(_))
                //         | (RegisterType::Record(_), RegisterType::Trivial(_)) => {
                //             RegisterType::Bool(false)
                //         }
                //         // TODO: we should definitely *not* allow this, but i am lazy
                //         (RegisterType::Record(_), RegisterType::Byts(_))
                //         | (RegisterType::Byts(_), RegisterType::Record(_)) => {
                //             RegisterType::Bool(false)
                //         }
                //         (a, b) => panic!("cannot equals for {:?} and {:?}", a, b),
                //     };

                //     self.types.assign_type(i.result, res_typ);
                // }
                ir::Instruction::Negate(i) => {
                    let o = self.types.get(i.operand);

                    let res_typ = match o {
                        RegisterType::Boolean => RegisterType::Boolean,
                        RegisterType::Bool(b) => RegisterType::Bool(!b),
                        o => panic!("cannot negate for {:?}", o),
                    };

                    self.types.assign_type(i.result, res_typ);
                }
                // ir::Instruction::Add(i) => {
                //     let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                //     let res_typ = match (lhs, rhs) {
                //         (RegisterType::Number, RegisterType::Number)
                //         | (RegisterType::Number, RegisterType::Int(_))
                //         | (RegisterType::Int(_), RegisterType::Number) => RegisterType::Number,
                //         (RegisterType::Int(a), RegisterType::Int(b)) => RegisterType::Int(a + b),
                //         (a, b) => panic!("cannot add for {:?} and {:?}", a, b),
                //     };

                //     self.types.assign_type(i.result, res_typ);
                // }
                // ir::Instruction::Or(i) => {
                //     let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                //     let res_typ = match (lhs, rhs) {
                //         (RegisterType::Boolean, RegisterType::Boolean)
                //         | (RegisterType::Bool(_), RegisterType::Boolean)
                //         | (RegisterType::Boolean, RegisterType::Bool(_)) => RegisterType::Boolean,
                //         (RegisterType::Bool(a), RegisterType::Bool(b)) => {
                //             RegisterType::Bool(a || b)
                //         }
                //         (a, b) => panic!("cannot OR for {:?} and {:?}", a, b),
                //     };

                //     self.types.assign_type(i.result, res_typ);
                // }
                // ir::Instruction::And(i) => {
                //     let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                //     let res_typ = match (lhs, rhs) {
                //         (RegisterType::Boolean, RegisterType::Boolean)
                //         | (RegisterType::Bool(_), RegisterType::Boolean)
                //         | (RegisterType::Boolean, RegisterType::Bool(_)) => RegisterType::Boolean,
                //         (RegisterType::Bool(a), RegisterType::Bool(b)) => {
                //             RegisterType::Bool(a && b)
                //         }
                //         (a, b) => panic!("cannot AND for {:?} and {:?}", a, b),
                //     };

                //     self.types.assign_type(i.result, res_typ);
                // }
                ir::Instruction::CallVirt(i) => {
                    let fn_id = self.types.get_fnptr(i.calling);

                    // TODO: don't blatantly copy `CallStatic`
                    let src_fn = self.program.functions.get(&fn_id).unwrap();
                    debug_assert_eq!(src_fn.parameters.len(), i.args.len());
                    let map_args = (i.args.iter().copied()).zip(src_fn.parameters.iter().copied());
                    let (types, alloc_map) = self.types.extract_map(map_args);
                    let id = self.fn_ids.id_of(fn_id, types, true);
                    let r = system.spawn(id);

                    let mut rev_alloc_map = alloc_map
                        .iter()
                        .map(|(k, v)| (*v, *k))
                        .collect::<FxHashMap<_, _>>();

                    for (_, its_alloc) in alloc_map.iter() {
                        // update the register type
                        r.types.pull_type_into(
                            RegisterType::Record(*its_alloc),
                            &mut self.types,
                            &rev_alloc_map,
                        );
                    }

                    let return_type = r.clone_ret_typ(&mut self.types, &alloc_map);

                    match (i.result, return_type) {
                        (_, ReturnType::Never) => {
                            self.never_infected = true;
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
                    let src_fn = self.program.functions.get(&i.calling).unwrap();
                    debug_assert_eq!(src_fn.parameters.len(), i.args.len());
                    let map_args = (i.args.iter().copied())
                        .zip(src_fn.parameters.iter().copied())
                        .collect::<Vec<_>>();
                    let (types, alloc_map) = self.types.extract_map(map_args.iter().copied());
                    let id = self.fn_ids.id_of(i.calling, types, true);
                    let r = system.spawn(id);

                    let mut rev_alloc_map = alloc_map
                        .iter()
                        .map(|(k, v)| (*v, *k))
                        .collect::<FxHashMap<_, _>>();

                    for (_, its_alloc) in alloc_map.iter() {
                        // update the register type
                        r.types.pull_type_into(
                            RegisterType::Record(*its_alloc),
                            &mut self.types,
                            &rev_alloc_map,
                        );
                    }

                    let return_type = r.clone_ret_typ(&mut self.types, &alloc_map);

                    match (i.result, return_type) {
                        (_, ReturnType::Never) => {
                            self.never_infected = true;
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
                    let ext_fn = self.program.external_functions.get(&i.calling).unwrap();

                    match (i.result, &ext_fn.returns) {
                        (Some(_), Returns::Void) => panic!("cannot assign `void` to register"),
                        (None, _) => {}
                        (Some(_), Returns::Value(_)) => {
                            todo!("handle return types of ext fns")
                        }
                    };
                }
                ir::Instruction::Generalize(i) => {
                    todo!("generalization algorithm");
                }
            }
        }

        if self.never_infected {
            // TODO: write "unreachable" instruction
            self.return_type = ReturnType::Never;
        }

        self.inst_on = CurrentInstruction::ControlFlow(&self.func.end);
        let rs = if self.never_infected {
            vec![]
        } else {
            match &self.func.end {
                crate::lifted::EndInstruction::Jump(i) => {
                    let src_fn = self.program.functions.get(&i.0 .0).unwrap();
                    debug_assert_eq!(src_fn.parameters.len(), i.0 .1.len());
                    let map_args = (i.0 .1.iter().copied()).zip(src_fn.parameters.iter().copied());
                    let (types, alloc_map) = self.types.extract_map(map_args);
                    let id = self.fn_ids.id_of(i.0 .0, types, false);
                    let r = system.spawn(id);

                    let mut rev_alloc_map = alloc_map
                        .iter()
                        .map(|(k, v)| (*v, *k))
                        .collect::<FxHashMap<_, _>>();

                    for (_, its_alloc) in alloc_map.iter() {
                        // update the register type
                        r.types.pull_type_into(
                            RegisterType::Record(*its_alloc),
                            &mut self.types,
                            &rev_alloc_map,
                        );
                    }

                    self.return_type = r.clone_ret_typ(&mut self.types, &alloc_map);

                    vec![r]
                }
                crate::lifted::EndInstruction::JumpIf(i) => match self.types.get(i.condition) {
                    RegisterType::Bool(true) => {
                        let src_fn = self.program.functions.get(&i.if_so.0).unwrap();
                        debug_assert_eq!(src_fn.parameters.len(), i.if_so.1.len());
                        let map_args =
                            (i.if_so.1.iter().copied()).zip(src_fn.parameters.iter().copied());
                        let (types, alloc_map) = self.types.extract_map(map_args);
                        let id = self.fn_ids.id_of(i.if_so.0, types, false);
                        let r = system.spawn(id);

                        let mut rev_alloc_map = alloc_map
                            .iter()
                            .map(|(k, v)| (*v, *k))
                            .collect::<FxHashMap<_, _>>();

                        for (_, its_alloc) in alloc_map.iter() {
                            // update the register type
                            r.types.pull_type_into(
                                RegisterType::Record(*its_alloc),
                                &mut self.types,
                                &rev_alloc_map,
                            );
                        }

                        self.return_type = r.clone_ret_typ(&mut self.types, &alloc_map);

                        vec![r]
                    }
                    RegisterType::Bool(false) => {
                        let src_fn = self.program.functions.get(&i.other.0).unwrap();
                        debug_assert_eq!(src_fn.parameters.len(), i.other.1.len());
                        let map_args =
                            (i.other.1.iter().copied()).zip(src_fn.parameters.iter().copied());
                        let (types, alloc_map) = self.types.extract_map(map_args);
                        let id = self.fn_ids.id_of(i.other.0, types, false);
                        let r = system.spawn(id);

                        let mut rev_alloc_map = alloc_map
                            .iter()
                            .map(|(k, v)| (*v, *k))
                            .collect::<FxHashMap<_, _>>();

                        for (_, its_alloc) in alloc_map.iter() {
                            // update the register type
                            r.types.pull_type_into(
                                RegisterType::Record(*its_alloc),
                                &mut self.types,
                                &rev_alloc_map,
                            );
                        }

                        self.return_type = r.clone_ret_typ(&mut self.types, &alloc_map);

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
                    vec![]
                }
            }
        };

        self.inst_on = CurrentInstruction::Completed;

        let types = self.types.clone();

        // this is only here temporarily
        assert_ne!(self.return_type, ReturnType::Never, "why is ret typ never");

        WorkerResults {
            is_entry_fn: self.is_entry_fn,
            return_type: self.return_type,
            types,
        }
    }
}
