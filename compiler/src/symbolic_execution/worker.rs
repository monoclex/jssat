//! The file where the actual symbolic execution work gets done

use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::frontend::ir;
use crate::frontend::ir::Returns;
use crate::id::*;
use crate::isa::Make;
use crate::lifted;
use crate::lifted::{Function, LiftedProgram};
use crate::retag::ExtFnPassRetagger;
use crate::symbolic_execution::types::{RegisterType, ReturnType};

use super::graph_system::Computation;
use super::types::InstIdx;
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

#[derive(Clone)]
pub struct WorkerResults {
    pub is_entry_fn: bool,
    pub return_type: ReturnType,
    pub types: TypeBag,
}

impl<'p> Worker for SymbWorker<'p> {
    type Id = FunctionId<SymbolicCtx>;

    // TODO: the result of a worker should be a return type like Never
    // or something idk. maybe it's fine to leave it as self and hope that
    // callers only use the return type and not any other values
    type Result = WorkerResults;

    fn work(&mut self, system: &impl System<Self>) -> Computation<Self::Result> {
        for (inst_idx, inst) in self.func.instructions.iter().enumerate() {
            self.inst_on = CurrentInstruction::Sequential(inst);
            self.exec_inst(inst, InstIdx::Inst(inst_idx), system);

            if self.never_infected {
                break;
            }
        }

        self.inst_on = CurrentInstruction::ControlFlow(&self.func.end);

        let return_type = if self.never_infected {
            // TODO: write "unreachable" instruction
            ReturnType::Never
        } else {
            let inst_idx = InstIdx::Epilogue;
            match &self.func.end {
                crate::lifted::EndInstruction::Jump(i) => {
                    self.exec_types(system, i.0 .0, &i.0 .1, inst_idx)
                }
                crate::lifted::EndInstruction::JumpIf(i) => match self.types.get(i.condition) {
                    RegisterType::Bool(true) => {
                        self.exec_types(system, i.if_so.0, &i.if_so.1, inst_idx)
                    }
                    RegisterType::Bool(false) => {
                        self.exec_types(system, i.other.0, &i.other.1, inst_idx)
                    }
                    RegisterType::Boolean => todo!("cannot handle divergence atm"),
                    r => unimplemented!("cannot use non-boolean register as conditional {:?}", r),
                },
                crate::lifted::EndInstruction::Return(i) => match i.0 {
                    Some(r) => ReturnType::Value(self.types.get(r)),
                    None => ReturnType::Void,
                },
            }
        };

        self.inst_on = CurrentInstruction::Completed;
        self.return_type = return_type;

        // this is only here temporarily
        assert_ne!(return_type, ReturnType::Never, "why is ret typ never");

        Computation::Result(WorkerResults {
            is_entry_fn: self.is_entry_fn,
            return_type,
            types: self.types.clone(),
        })
    }
}

impl SymbWorker<'_> {
    fn exec_inst(
        &mut self,
        inst: &ir::Instruction<LiftedCtx, LiftedCtx>,
        inst_idx: InstIdx,
        system: &impl System<Self>,
    ) {
        match inst {
            ir::Instruction::Comment(_) => {}
            ir::Instruction::NewRecord(i) => self.types.new_record(i.result),
            ir::Instruction::RecordGet(i) => {
                let field_typ = self.types.record_get_field(i.record, i.key);
                self.types.assign_type(i.result, field_typ);
            }
            ir::Instruction::RecordSet(i) => {
                let field_typ = self.types.get(i.value);
                self.types
                    .record_set_field(i.record, i.key, Some(field_typ), inst_idx);
            }
            ir::Instruction::RecordHasKey(i) => {
                match self.types.get(i.record) {
                    RegisterType::Record(_) => {
                        let has_field = match self.types.record_has_field(i.record, i.key) {
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
            &ir::Instruction::GetFnPtr(i) => self.make(i, RegisterType::FnPtr),
            &ir::Instruction::MakeTrivial(i) => self.make(i, RegisterType::Trivial),
            &ir::Instruction::MakeInteger(i) => self.make(i, RegisterType::Int),
            &ir::Instruction::MakeBoolean(i) => self.make(i, RegisterType::Bool),
            ir::Instruction::MakeBytes(i) => {
                let c = self.program.constants.get(&i.item).unwrap();
                let c = self.types.intern_constant(&c.payload);
                self.types.assign_type(i.result, RegisterType::Byts(c));
            }
            ir::Instruction::BinOp(i) => {
                let (lhs, rhs) = (self.types.get(i.lhs), self.types.get(i.rhs));

                let res_typ =
                    i.op.make_executor(&mut self.types)
                        .execute(lhs, rhs)
                        .expect("expected binary operator to perform");

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
            ir::Instruction::CallVirt(i) => {
                let fn_id = self.types.get_fnptr(i.calling);
                self.call_fn(system, i.result, fn_id, &i.args, inst_idx);
            }
            ir::Instruction::CallStatic(i) => {
                self.call_fn(system, i.result, i.calling, &i.args, inst_idx)
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
            ir::Instruction::Generalize(_) => {
                todo!("generalization algorithm");
            }
        };
    }

    fn make<I: Copy>(&mut self, make: Make<LiftedCtx, I>, constructor: impl Fn(I) -> RegisterType) {
        self.types.assign_type(make.result, constructor(make.item))
    }

    fn call_fn(
        &mut self,
        system: &impl System<Self>,
        result: Option<RegisterId<LiftedCtx>>,
        fn_id: FunctionId<LiftedCtx>,
        args: &[RegisterId<LiftedCtx>],
        inst_idx: InstIdx,
    ) {
        let return_type = self.exec_types(system, fn_id, args, inst_idx);

        match (result, return_type) {
            (_, ReturnType::Never) => {
                self.never_infected = true;
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
}

impl SymbWorker<'_> {
    fn exec_types(
        &mut self,
        system: &impl System<Self>,
        fn_id: FunctionId<LiftedCtx>,
        fn_args: &[RegisterId<LiftedCtx>],
        inst_idx: InstIdx,
    ) -> ReturnType {
        let target_fn = self.program.functions.get(&fn_id).unwrap();
        debug_assert_eq!(
            target_fn.parameters.len(),
            fn_args.len(),
            "param count should match"
        );

        let mut subset = self.types.subset(fn_args, &target_fn.parameters, inst_idx);

        let target_id = self.fn_ids.id_of(fn_id, subset.child(), false);
        let results = system.spawn(target_id);

        target_fn
            .parameters
            .iter()
            .for_each(|reg| subset.update_reg(&results.types, *reg, inst_idx));

        results
            .return_type
            .map(|v| subset.update_typ(&results.types, v, inst_idx))
    }
}
