use rustc_hash::FxHashMap;

use super::{
    assembler::{Block, Function, Program},
    type_annotater::ValueType,
    types::RegMap,
};
use crate::{
    frontend::assembler::{BlockJump, Callable, EndInstruction, Instruction},
    id::*,
};

pub fn opt_constant_elimination(program: Program) -> Program {
    let Program {
        entrypoint,
        constants,
        external_functions,
        mut functions,
    } = program;

    let cnsts = Cnsts {
        constants: &constants,
    };

    for (_, f) in functions.iter_mut() {
        opt_fn(f, &cnsts);
    }

    Program {
        entrypoint,
        constants,
        external_functions,
        functions,
    }
}

fn opt_fn(f: &mut Function, cnsts: &Cnsts) {
    for (_, b) in f.blocks.iter_mut() {
        opt_blk(&f.register_types, cnsts, b);
    }
}

fn opt_blk(regs: &RegMap<AssemblerCtx>, cnsts: &Cnsts, b: &mut Block) {
    // remove const params
    let const_params = b
        .parameters
        .iter()
        .filter(|p| regs.is_const(p.register))
        .collect::<Vec<_>>();

    let mut prepend = Vec::with_capacity(const_params.len());
    for p in const_params {
        let r = p.register;
        match &p.typ {
            ValueType::ExactString(payload) => {
                prepend.push(Instruction::MakeString(r, cnsts.intern(&payload)))
            }
            &ValueType::ExactInteger(i) => prepend.push(Instruction::MakeNumber(r, i)),
            &ValueType::Bool(b) => prepend.push(Instruction::MakeBoolean(r, b)),
            &ValueType::Record(alloc) => {
                todo!("cannot create records on the stack yet")
            }
            ValueType::Any
            | ValueType::Runtime
            | ValueType::String
            | ValueType::Number
            | ValueType::Boolean
            | ValueType::Pointer(_)
            | ValueType::Word => unreachable!("not simple type"),
        }
    }
    b.instructions.splice(0..0, prepend);

    // now use only the non constant parameters
    b.parameters = b
        .parameters
        .iter()
        .filter(|p| !regs.is_const(p.register))
        .cloned()
        .collect();

    let filter_args = |args: Vec<RegisterId<AssemblerCtx>>| {
        // when we filter args, we're looking to remove constant args
        args.into_iter().filter(|r| !regs.is_const(*r)).collect()
    };

    // remove const params when jumping to other blocks
    b.end = match b.end.clone() {
        EndInstruction::Unreachable => EndInstruction::Unreachable,
        EndInstruction::Jump(BlockJump(id, args)) => {
            EndInstruction::Jump(BlockJump(id, filter_args(args)))
        }
        EndInstruction::JumpIf {
            condition,
            true_path: BlockJump(t_id, t_args),
            false_path: BlockJump(f_id, f_args),
        } => EndInstruction::JumpIf {
            condition,
            true_path: BlockJump(t_id, filter_args(t_args)),
            false_path: BlockJump(f_id, filter_args(f_args)),
        },
        EndInstruction::Return(r) => EndInstruction::Return(r),
    };

    // if we call any functions with const params, we want to remove those too
    // because when we modify the params of all blocks, we modify the params of the entry block,
    // and the params of the entry block are the params of the function
    for i in b.instructions.iter_mut() {
        match i {
            Instruction::Call(_, Callable::Static(_), args) => {
                let mut new_args = vec![];
                for arg in args.iter_mut() {
                    if !regs.is_simple(*arg) {
                        new_args.push(*arg);
                    }
                }
                *args = new_args;
            }
            _ => continue,
        }
    }

    // let reg_typ = |r| reg_tps.get(&r).unwrap();
    // let mut to_be_removed = vec![];
    // // let mut to_be_kept = vec![];

    // for i in b.instructions.iter_mut() {
    //     // we want to remove all registers with constant types
    //     if let Some(reg) = i.assigned_to() {
    //         if reg_typ(reg).is_const() {
    //             to_be_removed.push(reg);
    //         }
    //     }
    // }
}

struct Cnsts<'constants> {
    constants: &'constants FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>,
}

impl Cnsts<'_> {
    pub fn intern(&self, payload: &[u8]) -> ConstantId<AssemblerCtx> {
        for (k, v) in self.constants {
            if v == payload {
                return *k;
            }
        }
        panic!("cannot actually intern constnat yet");
    }
}