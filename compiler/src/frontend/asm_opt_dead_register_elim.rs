use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    assembler::{Block, Function, Program},
    type_annotater::ValueType,
    types::RegMap,
};
use crate::{
    frontend::assembler::{BlockJump, Callable, EndInstruction, Instruction},
    id::*,
};

/// This optimization pass is necessary to eliminate instructions that generate LLVM IR
/// that uses runtime machinery - meaning LLVM cannot optimize it away.
pub fn opt_dead_register_elimination(program: Program) -> Program {
    let Program {
        entrypoint,
        constants,
        external_functions,
        mut functions,
    } = program;

    for (_, f) in functions.iter_mut() {
        opt_fn(f);
    }

    Program {
        entrypoint,
        constants,
        external_functions,
        functions,
    }
}

fn opt_fn(f: &mut Function) {
    let mut declared = FxHashSet::default();
    let mut used = FxHashSet::default();

    for (_, b) in f.blocks.iter() {
        for inst in b.instructions.iter() {
            if let Some(declared_reg) = inst.assigned_to() {
                declared.insert(declared_reg);
            }

            used.extend(inst.used_registers());
        }

        match &b.end {
            EndInstruction::Unreachable => {}
            EndInstruction::Jump(BlockJump(_, args)) => {
                used.extend(args);
            }
            EndInstruction::JumpIf {
                condition,
                true_path: BlockJump(_, true_args),
                false_path: BlockJump(_, false_args),
            } => {
                used.insert(*condition);
                used.extend(true_args);
                used.extend(false_args);
            }
            &EndInstruction::Return(Some(r)) => {
                used.insert(r);
            }
            EndInstruction::Return(None) => {}
        }
    }

    let declared_but_not_used = declared.difference(&used).collect::<FxHashSet<_>>();

    for inst in f
        .blocks
        .iter_mut()
        .flat_map(|(_, b)| b.instructions.iter_mut())
    {
        if let Some(declared) = inst.assigned_to() {
            if declared_but_not_used.contains(&declared) {
                // useless instruction
                *inst = Instruction::Noop;
            }
        }
    }
}
