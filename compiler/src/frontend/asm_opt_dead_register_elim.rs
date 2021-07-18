use rustc_hash::FxHashSet;

use super::{
    assembler::{Function, Program},
    type_annotater::ValueType,
};
use crate::frontend::{assembler::Instruction, old_types::ShapeKey};

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

        used.extend(b.end.used_registers());
    }

    let declared_but_not_used = declared.difference(&used).collect::<FxHashSet<_>>();

    for inst in f
        .blocks
        .iter_mut()
        .flat_map(|(_, b)| b.instructions.iter_mut())
    {
        if let Some(declared) = inst.assigned_to() {
            if declared_but_not_used.contains(&declared) {
                // this is a useless instruction, delete it

                match inst {
                    // fix the object type
                    Instruction::RecordSet { shape_id, key, .. } => {
                        let k = match key {
                            super::ir::RecordKey::Value(v) => match f.register_types.get(*v) {
                                ValueType::ExactString(s) => ShapeKey::Str(s.clone()),
                                ValueType::Any
                                | ValueType::Runtime
                                | ValueType::String
                                | ValueType::Number
                                | ValueType::ExactInteger(_)
                                | ValueType::Boolean
                                | ValueType::Bool(_)
                                | ValueType::Record(_)
                                | ValueType::FnPtr(_)
                                | ValueType::Null
                                | ValueType::Undefined => todo!(),
                            },
                            super::ir::RecordKey::InternalSlot(s) => ShapeKey::InternalSlot(s),
                        };
                        let shape = f.register_types.get_shape_by_id_mut(shape_id);
                        shape.remove_prop(&k);
                    }
                    // if we're completely deleting an allocation,
                    // remove its existence from the list of allocations while we're at it
                    Instruction::RecordNew(r) => {
                        if let ValueType::Record(alloc) = *f.register_types.get(*r) {
                            f.register_types.remove_alloc(alloc);
                            *inst = Instruction::Noop;
                        }
                    }
                    _ => *inst = Instruction::Noop,
                };
            }
        }
    }
}
