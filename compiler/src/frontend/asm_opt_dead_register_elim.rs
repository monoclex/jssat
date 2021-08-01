use rustc_hash::FxHashSet;

use super::{
    assembler::{Function, Program},
    type_annotater::ValueType,
};
use crate::frontend::{assembler::Instruction, old_types::ShapeKey};
use crate::isa::RecordKey;

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

    for (_, b) in f.blocks.iter_mut() {
        for inst in b.instructions.iter_mut() {
            if let Some(declared) = inst.assigned_to() {
                // only opt if this inst is pure
                if declared_but_not_used.contains(&declared) {
                    // this is a useless instruction, delete it

                    match inst {
                        // fix the object type
                        Instruction::RecordSet { shape_id, key, .. } => {
                            let k = match key {
                                RecordKey::Prop(v) => match b.register_types.get(*v) {
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
                                RecordKey::Slot(s) => ShapeKey::InternalSlot(*s),
                            };
                            let shape = b.register_types.get_shape_by_id_mut(shape_id);
                            shape.remove_prop(&k);
                        }
                        // if we're completely deleting an allocation,
                        // remove its existence from the list of allocations while we're at it
                        Instruction::RecordNew(r) => {
                            if let ValueType::Record(alloc) = *b.register_types.get(*r) {
                                b.register_types.remove_alloc(alloc);
                                *inst = Instruction::Noop;
                            }
                        }
                        // TODO: this should really be in asm_opt_const_elimination
                        Instruction::Call(result, _, _) => {
                            // if we have a call inst, we can only optimize it by removing the value
                            // achieved from the call (if that value is constant)
                            if let Some(r) = result {
                                assert!(b.register_types.is_const(*r));
                                *result = None;
                            }
                        }
                        _ => {
                            assert!(inst.is_pure());
                            *inst = Instruction::Noop;
                        }
                    };
                }
            }
        }
    }
}
