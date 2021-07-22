use rustc_hash::FxHashMap;

use super::{
    assembler::{Block, Function, Program},
    old_types::{RegMap, ShapeKey},
    type_annotater::ValueType,
};
use crate::{
    frontend::assembler::{BlockJump, Callable, EndInstruction, Instruction},
    id::*,
    isa::{MakeTrivial, RecordKey},
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
        opt_blk(cnsts, b);
    }
}

fn opt_blk(cnsts: &Cnsts, b: &mut Block) {
    let regs = &mut b.register_types;
    // remove const params
    let const_params = b
        .parameters
        .iter()
        .filter(|p| regs.is_const(p.register))
        .collect::<Vec<_>>();

    let mut prepend = Vec::with_capacity(const_params.len());
    for p in const_params {
        let r = p.register;
        stack_alloc_valule(&p.typ, &mut prepend, r, cnsts, regs);
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
        let i2 = i.clone();
        match i {
            Instruction::Call(_, Callable::Static(_), args) => {
                let mut new_args = vec![];
                for arg in args.iter_mut() {
                    if !regs.is_const(*arg) {
                        new_args.push(*arg);
                    }
                }
                *args = new_args;
            }
            // TODO: is this the right place to put this opt?
            Instruction::RecordSet {
                shape_id,
                record,
                key,
                value,
            } => {
                // if we're setting a constant value
                // into a shape with a slot for only 1 constant value
                // we can just noop this

                if let ValueType::Record(alloc) = *regs.get(*record) {
                    let total_shape = regs.get_shape_by_id(shape_id);
                    let key = match key {
                        RecordKey::Prop(v) => match regs.get(*v) {
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
                    println!("key: {:?}", key);
                    println!("shape: {:?}", total_shape);
                    println!("final shape: {:?}", regs.get_shape(alloc));
                    println!("inst: {:?}", i2);
                    let k = total_shape.type_at_key(&key);

                    if regs.is_const_typ(k) && regs.is_const(*value) {
                        // TODO: is this sound?
                        // because we have a list of shapes
                        // [A -> B -> C]
                        // each shape only has *one* change from the previous shape
                        // [{} -> { a: 1 } -> { a: 1, b: 2 }]
                        // thus, wiping a shape will remove that transition
                        // [{} -> {} -> { a: 1, b: 2 }]
                        // which will let our lack of uninfication work
                        // no idea if this is sound tho

                        // we don't want to remove the last shape in the transition tree, because the
                        // last transition gives this record its final shape
                        let last_shape = regs.get_shapes(alloc).last().copied();
                        let my_shape = Some(*shape_id);
                        let is_last_shape = last_shape == my_shape;
                        println!("-----------> {:?} == {:?}", last_shape, my_shape);
                        if !is_last_shape {
                            regs.get_shape_by_id_mut(shape_id).wipe();
                        }
                        // this should be fine
                        *i = Instruction::Noop;
                    }
                } else {
                    unreachable!("how?????");
                }
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

fn stack_alloc_valule(
    p: &ValueType,
    prepend: &mut Vec<Instruction>,
    r: RegisterId<AssemblerCtx>,
    cnsts: &Cnsts,
    regs: &mut RegMap<AssemblerCtx>,
) {
    match p {
        &ValueType::FnPtr(func_id) => {
            prepend.push(Instruction::MakeFnPtr(r, func_id));
        }
        ValueType::ExactString(payload) => {
            prepend.push(Instruction::MakeString(r, cnsts.intern(&payload)));
        }
        &ValueType::ExactInteger(i) => prepend.push(Instruction::MakeNumber(r, i)),
        &ValueType::Bool(b) => prepend.push(Instruction::MakeBoolean(r, b)),
        &ValueType::Record(alloc) => {
            prepend.push(Instruction::RecordNew(r));
            regs.registers.remove(&r);
            regs.insert(r, ValueType::Record(alloc));

            let shape = regs.get_shape(alloc).clone();
            for (k, v) in shape.fields() {
                let key = match k {
                    crate::frontend::old_types::ShapeKey::String => unreachable!("non const key"),
                    crate::frontend::old_types::ShapeKey::Str(key) => {
                        let key_id = regs.gen_id();
                        prepend.push(Instruction::MakeString(key_id, cnsts.intern(key)));
                        regs.insert(key_id, ValueType::ExactString(key.clone()));
                        RecordKey::Prop(key_id)
                    }
                    crate::frontend::old_types::ShapeKey::InternalSlot(slot) => {
                        RecordKey::Slot(*slot)
                    }
                };

                let value = regs.gen_id();
                stack_alloc_valule(v, prepend, value, cnsts, regs);

                let id = regs.get_shape_id_of_alloc(alloc);
                prepend.push(Instruction::RecordSet {
                    shape_id: *id,
                    record: r,
                    key,
                    value,
                });
            }
        }
        ValueType::Null => {
            prepend.push(Instruction::MakeTrivial(MakeTrivial {
                result: r,
                item: crate::isa::TrivialItem::Null,
            }));
        }
        ValueType::Undefined => {
            prepend.push(Instruction::MakeTrivial(MakeTrivial {
                result: r,
                item: crate::isa::TrivialItem::Undefined,
            }));
        }
        ValueType::Any
        | ValueType::Runtime
        | ValueType::String
        | ValueType::Number
        | ValueType::Boolean => unreachable!("not simple type"),
    };
    regs.registers.remove(&r);
    regs.insert(r, p.clone());
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
