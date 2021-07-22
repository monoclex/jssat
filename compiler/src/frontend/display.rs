use crate::frontend::assembler::BlockJump;

use super::{
    assembler::{Block, Program},
    old_types::RecordShape,
    type_annotater::ValueType,
};
use std::fmt::Write;

/// Infallible write
macro_rules! iw {
    ($($e:tt)+) => {
        write!($($e)+).unwrap()
    };
}
/// Infallible writeln
macro_rules! iwl {
    ($($e:tt)+) => {
        writeln!($($e)+).unwrap()
    };
}

pub fn display(program: &Program) -> String {
    let mut text = String::new();

    for (id, ext_fn) in program.external_functions.iter() {
        iw!(text, "ext fn @@{}(", id);

        for arg in ext_fn.parameters.iter() {
            iw!(text, "{}, ", display_norecord(arg));
        }

        iw!(text, ");\n\n");
    }

    for (id, f) in program.functions.iter() {
        iw!(text, "fn @{}() {{\n", id);

        let blocks = std::iter::once((&f.entry_block, f.entry_block()))
            .chain(f.blocks.iter().filter(|(id, _)| **id != f.entry_block));

        for (id, block) in blocks {
            iw!(text, "  ${}(", id);
            for arg in block.parameters.iter() {
                iw!(
                    text,
                    "%{}: {}, ",
                    arg.register,
                    display_vt(&arg.typ, &block)
                );
            }
            iw!(text, "):\n");

            let reg_typs = &block.register_types;

            for inst in block.instructions.iter() {
                match inst {
                    crate::frontend::assembler::Instruction::RecordNew(r) => {
                        iwl!(
                            text,
                            "    %{}: {} = RecordNew({})",
                            *r,
                            display_vt(reg_typs.get(*r), block),
                            match reg_typs.get(*r) {
                                ValueType::Record(a) => format!("{}", a),
                                x => format!(
                                    "????????????????????????{:?}????????????????????????",
                                    x
                                ),
                            }
                        )
                    }
                    crate::frontend::assembler::Instruction::RecordGet {
                        result,
                        record,
                        key,
                    } => {
                        iwl!(
                            text,
                            "    %{}: {} = RecordGet %{}, {}",
                            result,
                            display_vt(reg_typs.get(*result), block),
                            record,
                            key,
                        )
                    }
                    crate::frontend::assembler::Instruction::RecordSet {
                        shape_id: shape,
                        record,
                        key,
                        value,
                    } => {
                        iwl!(
                            text,
                            "    RecordSet (%{}: {}), {}, %{}",
                            record,
                            display_rs(reg_typs.get_shape_by_id(shape), block),
                            key,
                            value,
                        )
                    }
                    crate::frontend::assembler::Instruction::Call(a, x, y) => {
                        iwl!(text, "    todo {:?} {:?} {:?}", a, x, y);
                    }
                    crate::frontend::assembler::Instruction::MakeFnPtr(x, y) => {
                        iwl!(text, "    todo {:?} {:?}", x, y)
                    }
                    crate::frontend::assembler::Instruction::MakeTrivial(a) => {
                        iwl!(text, "    todo {:?}", a)
                    }
                    crate::frontend::assembler::Instruction::MakeString(r, s) => {
                        iwl!(
                            text,
                            "    %{}: {} = MakeString {}",
                            r,
                            display_vt(reg_typs.get(*r), block),
                            display_str(program.constants.get(s).unwrap())
                        );
                    }
                    crate::frontend::assembler::Instruction::MakeNumber(r, i) => {
                        iwl!(
                            text,
                            "    %{}: {} = MakeNumber {}",
                            r,
                            display_vt(reg_typs.get(*r), block),
                            i
                        );
                    }
                    crate::frontend::assembler::Instruction::MakeBoolean(r, v) => iwl!(
                        text,
                        "    %{}: {} = MakeBoolean {}",
                        r,
                        display_vt(reg_typs.get(*r), block),
                        v
                    ),
                    crate::frontend::assembler::Instruction::Widen { .. } => {
                        iwl!(text, "todo wieden")
                    }
                    crate::frontend::assembler::Instruction::Unreachable => {
                        iwl!(text, "    Unreachable");
                    }
                    crate::frontend::assembler::Instruction::Noop => {
                        // don't write anything, it's annoying
                        // iwl!(text, "    Noop")
                    }
                    crate::frontend::assembler::Instruction::OpLessThan(_) => {
                        iwl!(text, "    op less than TODO")
                    }
                }
            }

            match &block.end {
                crate::frontend::assembler::EndInstruction::Unreachable => {
                    iwl!(text, "    Unreachable")
                }
                crate::frontend::assembler::EndInstruction::Jump(BlockJump(id, args)) => {
                    iwl!(text, "    Jump ${}({:?})", id, args)
                }
                crate::frontend::assembler::EndInstruction::JumpIf {
                    condition,
                    true_path,
                    false_path,
                } => iwl!(
                    text,
                    "    if %{}:\n        Jump ${}({:?})\n    else\n        Jump ${}({:?})",
                    condition,
                    true_path.0,
                    true_path.1,
                    false_path.0,
                    false_path.1,
                ),
                crate::frontend::assembler::EndInstruction::Return(r) => {
                    iwl!(text, "    Ret {:?}", r)
                }
            };
        }

        iw!(text, "}}\n\n");
    }

    text
}

fn display_vt(t: &ValueType, f: &Block) -> String {
    match t {
        ValueType::Bool(b) => format!("{}", b),
        ValueType::ExactInteger(i) => format!("{}", i),
        ValueType::ExactString(payload) => display_str(payload),
        ValueType::Record(a) => {
            if let Some(shape) = f.register_types.try_get_shape(*a) {
                display_rs(shape, f)
            } else {
                "?(deleted)?".to_string()
            }
        }
        ValueType::FnPtr(_) => "todo::FnPtr".into(),
        _ => format!("{:?}", t),
    }
}

fn display_rs(shape: &RecordShape, f: &Block) -> String {
    let mut s = String::new();
    iw!(s, "{{ ");
    for (k, v) in shape.fields() {
        match k {
            crate::frontend::old_types::ShapeKey::String => iw!(s, "*: "),
            crate::frontend::old_types::ShapeKey::Str(cs) => iw!(s, "{}: ", display_str(cs)),
            crate::frontend::old_types::ShapeKey::InternalSlot(str) => iw!(s, "[[{}]]: ", str),
        };

        iw!(s, "{}, ", display_vt(v, f));
    }

    iw!(s, "}}");
    s
}

fn display_norecord(t: &ValueType) -> String {
    match t {
        ValueType::Bool(b) => format!("{}", b),
        ValueType::ExactInteger(i) => format!("{}", i),
        ValueType::ExactString(payload) => display_str(payload),
        ValueType::Record(_) => unreachable!(),
        ValueType::FnPtr(_) => "todo::FnPtr".into(),
        _ => format!("{:?}", t),
    }
}

fn display_str(payload: &[u8]) -> String {
    if let Ok(s) = String::from_utf8(payload.to_vec()) {
        return format!("{:?}", s);
    };

    let (pre, bytes, post) = unsafe { payload.align_to() };

    if !pre.is_empty() && !post.is_empty() {
        return format!("{:?}", payload);
    }

    if let Ok(s) = String::from_utf16(bytes) {
        return s;
    };

    return format!("{:?}", payload);
}
