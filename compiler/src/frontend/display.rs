use crate::frontend::assembler::BlockJump;

use super::{
    assembler::{Function, Program},
    type_annotater::ValueType,
    types::RecordShape,
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
                iw!(text, "%{}: {}, ", arg.register, display_vt(&arg.typ, &f));
            }
            iw!(text, "):\n");

            for inst in block.instructions.iter() {
                match inst {
                    crate::frontend::assembler::Instruction::RecordNew(r) => {
                        iwl!(
                            text,
                            "    %{}: {} = RecordNew({})",
                            *r,
                            display_vt(f.register_types.get(*r), f),
                            match f.register_types.get(*r) {
                                ValueType::Record(a) => a,
                                _ => unreachable!(),
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
                            display_vt(f.register_types.get(*result), f),
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
                        let rec_shp = f.register_types.get_shape_by_id(shape);
                        iwl!(
                            text,
                            "    RecordSet (%{}: {}), {}, %{}",
                            record,
                            display_rs(f.register_types.get_shape_by_id(shape), f),
                            key,
                            value,
                        )
                    }
                    crate::frontend::assembler::Instruction::Call(_, _, _) => todo!(),
                    crate::frontend::assembler::Instruction::GetRuntime(_) => todo!(),
                    crate::frontend::assembler::Instruction::MakeFnPtr(_, _) => todo!(),
                    crate::frontend::assembler::Instruction::MakeString(r, s) => {
                        iwl!(
                            text,
                            "    %{}: {} = MakeString {}",
                            r,
                            display_vt(f.register_types.get(*r), f),
                            display_str(program.constants.get(s).unwrap())
                        );
                    }
                    crate::frontend::assembler::Instruction::MakeNumber(r, i) => {
                        iwl!(
                            text,
                            "    %{}: {} = MakeNumber {}",
                            r,
                            display_vt(f.register_types.get(*r), f),
                            i
                        );
                    }
                    crate::frontend::assembler::Instruction::MakeBoolean(r, v) => iwl!(
                        text,
                        "    %{}: {} = MakeBoolean {}",
                        r,
                        display_vt(f.register_types.get(*r), f),
                        v
                    ),
                    crate::frontend::assembler::Instruction::MakeNull(r) => iwl!(
                        text,
                        "    %{}: {} = MakeNull",
                        r,
                        display_vt(f.register_types.get(*r), f)
                    ),
                    crate::frontend::assembler::Instruction::MakeUndefined(r) => iwl!(
                        text,
                        "    %{}: {} = MakeUndefined",
                        r,
                        display_vt(f.register_types.get(*r), f)
                    ),
                    crate::frontend::assembler::Instruction::Widen {
                        result,
                        input,
                        from,
                        to,
                    } => todo!(),
                    crate::frontend::assembler::Instruction::Unreachable => {
                        iwl!(text, "    Unreachable")
                    }
                    crate::frontend::assembler::Instruction::Noop => iwl!(text, "    Noop"),
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

fn display_vt(t: &ValueType, f: &Function) -> String {
    match t {
        ValueType::Bool(b) => format!("{}", b),
        ValueType::ExactInteger(i) => format!("{}", i),
        ValueType::ExactString(payload) => display_str(payload),
        ValueType::Record(a) => {
            if let Some(shape) = f.register_types.try_get_shape(*a) {
                display_rs(shape, f)
            } else {
                format!("?(deleted)?")
            }
        }
        ValueType::FnPtr(_) => "todo::FnPtr".into(),
        _ => format!("{:?}", t),
    }
}

fn display_rs(shape: &RecordShape, f: &Function) -> String {
    let mut s = String::new();
    iw!(s, "{{ ");
    for (k, v) in shape.fields() {
        match k {
            crate::frontend::types::ShapeKey::String => iw!(s, "*: "),
            crate::frontend::types::ShapeKey::Str(cs) => iw!(s, "{}: ", display_str(cs)),
            crate::frontend::types::ShapeKey::InternalSlot(str) => iw!(s, "[[{}]]: ", str),
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
