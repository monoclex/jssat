use crate::frontend::ir::{ControlFlowInstruction, Instruction};

use super::ir::{FFIValueType, IR};
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

pub fn display(program: &IR) -> String {
    let mut text = String::new();

    for (id, ext_fn) in program.external_functions.iter() {
        iw!(text, "ext fn @@{}(", id);

        for arg in ext_fn.parameters.iter() {
            iw!(text, "{}, ", display_norecord(arg));
        }

        iw!(text, ");\n\n");
    }

    for (id, f) in program.functions.iter() {
        iw!(text, "fn @{}(", id);

        for p in f.parameters.iter() {
            iw!(text, "%{}, ", p.register);
        }

        iw!(text, ") {{\n");

        let blocks = std::iter::once((&f.entry_block, f.blocks.get(&f.entry_block).unwrap()))
            .chain(f.blocks.iter().filter(|(id, _)| **id != f.entry_block));

        for (id, block) in blocks {
            iw!(text, "  ${}(", id);
            for arg in block.parameters.iter() {
                iw!(text, "%{}, ", arg);
            }
            iw!(text, "):\n");

            for inst in block.instructions.iter() {
                match inst {
                    Instruction::Comment(s, l) => iwl!(text, "    -- {}, {}", s, l),
                    Instruction::RecordNew(r) => {
                        iwl!(text, "    %{} = RecordNew", r.result,)
                    }
                    Instruction::RecordGet(t) => iwl!(text, "    RecordGet {:?}", t),
                    Instruction::RecordSet(t) => iwl!(text, "    RecordSet {:?}", t),
                    Instruction::CallVirt(t) => iwl!(text, "    CallVirt {:?}", t),
                    Instruction::CallExtern(t) => iwl!(text, "    CallExtern {:?}", t),
                    Instruction::CallStatic(t) => iwl!(text, "    CallStatic {:?}", t),
                    Instruction::MakeTrivial(t) => iwl!(text, "    MakeTrivial {:?}", t),
                    Instruction::MakeString(instt) => iwl!(text, "    MakeString {:?}", instt),
                    Instruction::ReferenceOfFunction(r, f) => {
                        iwl!(text, "    %{} = MakeFnPtr @{}", r, f)
                    }
                    Instruction::CompareLessThan(inst) => {
                        let r = inst.result;
                        let l = inst.lhs;
                        let rr = inst.rhs;
                        iwl!(text, "    %{} = CompareLessThan %{}, %{}", r, l, rr)
                    }
                    Instruction::MakeInteger(t) => iwl!(text, "    MakeInteger {:?}", t),
                    Instruction::CompareEqual(t) => iwl!(text, "    CompareEqual {:?}", t),
                    Instruction::Negate(t) => iwl!(text, "    Negate {:?}", t),
                    Instruction::Add(t) => iwl!(text, "    Add {:?}", t),
                }
            }

            match &block.end {
                ControlFlowInstruction::Jmp(inst) => iwl!(text, "    {:?}", inst),
                ControlFlowInstruction::JmpIf(inst) => iwl!(text, "    {:?}", inst),
                ControlFlowInstruction::Ret(r) => {
                    iwl!(text, "    Ret {:?}", r)
                }
            };
        }

        iw!(text, "}}\n\n");
    }

    text
}

fn display_norecord(t: &FFIValueType) -> String {
    match t {
        // ValueType::Bool(b) => format!("{}", b),
        // ValueType::ExactInteger(i) => format!("{}", i),
        // ValueType::ExactString(payload) => display_str(payload),
        // ValueType::Record(_) => unreachable!(),
        // ValueType::FnPtr(_) => "todo::FnPtr".into(),
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
