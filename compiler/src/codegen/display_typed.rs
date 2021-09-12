use crate::{
    codegen::EndInstruction,
    frontend::ir::{ControlFlowInstruction, FFIValueType},
    id::AssemblerCtx,
    isa::{ISAInstruction, Unreachable},
    symbolic_execution::types::InstIdx,
};

use std::fmt::Write;

use super::TypedProgram;

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

pub fn display(program: &TypedProgram) -> String {
    let mut text = String::new();

    for (id, ext_fn) in program.external_functions.iter() {
        iw!(text, "ext fn @@{}(", id);

        for arg in ext_fn.parameters.iter() {
            iw!(text, "{}, ", display_norecord(arg));
        }

        iw!(text, ");\n\n");
    }

    let mut fns = program.functions.iter().collect::<Vec<_>>();
    fns.sort_by(|(a, _), (b, _)| a.cmp(b));

    for (fn_id, f) in fns {
        iw!(text, "fn @{}(", fn_id);

        for p in f.parameters.iter() {
            iw!(text, "%{}, ", p);
        }

        iw!(text, ") {{\n");

        let block = f;

        for (idx, inst) in block.instructions.iter().enumerate() {
            let inst_idx = InstIdx::Inst(idx);
            iw!(text, "    ");
            inst.display(&mut text).unwrap();
            text.push('\n');

            // TODO: instead of `let r = r.map_context();`
            // we should produce a `TypeBag` with an `AssemblerCtx`
            // but that requires major amounts of rewriting so we ignore that for now :)
            if let Some(r) = inst.declared_register() {
                let r = r.map_context();
                iwl!(text, "        -> {}", block.type_info.display(r, inst_idx));
            }

            for r in inst.used_registers() {
                let r = r.map_context();
                iwl!(text, "         , {}", block.type_info.display(r, inst_idx));
            }
        }

        iw!(text, "    ");
        match &block.end {
            EndInstruction::Jump(inst) => inst.display(&mut text),
            EndInstruction::JumpIf(inst) => inst.display(&mut text),
            EndInstruction::Return(inst) => inst.display(&mut text),
            EndInstruction::Unreachable(inst) => {
                <Unreachable as ISAInstruction<AssemblerCtx>>::display(inst, &mut text)
            }
        }
        .unwrap();
        iwl!(text);

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
