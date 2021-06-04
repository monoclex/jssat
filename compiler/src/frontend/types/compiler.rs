use std::collections::HashMap;

use super::ir::*;
use crate::frontend::js::ir::{Instruction, IR};
use crate::id::*;

pub fn compile(ir: &IR) -> TypesArtifact {
    let mut annotations = TypeAnnotations {
        functions: HashMap::new(),
    };

    for (id, ext_fn) in ir.external_functions.iter() {
        let mut types = TypeManager::new();

        annotations.functions.insert(
            *id,
            FunctionContext {
                return_type: types.id(&map_type(&ext_fn.return_type)),
                // TODO: this feels **wrong**, as the `monomorphizer` will map the
                // parameter types for us but we have to map the return type. seems like
                // the type annotations could be designed better
                registers: HashMap::new(),
                types,
            },
        );
    }

    for (id, function) in ir.functions.iter() {
        // TODO: proper annotation of a function
        // proper annotation would involve going over every block, making a bag
        // mapping every register to a type, and then going to other functions
        // and if they use `phi` pull types from the corresponding bags given some
        // conditional, and a lot of generally really fun stuff :))))))))))

        let mut return_type = None;
        let mut registers = HashMap::new();

        // TODO encapsulate free_id + types mechanism
        // (i believe it is already encaaspulated somewhere else actually)
        let mut types = TypeManager::new();

        // TODO: this is a hack to ensure that in `monomorphizer` we have a
        // `Runtime` type id to use.
        types.id(&Type::Runtime);

        for p in function.parameters.iter() {
            // TODO: insert generics and whatever other fanciness
            registers.insert(p.register, types.id(&Type::Any));
        }

        for i in function
            .body
            .blocks
            .iter()
            .flat_map(|f| f.instructions.iter())
        {
            match i {
                Instruction::LoadGlobal(_, _) => todo!("LoadGlobal"),
                Instruction::SaveGlobal(_, _) => todo!("SaveGlobal"),
                Instruction::RecordGet(_, _, _) => todo!("RecordGet"),
                Instruction::RecordSet(_, _, _) => todo!("RecordSet"),
                Instruction::RefIsEmpty(_, _) => todo!("RefIsEmpty"),
                Instruction::RefDeref(_, _) => todo!("RefDeref"),
                Instruction::MakePrimitive(_, _) => todo!("MakePrimitive"),
                Instruction::GcMakeRegion(_) => todo!("GcMakeRegion"),
                Instruction::GcEndRegion(_) => todo!("GcEndRegion"),
                Instruction::GcTracingMarkRoot(_) => todo!("GcTracingMarkRoot"),
                Instruction::GcTracingUnmarkRoot(_) => todo!("GcTracingUnmarkRoot"),
                Instruction::Call(a, b, c) => {
                    if let Some(r) = a {
                        todo!("determine callable return type");
                    }

                    let f = *match b {
                        crate::frontend::js::ir::Callable::GlobalFunction(f) => f,
                        crate::frontend::js::ir::Callable::LocalFunction(_) => {
                            todo!("analyze tpyes ")
                        }
                    };

                    // don't need to do anythign with values since they are all
                    // simple types
                }
                Instruction::Phi(_, _) => todo!("Phi"),
                Instruction::Jmp(_) => todo!("Jmp"),
                Instruction::JmpIf(_, _) => todo!("JmpIf"),
                Instruction::Ret(a) => {
                    // TODO: support more than one `ret`
                    match a {
                        Some(r) => return_type = Some(*registers.get(r).unwrap()),
                        None => return_type = Some(types.id(&Type::Void)),
                    };
                }
            }
        }

        annotations.functions.insert(
            *id,
            FunctionContext {
                return_type: return_type.unwrap(),
                registers,
                types,
            },
        );
    }

    TypesArtifact { annotations }
}

fn map_type(input: &crate::frontend::js::ir::Type) -> Type {
    // TODO: we should be able to handle returning runtime (not that an ext fn
    // that returns the rt would do anything useful)
    match input {
        crate::frontend::js::ir::Type::Any => Type::Any,
        crate::frontend::js::ir::Type::Runtime => panic!("rt"),
        crate::frontend::js::ir::Type::Void => Type::Void,
        crate::frontend::js::ir::Type::Constant(_) => panic!("const"),
    }
}
