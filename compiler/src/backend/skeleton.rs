use rustc_hash::{FxHashMap, FxHashSet};

use crate::backend::llvm::{
    self, BackendIR, Callable, Constant, ExternalFunction, LLVMLinkage, OpaqueStruct,
};
use crate::frontend::ir::{FFIReturnType, FFIValueType, Instruction};
use crate::frontend::{self};
use crate::frontend::{ir::IR, type_annotater::TypeAnnotations};
use crate::id::*;

use super::llvm::{Function, Parameter, ReturnType, ValueType};

struct OpaqueStructs {
    runtime: OpaqueStructId,
    any: OpaqueStructId,
}

pub fn translate<'ir>(ir: &'ir IR, annotations: &'ir TypeAnnotations) -> BackendIR<'ir> {
    //
    // let mut external_functions = FxHashMap::default();
    // let mut function = FxHashMap::default();

    let mut constants = FxHashMap::default();
    for (id, constant) in ir.constants.iter() {
        constants.insert(
            *id,
            Constant {
                name: constant.name.value().unwrap_or(""),
                payload: constant.payload.clone(),
            },
        );
    }

    // TODO: shove JSSATRT stuff into its own space
    let mut opaque_structs = FxHashMap::default();
    let runtime_id = OpaqueStructId::new();
    opaque_structs.insert(
        runtime_id,
        OpaqueStruct {
            name: "struct.Runtime",
        },
    );
    let any_id = runtime_id.next();
    opaque_structs.insert(
        any_id,
        OpaqueStruct {
            name: "struct.Value",
        },
    );
    let opaque_map = OpaqueStructs {
        runtime: runtime_id,
        any: any_id,
    };

    let mut external_functions = FxHashMap::default();

    // TODO: this is more JSSAT RT crud just littered in here. it's ugly.
    // TODO: this is repeated when getting the highest available id for registers.
    // this core logic (the "find by max" **and then `.next()` the result**) should be
    // put in a function
    let mut unused_id = ir
        .external_functions
        .iter()
        .map(|(id, _)| *id)
        .max_by(|id, id2| id.value().cmp(&id2.value()))
        .map(|r| r.next())
        .unwrap_or(ExternalFunctionId::new());

    let jssatrt_runtime_new = unused_id;
    let jssatrt_string_new = jssatrt_runtime_new.next();
    let jssatrt_ext_fns = std::array::IntoIter::new([
        (
            jssatrt_runtime_new,
            frontend::ir::ExternalFunction {
                name: "jssatrt_runtime_new".into(),
                parameters: vec![],
                return_type: FFIReturnType::Value(FFIValueType::Runtime),
            },
        ),
        (
            jssatrt_string_new,
            frontend::ir::ExternalFunction {
                name: "jssatrt_string_new".into(),
                parameters: vec![
                    FFIValueType::Runtime,
                    FFIValueType::BytePointer,
                    FFIValueType::Word,
                ],
                return_type: FFIReturnType::Value(FFIValueType::Any),
            },
        ),
    ])
    .collect::<FxHashMap<ExternalFunctionId, frontend::ir::ExternalFunction>>();

    for (id, external_function) in ir.external_functions.iter().chain(jssatrt_ext_fns.iter()) {
        if id.next().value() > unused_id.value() {
            unused_id = id.next();
        }

        let name = external_function.name.as_str().to_owned();

        let return_type = map_ffi_ret_type(&external_function.return_type, &opaque_map);

        let parameters = external_function
            .parameters
            .iter()
            .map(|p| map_ffi_val_type(p, &opaque_map))
            .collect();

        external_functions.insert(
            *id,
            ExternalFunction {
                name,
                return_type,
                parameters,
            },
        );
    }

    let mut functions = FxHashMap::default();
    for (id, function) in annotations.functions.iter() {
        let is_main = *id == annotations.entrypoint;

        let name = function
            .name
            .value()
            .unwrap_or(if is_main { "main" } else { "" });

        let linkage = is_main.then_some(LLVMLinkage::External);

        let return_type = map_ret_type(&function.return_type, &opaque_map);

        let runtime_parameter = function.top_free_register;
        let parameters = if is_main {
            // the main function doesn't have any parameters
            // (TODO: support argc, argv)
            vec![]
        } else {
            // if the function isn't the main function, prepend the runtime as
            // a parameter.
            //
            // all JSSAT functions must have the runtime passed as a parameter
            // to perform anything.
            std::iter::once(Parameter {
                register: runtime_parameter,
                r#type: map_ffi_val_type(&FFIValueType::Runtime, &opaque_map),
            })
            .chain(function.parameters.iter().map(|p| Parameter {
                register: p.register,
                r#type: map_val_type(&p.r#type, &opaque_map),
            }))
            .collect()
        };

        let mut blocks = FxHashMap::default();

        if is_main {
            let jssatrt_runtime_new = llvm::Instruction::Call(
                Some(runtime_parameter),
                Callable::External(jssatrt_runtime_new),
                vec![],
            );

            blocks.insert(function.entry_block, vec![jssatrt_runtime_new]);
        }

        // TODO: little capabilities like "which register has the runtime?"
        // should really be abstracted into a struct with a setup phase
        // of some kind
        let runtime_register = runtime_parameter;
        let mut free_register = runtime_parameter.next();

        let mut is_runtime_register = FxHashSet::default();

        for (id, block) in function.blocks.iter() {
            let instructions: &mut Vec<llvm::Instruction> =
                blocks.entry(*id).or_insert_with(|| vec![]);

            for instruction in block.instructions.iter() {
                match instruction {
                    Instruction::Call(result, callable, args) => {
                        let mut args = args
                            .iter()
                            .map(|r| {
                                is_runtime_register
                                    .contains(r)
                                    .then_some(runtime_register)
                                    .unwrap_or(*r)
                            })
                            .collect::<Vec<_>>();

                        let callable = match callable {
                            frontend::ir::Callable::External(id) => Callable::External(*id),
                            frontend::ir::Callable::Static(id) => {
                                // for jssat <-> jssat function calls, the runtime register is
                                // always the first parameter to the function
                                args.insert(0, runtime_register);
                                Callable::Static(*id)
                            } // frontend::ir::Callable::Virtual(_) => todo!(),
                        };

                        instructions.push(llvm::Instruction::Call(*result, callable, args));
                    }
                    Instruction::GetRuntime(register) => {
                        // any time we request the runtime to be put in the
                        // register, we'll directly inline the actual runtime
                        // register into whereever that register is used
                        is_runtime_register.insert(*register);
                    }
                    Instruction::MakeString(result, payload) => {
                        let const_ptr = free_register.next_and_mut();
                        let const_len = free_register.next_and_mut();

                        instructions.push(llvm::Instruction::LoadConstantPtr(const_ptr, *payload));
                        instructions.push(llvm::Instruction::LoadConstantLen(const_len, *payload));

                        instructions.push(llvm::Instruction::Call(
                            Some(*result),
                            Callable::External(jssatrt_string_new),
                            vec![runtime_register, const_ptr, const_len],
                        ));
                    }
                    Instruction::Unreachable => {
                        instructions.push(llvm::Instruction::Unreachable);
                    }
                };
            }

            match block.end {
                frontend::ir::ControlFlowInstruction::Ret(register) => {
                    let register = register
                        .and_then(|r| is_runtime_register.contains(&r).then_some(runtime_register));

                    instructions.push(llvm::Instruction::Return(register));
                }
            };
        }

        functions.insert(
            *id,
            Function {
                name,
                linkage,
                return_type,
                parameters,
                entry_block: function.entry_block,
                blocks,
            },
        );
    }

    BackendIR {
        constants,
        opaque_structs,
        external_functions,
        functions,
    }
}

fn map_ffi_ret_type(ffi: &FFIReturnType, opaque: &OpaqueStructs) -> ReturnType {
    match ffi {
        FFIReturnType::Void => ReturnType::Void,
        FFIReturnType::Value(v) => ReturnType::Value(map_ffi_val_type(v, opaque)),
    }
}

fn map_ffi_val_type(ffi: &FFIValueType, opaque: &OpaqueStructs) -> ValueType {
    match ffi {
        FFIValueType::Any => ValueType::Pointer(Box::new(ValueType::Opaque(opaque.any))),
        FFIValueType::Runtime => ValueType::Pointer(Box::new(ValueType::Opaque(opaque.runtime))),
        FFIValueType::BytePointer => ValueType::Pointer(Box::new(ValueType::BitType(8))), // i8*
        FFIValueType::Word => ValueType::WordSizeBitType,
    }
}

fn map_ret_type(
    r#type: &crate::frontend::type_annotater::ReturnType,
    opaque: &OpaqueStructs,
) -> ReturnType {
    match r#type {
        crate::frontend::type_annotater::ReturnType::Never => ReturnType::Void,
        crate::frontend::type_annotater::ReturnType::Void => ReturnType::Void,
        crate::frontend::type_annotater::ReturnType::Value(v) => {
            ReturnType::Value(map_val_type(v, opaque))
        }
    }
}

fn map_val_type(
    r#type: &crate::frontend::type_annotater::ValueType,
    opaque: &OpaqueStructs,
) -> ValueType {
    match r#type {
        frontend::type_annotater::ValueType::Any => map_ffi_val_type(&FFIValueType::Any, opaque),
        frontend::type_annotater::ValueType::Runtime => {
            map_ffi_val_type(&FFIValueType::Runtime, opaque)
        }
        // TODO: figure out a better type for a string and constant string
        frontend::type_annotater::ValueType::String
        | frontend::type_annotater::ValueType::ExactString(_) => {
            map_ffi_val_type(&FFIValueType::Any, opaque)
        }
        frontend::type_annotater::ValueType::BytePointer => {
            map_ffi_val_type(&FFIValueType::BytePointer, opaque)
        }
        frontend::type_annotater::ValueType::Word => map_ffi_val_type(&FFIValueType::Word, opaque),
        frontend::type_annotater::ValueType::Union(_) => todo!(),
    }
}
