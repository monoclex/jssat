use inkwell::module::Linkage;
use rustc_hash::FxHashMap;

use crate::backend::llvm::{BackendIR, Constant, ExternalFunction, OpaqueStruct};
use crate::frontend::ir::{FFIReturnType, FFIValueType};
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
    for (id, external_function) in ir.external_functions.iter() {
        external_functions.insert(
            *id,
            ExternalFunction {
                name: external_function.name.as_str(),
                return_type: map_ffi_ret_type(&external_function.return_type, &opaque_map),
                parameters: external_function
                    .parameters
                    .iter()
                    .map(|p| map_ffi_val_type(p, &opaque_map))
                    .collect(),
            },
        );
    }

    let mut functions = FxHashMap::default();
    for (id, function) in annotations.functions.iter() {
        functions.insert(
            *id,
            Function {
                name: function.name.value().unwrap_or(""),
                linkage: (*id == annotations.entrypoint).then_some(Linkage::External),
                return_type: map_ret_type(&function.return_type, &opaque_map),
                parameters: function
                    .parameters
                    .iter()
                    .map(|p| Parameter {
                        register: p.register,
                        r#type: map_val_type(&p.r#type, &opaque_map),
                    })
                    .collect(),
                entry_block: function.entry_block,
                blocks: function
                    .blocks
                    .iter()
                    .map(|(k, block)| {
                        // todo
                        (*k, vec![])
                    })
                    .collect(),
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
    }
}

fn map_ret_type(
    r#type: &crate::frontend::type_annotater::ReturnType,
    opaque: &OpaqueStructs,
) -> ReturnType {
    match r#type {
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
        crate::frontend::type_annotater::ValueType::Any => {
            map_ffi_val_type(&FFIValueType::Any, opaque)
        }
        crate::frontend::type_annotater::ValueType::Runtime => {
            map_ffi_val_type(&FFIValueType::Runtime, opaque)
        }
        crate::frontend::type_annotater::ValueType::String => todo!(),
        crate::frontend::type_annotater::ValueType::ExactString(_) => todo!(),
    }
}
