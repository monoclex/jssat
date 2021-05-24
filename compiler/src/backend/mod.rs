use std::collections::HashMap;

use inkwell::{context::Context, module::Linkage, targets::Target, types::BasicTypeEnum};

use crate::{
    ir::{Type, IR},
    types::TypeAnnotations,
};

pub fn build(ir: &IR, type_info: &TypeAnnotations) -> String {
    let context = Context::create();
    let builder = context.create_builder();
    let llvm_module = context.create_module("jssat");

    // ==> build LLVM types
    let mut types = HashMap::new();

    for (key, kind) in type_info.type_map.iter() {
        assert!(matches!(kind, Type::Any));

        let struct_type = context.opaque_struct_type(format!("anon.{}", key.value()).as_str());
        struct_type.set_body(&[context.i8_type().into()], false);

        types.insert(*key, struct_type);
    }

    // ==> setup function declarations
    for function in ir.external_functions.iter() {
        let name = ir
            .debug_info
            .top_level_names
            .get(&function.id)
            .map(|n| format!("{}", n))
            .unwrap_or(format!(".{}", function.id.value()));

        let fn_type = context.void_type().fn_type(
            &type_info
                .external_functions
                .get(&function.id)
                .unwrap()
                .parameter_annotations
                .iter()
                .map(|id| types.get(id).unwrap())
                .map(|t| BasicTypeEnum::StructType(*t))
                .collect::<Vec<_>>()
                .as_slice(),
            false,
        );

        llvm_module.add_function(name.as_str(), fn_type, Some(Linkage::ExternalWeak));
    }

    // ==> setup functions
    for function in ir.functions.iter() {
        let name = ir
            .debug_info
            .top_level_names
            .get(&function.id)
            .map(|n| format!("{}", n))
            .unwrap_or(format!(".{}", function.id.value()));

        let annotations = type_info.function_annotations.get(&function.id).unwrap();

        let fn_type = context.void_type().fn_type(
            function
                .parameters
                .iter()
                .map(|p| annotations.register_annotations.get(&p.register).unwrap())
                .map(|id| types.get(id).unwrap())
                .map(|t| BasicTypeEnum::StructType(*t))
                .collect::<Vec<_>>()
                .as_slice(),
            false,
        );

        let function = if function.is_main {
            llvm_module.add_function("main", fn_type, Some(Linkage::External))
        } else {
            llvm_module.add_function(name.as_str(), fn_type, None)
        };

        let basic_block = context.append_basic_block(function, "entry");

        builder.position_at_end(basic_block);
        builder.build_return(None);
    }

    // ==> output LLVM IR
    Target::initialize_all(&Default::default());

    let buff = llvm_module.print_to_string();
    buff.to_string()
}
