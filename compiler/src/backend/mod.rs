use std::collections::HashMap;

use inkwell::{
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Linkage,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::BasicTypeEnum,
    values::FunctionValue,
    OptimizationLevel,
};

use crate::{
    ir::{Type, IR},
    types::TypeAnnotations,
};

use self::runtime_glue::RuntimeGlue;

mod runtime_glue;

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn build(ir: &IR, type_info: &TypeAnnotations) -> BuildArtifact {
    let context = Context::create();

    let builder = context.create_builder();
    let llvm_module = context.create_module("jssat_gen");

    let glue = RuntimeGlue::new(&context, &llvm_module);

    let main = context.i32_type().fn_type(&[], false);
    let main = llvm_module.add_function("main", main, Some(Linkage::External));

    let entry = context.append_basic_block(main, "entry");
    builder.position_at_end(entry);

    // ==> setup
    let runtime_inst = builder.build_call(glue.fn_jssatrt_runtime_new, &[], "runtime");

    let runtime = runtime_inst.try_as_basic_value().unwrap_left();

    // make a record and print it for funsies
    let record_inst = builder.build_call(glue.fn_jssatrt_record_tracing_new, &[runtime], "record");

    let record = record_inst.try_as_basic_value().unwrap_left();

    builder.build_call(glue.fn_jssatrt_print, &[runtime, record, record], "");

    // <== teardown
    builder.build_call(glue.fn_jssatrt_runtime_drop, &[runtime], "");

    builder.build_return(Some(&context.i32_type().const_int(0, false)));

    compile(llvm_module)
}

fn compile(llvm_module: inkwell::module::Module) -> BuildArtifact {
    Target::initialize_all(&Default::default());

    let target_triple = TargetMachine::get_default_triple();

    let target = Target::from_triple(&target_triple).unwrap();

    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("couldn't make target machine");

    let obj_buff = target_machine
        .write_to_memory_buffer(&llvm_module, FileType::Object)
        .expect("couldn't compile to assembly");

    let text_buff = llvm_module.print_to_string();

    BuildArtifact {
        llvm_ir: text_buff.to_string(),
        obj: obj_buff.as_slice().to_vec(),
    }
}

pub fn build2(ir: &IR, type_info: &TypeAnnotations) -> BuildArtifact {
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

    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("couldn't make target machine");

    let obj_buff = target_machine
        .write_to_memory_buffer(&llvm_module, FileType::Object)
        .expect("couldn't compile to assembly");

    let text_buff = llvm_module.print_to_string();

    BuildArtifact {
        llvm_ir: text_buff.to_string(),
        obj: obj_buff.as_slice().to_vec(),
    }
}
