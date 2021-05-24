use std::collections::HashMap;

use inkwell::{
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Linkage,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::BasicTypeEnum,
    OptimizationLevel,
};

use crate::{
    ir::{Type, IR},
    types::TypeAnnotations,
};

pub struct BuildArtifact {
    pub llvm_ir: String,
    pub obj: Vec<u8>,
}

pub fn build(ir: &IR, type_info: &TypeAnnotations) -> BuildArtifact {
    let context = Context::create();
    let builder = context.create_builder();

    let llvm_module = context
        .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
            MAIN_C.as_bytes(),
            "main.c",
        ))
        .unwrap();

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

const MAIN_C: &'static str = r###"
; ModuleID = 'main.c'
source_filename = "main.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%union.FFIMap = type { [64 x i8] }

@.str = private unnamed_addr constant [17 x i8] c"yield_3() -> %d\0A\00", align 1
@.str.1 = private unnamed_addr constant [18 x i8] c"obtaining map...\0A\00", align 1
@.str.2 = private unnamed_addr constant [10 x i8] c"got map!\0A\00", align 1
@.str.3 = private unnamed_addr constant [14 x i8] c"dropped map!\0A\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %union.FFIMap, align 8
  store i32 0, i32* %1, align 4
  %3 = call i32 @yield_3()
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str, i64 0, i64 0), i32 %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.1, i64 0, i64 0))
  call void @make_map(%union.FFIMap* sret align 1 %2)
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.2, i64 0, i64 0))
  call void @drop_map(%union.FFIMap* byval(%union.FFIMap) align 8 %2)
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.3, i64 0, i64 0))
  ret i32 0
}

declare dso_local i32 @printf(i8*, ...) #1

declare dso_local i32 @yield_3() #1

declare dso_local void @make_map(%union.FFIMap* sret align 1) #1

declare dso_local void @drop_map(%union.FFIMap* byval(%union.FFIMap) align 8) #1

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 11.0.1"}

"###;
