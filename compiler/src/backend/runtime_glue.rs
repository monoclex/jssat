use inkwell::{
    context::Context,
    module::{Linkage, Module},
    types::{BasicTypeEnum, PointerType, StructType},
    values::FunctionValue,
    AddressSpace,
};

// TODO: is it possible to validate this at compile time against `jssatrt`?
// maybe we should auto generate some LLVM IR based on `cbindgen` output,
// and somehow link that together here. /shrug

pub struct RuntimeGlue<'ctx, 'module> {
    pub type_runtime_value: StructType<'ctx>,
    pub type_runtime: PointerType<'ctx>,
    pub type_value_value: StructType<'ctx>,
    pub type_value: PointerType<'ctx>,
    pub type_key_value: StructType<'ctx>,
    pub type_key: PointerType<'ctx>,
    pub fn_jssatrt_runtime_new: FunctionValue<'module>,
    pub fn_jssatrt_record_tracing_new: FunctionValue<'module>,
    pub fn_jssatrt_constant_new: FunctionValue<'module>,
    pub fn_jssatrt_key_new_fromslot: FunctionValue<'module>,
    pub fn_jssatrt_record_set: FunctionValue<'module>,
    pub fn_jssatrt_runtime_drop: FunctionValue<'module>,
}

impl<'c, 'm> RuntimeGlue<'c, 'm> {
    pub fn new(context: &'c Context, module: &'m Module<'c>) -> RuntimeGlue<'c, 'm> {
        let type_runtime_value = context.opaque_struct_type("struct.Runtime");
        let type_value_value = context.opaque_struct_type("struct.Value");
        let type_key_value = context.opaque_struct_type("struct.Key");

        let type_runtime = type_runtime_value.ptr_type(AddressSpace::Generic);
        let type_value = type_value_value.ptr_type(AddressSpace::Generic);
        let type_key = type_key_value.ptr_type(AddressSpace::Generic);

        #[cfg(target_pointer_width = "32")]
        let ptr_size = context.i32_type();
        #[cfg(target_pointer_width = "64")]
        let ptr_size = context.i64_type();

        let word = BasicTypeEnum::IntType(ptr_size);

        let fn_jssatrt_runtime_new = type_runtime.fn_type(&[], false);
        let fn_jssatrt_record_tracing_new = type_value.fn_type(&[type_runtime.into()], false);
        let fn_jssatrt_constant_new = type_value.fn_type(
            &[
                type_runtime.into(),
                context.i8_type().ptr_type(AddressSpace::Generic).into(),
                word,
            ],
            false,
        );
        let fn_jssatrt_key_new_fromslot = type_key.fn_type(&[type_runtime.into(), word], false);
        let fn_jssatrt_record_set = context.void_type().fn_type(
            &[
                type_runtime.into(),
                type_value.into(),
                type_key.into(),
                type_value.into(),
            ],
            false,
        );
        let fn_jssatrt_runtime_drop = context.void_type().fn_type(&[type_runtime.into()], false);

        let fn_jssatrt_runtime_new = module.add_function(
            "jssatrt_runtime_new",
            fn_jssatrt_runtime_new,
            Some(Linkage::External),
        );
        let fn_jssatrt_record_tracing_new = module.add_function(
            "jssatrt_record_tracing_new",
            fn_jssatrt_record_tracing_new,
            Some(Linkage::External),
        );
        let fn_jssatrt_constant_new = module.add_function(
            "jssatrt_constant_new",
            fn_jssatrt_constant_new,
            Some(Linkage::External),
        );
        let fn_jssatrt_key_new_fromslot = module.add_function(
            "jssatrt_key_new_fromslot",
            fn_jssatrt_key_new_fromslot,
            Some(Linkage::External),
        );
        let fn_jssatrt_record_set = module.add_function(
            "jssatrt_record_set",
            fn_jssatrt_record_set,
            Some(Linkage::External),
        );
        let fn_jssatrt_runtime_drop = module.add_function(
            "jssatrt_runtime_drop",
            fn_jssatrt_runtime_drop,
            Some(Linkage::External),
        );

        RuntimeGlue {
            type_runtime_value,
            type_runtime,
            type_value_value,
            type_value,
            type_key_value,
            type_key,
            fn_jssatrt_runtime_new,
            fn_jssatrt_record_tracing_new,
            fn_jssatrt_constant_new,
            fn_jssatrt_key_new_fromslot,
            fn_jssatrt_record_set,
            fn_jssatrt_runtime_drop,
        }
    }
}
