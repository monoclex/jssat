use inkwell::{
    context::Context,
    module::{Linkage, Module},
    types::{AnyType, BasicType},
    types::{AnyTypeEnum, BasicTypeEnum, FunctionType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue},
    AddressSpace,
};
use std::{borrow::Cow, collections::HashMap};

use crate::backend::{runtime_glue::RuntimeGlue, skeleton::ir::*};
use crate::id::*;

pub struct SkeletonCompiler<'compilation, 'module> {
    ir: &'compilation IR,
    type_info: &'compilation TypeAnnotations,
    context: &'compilation Context,
    // builder: &'compilation Builder<'compilation>,
    module: &'module Module<'compilation>,
    glue: &'module RuntimeGlue<'compilation, 'module>,
}

pub struct SkeletonArtifact<'compilation> {
    types: HashMap<TypeId, AnyTypeEnum<'compilation>>,
    globals: HashMap<TopLevelId, GlobalValue<'compilation>>,
    functions: HashMap<TopLevelId, FunctionValue<'compilation>>,
}

impl<'c, 'm> SkeletonCompiler<'c, 'm> {
    pub fn compile(&self) -> SkeletonArtifact<'c> {
        let mut artifact = SkeletonArtifact {
            types: self.populate_types(),
            globals: HashMap::new(),
            functions: HashMap::new(),
        };

        for (&id, constant) in self.ir.constants.iter() {
            let global = self.populate_constant(id, constant);
            artifact.globals.insert(id, global);
        }

        for (&id, ext_func) in self.ir.external_functions.iter() {
            let llvm_fn = self.populate_external_function(id, ext_func, &artifact.types);
            artifact.functions.insert(id, llvm_fn);
        }

        for (&id, func) in self.ir.functions.iter() {
            let llvm_fn = self.populate_function(id, func, &artifact.types);
            artifact.functions.insert(id, llvm_fn);
        }

        artifact
    }

    // DISCUSSION: should this be in the skeleton compiler? as really, we need
    // full type info before making skeletons of `globals` and `functions`, but at that
    // point, it feels like a lot of boilerplate to have a `TypeCompiler` state.
    //
    // for the time being, these are going to be left here to reduce boilerplate.
    fn populate_types(&self) -> HashMap<TypeId, AnyTypeEnum<'c>> {
        let mut types = HashMap::new();

        for (&id, r#type) in self.type_info.type_map.iter() {
            let struct_type = self.create_type(id, r#type);
            types.insert(id, struct_type);
        }

        types
    }

    // DISCUSSION: see above
    fn create_type(&self, _id: TypeId, r#type: &Type) -> AnyTypeEnum<'c> {
        match r#type {
            Type::Any => self.glue.type_value.as_any_type_enum(),
            Type::Void => self.context.void_type().as_any_type_enum(),
            Type::Runtime => self.glue.type_runtime.as_any_type_enum(),
            // TODO: analyze why we need to handle this case
            Type::Constant(_) => panic!("a constant should not be present in the `TypeId`s"),
        }

        // let name = format!(".{}", id);

        // let opaque_struct = self.context.opaque_struct_type(name.as_str());
    }

    fn populate_constant(&self, id: TopLevelId, constant: &Constant) -> GlobalValue<'c> {
        let name = (self.ir.debug_info.top_level_names.get(&id))
            .map(|s| s.as_ref())
            .unwrap_or("");

        let raw_const = self.make_constant_value(&constant);

        let global =
            self.module
                .add_global(raw_const.get_type(), Some(AddressSpace::Generic), name);

        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        global.set_constant(true);
        global.set_initializer(&raw_const);

        global
    }

    fn make_constant_value(&self, constant: &Constant) -> BasicValueEnum<'c> {
        match std::str::from_utf8(constant.payload.as_slice()) {
            Ok(str) => self
                .context
                .const_string(str.as_bytes(), false)
                .as_basic_value_enum(),
            Err(_) => {
                let byte_values = constant
                    .payload
                    .iter()
                    .map(|n| self.context.i8_type().const_int(*n as u64, false))
                    .collect::<Vec<_>>();

                self.context
                    .i8_type()
                    .const_array(byte_values.as_slice())
                    .as_basic_value_enum()
            }
        }
    }

    // DISCUSSION: should `populate_external_function` and `populate_function`, be unified?
    // for now, going with "no" as code duplication for two functions isn't really an issue.
    //
    // once it gets to 3+, then i'd want to start thinking about it.
    fn populate_external_function(
        &self,
        id: TopLevelId,
        _ext_func: &ExternalFunction,
        types: &HashMap<TypeId, AnyTypeEnum<'c>>,
    ) -> FunctionValue<'c> {
        let name = (self.ir.debug_info.top_level_names.get(&id))
            .map(|n| Cow::Borrowed(n.as_ref()))
            .unwrap_or_else(|| Cow::Owned(format!(".{}", id.value())));

        let annotations = (self.type_info.external_functions.get(&id))
            .expect("expected type information for external function");

        // TODO: this should really be encapsulated (repeated in this code quite a bit)
        // or perhaps we could include the type annotations with the external function in the first place?
        //
        // we pass along the `ext_func` but do nothing with it, it should really have the types in it
        let llvm_ret = types
            .get(&annotations.return_type)
            .expect("Expected `AnyTypeEnum` present for `TypeId`");

        let llvm_params = (annotations.parameter_annotations.iter())
            .map(|id| {
                types
                    .get(id)
                    .expect("Expected `AnyTypeEnum` present for `TypeId`")
            })
            .map(|a| LLVMAnyWrapper(a).coerce_basic())
            .collect::<Vec<_>>();

        let fn_type =
            LLVMAnyWrapper(&llvm_ret.as_any_type_enum()).fn_type(llvm_params.as_slice(), false);

        self.module
            .add_function(&name, fn_type, Some(Linkage::External))
    }

    fn populate_function(
        &self,
        id: TopLevelId,
        func: &Function,
        types: &HashMap<TypeId, AnyTypeEnum<'c>>,
    ) -> FunctionValue<'c> {
        let name = (self.ir.debug_info.top_level_names.get(&id))
            .map(|n| Cow::Borrowed(n.as_ref()))
            .unwrap_or_else(|| Cow::Owned(format!(".{}", id.value())));

        let annotations = (self.type_info.function_annotations.get(&id))
            .expect("expected type information for function");

        // TODO: this should really be encapsulated (repeated in this code quite a bit)
        // or perhaps we could include the type annotations with the external function in the first place?
        //
        // we pass along the `ext_func` but do nothing with it, it should really have the types in it
        let llvm_ret = types
            .get(&annotations.return_annotation)
            .expect("Expected `AnyTypeEnum` present for `TypeId`");

        let mut llvm_params = (annotations.parameter_annotations.iter())
            .map(|id| {
                types
                    .get(id)
                    .expect("Expected `AnyTypeEnum` present for `TypeId`")
            })
            .map(|a| LLVMAnyWrapper(a).coerce_basic())
            .collect::<Vec<_>>();

        // all JSSAT functions have an implicit runtime parameter except for main
        // main does not use `llvm_params` so we can do this in all cases.
        llvm_params.insert(0, self.glue.type_runtime.as_basic_type_enum());

        let fn_type =
            LLVMAnyWrapper(&llvm_ret.as_any_type_enum()).fn_type(llvm_params.as_slice(), false);

        match self.ir.entry_function == id {
            false => self.module.add_function(&name, fn_type, None),
            true => {
                let main_fn = self.context.i32_type().fn_type(&[], false);
                self.module.add_function("main", main_fn, None)
            }
        }
    }
}

struct LLVMAnyWrapper<'usage, 'ctx>(&'usage AnyTypeEnum<'ctx>);

impl<'c> LLVMAnyWrapper<'_, 'c> {
    pub fn fn_type(
        &self,
        param_types: &[BasicTypeEnum<'c>],
        is_var_args: bool,
    ) -> FunctionType<'c> {
        match self.0 {
            AnyTypeEnum::ArrayType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::FloatType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::FunctionType(t) => t
                .ptr_type(AddressSpace::Generic)
                .fn_type(param_types, is_var_args),
            AnyTypeEnum::IntType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::PointerType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::StructType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::VectorType(t) => t.fn_type(param_types, is_var_args),
            AnyTypeEnum::VoidType(t) => t.fn_type(param_types, is_var_args),
        }
    }

    pub fn coerce_basic(&self) -> BasicTypeEnum<'c> {
        match self.0 {
            AnyTypeEnum::ArrayType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::FloatType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::FunctionType(t) => {
                panic!("cannot coerce `FunctionType` to variant of `BasicTypeEnum`")
            }
            AnyTypeEnum::IntType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::PointerType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::StructType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::VectorType(t) => t.as_basic_type_enum(),
            AnyTypeEnum::VoidType(t) => {
                panic!("cannot coerce `VoidType` to variant of `BasicTypeEnum`")
            }
        }
    }
}
