use inkwell::{
    context::Context,
    module::{Linkage, Module},
    types::AnyType,
    types::{AnyTypeEnum, BasicTypeEnum, FunctionType},
    values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue},
    AddressSpace,
};
use std::{borrow::Cow, collections::HashMap};

use crate::backend::{runtime_glue::RuntimeGlue, skeleton::ir::*};
use crate::id::*;

pub struct SkeletonCompiler<'compilation, 'module, 'type_info, 'glue, 'monomorphizer> {
    pub ir: &'compilation IR,
    pub type_info: &'type_info TypeManager,
    pub monomorphizer:
        &'monomorphizer mut LLVMMonomorphizer<'type_info, 'compilation, 'module, 'glue>,
    pub context: &'compilation Context,
    // pub builder: &'compilation Builder<'compilation>,
    pub module: &'module Module<'compilation>,
    pub glue: &'glue RuntimeGlue<'compilation, 'module>,
}

pub struct SkeletonArtifact<'compilation> {
    pub globals: HashMap<TopLevelId, GlobalValue<'compilation>>,
    pub functions: HashMap<TopLevelId, FunctionValue<'compilation>>,
}

impl<'c, 'm> SkeletonCompiler<'c, 'm, '_, '_, '_> {
    pub fn compile(&mut self) -> SkeletonArtifact<'c> {
        let mut artifact = SkeletonArtifact {
            globals: HashMap::new(),
            functions: HashMap::new(),
        };

        for (&id, constant) in self.ir.constants.iter() {
            let global = self.populate_constant(id, constant);
            artifact.globals.insert(id, global);
        }

        for (&id, func) in self.ir.functions.iter() {
            let llvm_fn = self.populate_function(id, func);
            artifact.functions.insert(id, llvm_fn);
        }

        artifact
    }

    fn populate_constant(&self, _id: TopLevelId, constant: &Constant) -> GlobalValue<'c> {
        let name = constant.name.as_deref().unwrap_or("");

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

    fn populate_function(&mut self, id: TopLevelId, function: &Function) -> FunctionValue<'c> {
        let name = (function.name.as_deref())
            .map(|n| Cow::Borrowed(n))
            .unwrap_or_else(|| Cow::Owned(format!(".{}", id.value())));

        let llvm_ret = match function.return_type {
            PossibleType::Void => self.context.void_type().as_any_type_enum(),
            PossibleType::Value(v) => self.monomorphizer.llvm_type(v).as_any_type_enum(),
        };

        // TODO: the `AnyWrapper` stuff shouldn't need to exist
        let llvm_params = (function.parameter_types.iter())
            .map(|id| self.monomorphizer.llvm_type(*id))
            .collect::<Vec<_>>();

        let kind = function.kind(id, self.ir.entry_function);

        match kind {
            FunctionKind::Code => {
                // all JSSAT functions have an implicit runtime parameter
                // TODO: investigate if this should be here, as it might be the frontend's responsibility
                // to emit runtime parameters
                // llvm_params.insert(0, self.glue.type_runtime.as_basic_type_enum());
            }
            FunctionKind::Entrypoint | FunctionKind::External => {}
        };

        let fn_type =
            LLVMAnyWrapper(llvm_ret.as_any_type_enum()).fn_type(llvm_params.as_slice(), false);

        match kind {
            FunctionKind::Code => self.module.add_function(&name, fn_type, None),
            FunctionKind::External => {
                self.module
                    .add_function(&name, fn_type, Some(Linkage::External))
            }
            FunctionKind::Entrypoint => {
                let main_fn = self.context.i32_type().fn_type(&[], false);
                self.module
                    .add_function("main", main_fn, Some(Linkage::External))
            }
        }
    }
}

struct LLVMAnyWrapper<'ctx>(AnyTypeEnum<'ctx>);

impl<'c> LLVMAnyWrapper<'c> {
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
}
