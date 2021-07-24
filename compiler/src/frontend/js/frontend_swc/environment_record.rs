//! x

use ref_cast::RefCast;
use std::ops::Deref;

use crate::{
    frontend::builder::{DynBlockBuilder, FnSignature, ProgramBuilder, RegisterId},
    isa::InternalSlot,
};

pub struct EnvironmentRecordFactory {
    declarative_environment_vtable: EnvironmentRecordVTable,
    object_environment_vtable: EnvironmentRecordVTable,
    function_environment_vtable: EnvironmentRecordVTable,
    global_environment_vtable: EnvironmentRecordVTable,
    // module_environment_vtable: EnvironmentRecordVTable,
}

#[allow(non_snake_case)]
impl EnvironmentRecordFactory {
    pub fn new(writer: &mut ProgramBuilder) -> Self {
        let declarative_environment_vtable =
            EnvironmentRecordFactory::init_vtable_declarative(writer);
        let object_environment_vtable = EnvironmentRecordFactory::init_vtable_object(writer);
        let function_environment_vtable =
            EnvironmentRecordFactory::init_vtable_function(writer, &declarative_environment_vtable);
        let global_environment_vtable = EnvironmentRecordFactory::init_vtable_global(writer);

        Self {
            declarative_environment_vtable,
            object_environment_vtable,
            function_environment_vtable,
            global_environment_vtable,
        }
    }

    pub fn make_decl_env_rec(&self, block: &mut DynBlockBuilder) -> EnvironmentRecord {
        self.make_env_rec(block, &self.declarative_environment_vtable)
    }

    pub fn make_obj_env_rec(&self, block: &mut DynBlockBuilder) -> EnvironmentRecord {
        self.make_env_rec(block, &self.object_environment_vtable)
    }

    pub fn make_func_env_rec(&self, block: &mut DynBlockBuilder) -> EnvironmentRecord {
        self.make_env_rec(block, &self.function_environment_vtable)
    }

    pub fn make_global_env_rec(&self, block: &mut DynBlockBuilder) -> EnvironmentRecord {
        self.make_env_rec(block, &self.global_environment_vtable)
    }

    fn make_env_rec(
        &self,
        block: &mut DynBlockBuilder,
        vtable: &EnvironmentRecordVTable,
    ) -> EnvironmentRecord {
        let record = block.record_new();

        let has_binding = block.make_fnptr(vtable.has_binding.id);
        block.record_set_slot(record, InternalSlot::JSSATHasBinding, has_binding);

        EnvironmentRecord::new_with_register_unchecked(record)
    }

    fn init_vtable_declarative(writer: &mut ProgramBuilder) -> EnvironmentRecordVTable {
        let has_binding = {
            let (mut f, [env, N]) = writer.start_function();
            let mut w = f.start_block_main();

            //# 1. If envRec has a binding for the name that is the value of N, return true.
            // TODO: instruction to see if key/value exists

            //# 2. Return false.

            f.end_block(w.ret(None));
            writer.end_function(f)
        };

        EnvironmentRecordVTable { has_binding }
    }

    fn init_vtable_object(writer: &mut ProgramBuilder) -> EnvironmentRecordVTable {
        let has_binding = {
            let (mut f, [envRec, N]) = writer.start_function();
            let mut w = f.start_block_main();

            //# 1. Let bindingObject be envRec.[[BindingObject]].
            let bindingObject = w.record_get_slot(envRec, InternalSlot::BindingObject);

            //# 2. Let foundBinding be ? HasProperty(bindingObject, N).
            // TODO: some way to invoke `HasProperty`

            //# 3. If foundBinding is false, return false.
            //# 4. If envRec.[[IsWithEnvironment]] is false, return true.
            //# 5. Let unscopables be ? Get(bindingObject, @@unscopables).
            //# 6. If Type(unscopables) is Object, then
            //# a. Let blocked be ! ToBoolean(? Get(unscopables, N)).
            //# b. If blocked is true, return false.
            //# 7. Return true.

            f.end_block(w.ret(None));
            writer.end_function(f)
        };

        EnvironmentRecordVTable { has_binding }
    }

    fn init_vtable_function(
        writer: &mut ProgramBuilder,
        declarative_environment_vtable: &EnvironmentRecordVTable,
    ) -> EnvironmentRecordVTable {
        // function declarative objects inherit most methods from the declarative environment

        EnvironmentRecordVTable {
            has_binding: declarative_environment_vtable.has_binding,
        }
    }

    fn init_vtable_global(writer: &mut ProgramBuilder) -> EnvironmentRecordVTable {
        let has_binding = {
            let (mut f, [envRec, N]) = writer.start_function();
            let mut w = f.start_block_main();

            //# 1. Let DclRec be envRec.[[DeclarativeRecord]].
            let DclRec = w.record_get_slot(envRec, InternalSlot::DeclarativeRecord);

            //# 2. If DclRec.HasBinding(N) is true, return true.
            let DclRec = EnvironmentRecord::new_with_register_unchecked(DclRec);
            // TODO: use result somehow
            DclRec.HasBinding(&mut w, N);

            //# 3. Let ObjRec be envRec.[[ObjectRecord]].
            let ObjRec = w.record_get_slot(envRec, InternalSlot::ObjectRecord);

            //# 4. Return ? ObjRec.HasBinding(N).
            let ObjRec = EnvironmentRecord::new_with_register_unchecked(ObjRec);
            // TODO: use result somehow
            ObjRec.HasBinding(&mut w, N);

            f.end_block(w.ret(None));
            writer.end_function(f)
        };

        EnvironmentRecordVTable { has_binding }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, RefCast)]
#[repr(transparent)]
pub struct EnvironmentRecord {
    pub register: RegisterId,
}

#[allow(non_snake_case)]
impl EnvironmentRecord {
    /// This assumes that you have created an [`EnvironmentRecord`] with the
    /// [`EnvironmentRecordFactory`] before-hand. If you have not done so, this
    /// API will not behave correctly. Use at your own disgression.
    pub fn new_with_register_unchecked(record: RegisterId) -> Self {
        Self { register: record }
    }

    pub fn HasBinding(&self, block: &mut DynBlockBuilder, N: RegisterId) -> RegisterId {
        block.comment("HasBinding");

        let func = block.record_get_slot(self.register, InternalSlot::JSSATHasBinding);
        block.call_virt_with_result(func, [self.register, N])
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct GlobalEnvironmentRecord {
    pub register: RegisterId,
}

impl Deref for GlobalEnvironmentRecord {
    type Target = EnvironmentRecord;

    fn deref(&self) -> &Self::Target {
        EnvironmentRecord::ref_cast(&self.register)
    }
}

#[allow(non_snake_case)]
impl GlobalEnvironmentRecord {
    /// This assumes that you have created an [`EnvironmentRecord`] with the
    /// [`EnvironmentRecordFactory`] before-hand. If you have not done so, this
    /// API will not behave correctly. Use at your own disgression.
    pub fn new_with_register_unchecked(record: RegisterId) -> Self {
        Self { register: record }
    }

    pub fn HasVarDeclaration(&self, block: &mut DynBlockBuilder, N: RegisterId) -> RegisterId {
        block.comment("HasVarDeclaration");

        todo!()
    }
}

/// Has fields for every method specified in
/// <https://tc39.es/ecma262/#table-abstract-methods-of-environment-records>.
///
/// Every function has one parameter for the environment record itself to be
/// passed in, and the remaining parameters are dedicated to what's specified
/// in the specification.
struct EnvironmentRecordVTable {
    has_binding: FnSignature<2>,
    // create_mutable_binding: FnSignature<3>,
    // create_immutable_binding: FnSignature<3>,
    // initialize_binding: FnSignature<3>,
    // set_mutable_binding: FnSignature<4>,
    // get_binding_value: FnSignature<3>,
    // delete_binding: FnSignature<2>,
    // has_this_binding: FnSignature<1>,
    // has_super_binding: FnSignature<1>,
    // with_base_object: FnSignature<1>,
}
