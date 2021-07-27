//! <https://tc39.es/ecma262/#sec-abstract-operations>

use crate::{
    frontend::builder::{DynBlockBuilder, DynFinalizedBlockBuilder, RegisterId},
    isa::InternalSlot,
};

use super::Emitter;

#[allow(non_snake_case)]
pub trait BlockBuilderExt {
    fn IsExtensible(&mut self, O: RegisterId) -> RegisterId;
}

#[allow(non_snake_case)]
impl BlockBuilderExt for DynBlockBuilder {
    /// <https://tc39.es/ecma262/#sec-isextensible-o>
    fn IsExtensible(&mut self, O: RegisterId) -> RegisterId {
        self.comment("IsExtensible");

        //# 1. Assert: Type(O) is Object.
        //# 2. Return ? O.[[IsExtensible]]().
        let is_extensible = self.record_get_slot(O, InternalSlot::IsExtensible);
        self.call_virt_with_result(is_extensible, [O])
    }
}

#[allow(non_snake_case)]
pub trait EmitterExt {
    /// <https://tc39.es/ecma262/#sec-isaccessordescriptor>
    fn IsAccessorDescriptor(&mut self, Desc: RegisterId) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-isdatadescriptor>
    fn IsDataDescriptor(&mut self, Desc: RegisterId) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-isgenericdescriptor>
    fn IsGenericDescriptor(&mut self, Desc: RegisterId) -> RegisterId;

    /// Performs the ECMAScript operation of `?`
    fn Q<F>(&mut self, invoke: F) -> RegisterId
    where
        F: FnOnce(&mut Self) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-returnifabrupt>
    fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId;

    fn if_then<T>(&mut self, condition: RegisterId, then: T)
    where
        T: FnOnce(&mut Self);

    fn if_then_else<T, E>(&mut self, condition: RegisterId, then: T, r#else: E)
    where
        T: FnOnce(&mut Self),
        E: FnOnce(&mut Self);

    fn if_then_x_else_y<T, E>(&mut self, condition: RegisterId, then: T, r#else: E) -> RegisterId
    where
        T: FnOnce(&mut Self) -> RegisterId,
        E: FnOnce(&mut Self) -> RegisterId;

    fn if_then_end<T, E>(&mut self, condition: RegisterId, then: T)
    where
        T: FnOnce(&mut Self) -> E,
        E: FnOnce(DynBlockBuilder) -> DynFinalizedBlockBuilder;

    fn slot_or_default(
        &mut self,
        record: RegisterId,
        slot: InternalSlot,
        default: RegisterId,
    ) -> RegisterId;

    fn is_normal_completion(&mut self, completion: RegisterId) -> RegisterId;

    fn is_abrupt_completion(&mut self, completion: RegisterId) -> RegisterId;
}

#[allow(non_snake_case)]
impl<const P: usize> EmitterExt for Emitter<'_, P> {
    fn IsAccessorDescriptor(&mut self, Desc: RegisterId) -> RegisterId {
        self.comment("IsAccessorDescriptor");

        let r#true = self.make_bool(true);
        let r#false = self.make_bool(false);

        //# 1. If Desc is undefined, return false.
        let undefined = self.make_undefined();
        let is_undefined = self.compare_equal(Desc, undefined);
        self.if_then_end(is_undefined, |_| |b| b.ret(Some(r#false)));

        //# 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
        let has_get = self.record_has_slot(Desc, InternalSlot::Get);
        let has_set = self.record_has_slot(Desc, InternalSlot::Set);
        let TODO_AND_OPERATION = 0;

        //# 3. Return true.
        r#true
    }

    fn IsDataDescriptor(&mut self, Desc: RegisterId) -> RegisterId {
        self.comment("IsDataDescriptor");

        let r#true = self.make_bool(true);
        let r#false = self.make_bool(false);

        //# 1. If Desc is undefined, return false.
        let undefined = self.make_undefined();
        let is_undefined = self.compare_equal(Desc, undefined);
        self.if_then_end(is_undefined, |_| |b| b.ret(Some(r#false)));

        //# 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
        let has_value = self.record_has_slot(Desc, InternalSlot::Value);
        let has_writable = self.record_has_slot(Desc, InternalSlot::Writable);
        let TODO_AND_OPERATION = 0;

        //# 3. Return true.
        r#true
    }

    fn IsGenericDescriptor(&mut self, Desc: RegisterId) -> RegisterId {
        self.comment("IsGenericDescriptor");

        let r#true = self.make_bool(true);
        let r#false = self.make_bool(false);
        //# 1. If Desc is undefined, return false.
        let undefined = self.make_undefined();
        let is_undefined = self.compare_equal(Desc, undefined);
        self.if_then_end(is_undefined, |_| |b| b.ret(Some(r#false)));

        //# 2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, return true.
        let is_accessor = self.IsAccessorDescriptor(Desc);
        let is_data = self.IsDataDescriptor(Desc);
        let TODO_AND_OPERATION = 0;

        //# 3. Return false.
        r#false
    }

    fn Q<F>(&mut self, invoke: F) -> RegisterId
    where
        F: FnOnce(&mut Self) -> RegisterId,
    {
        let fallible = invoke(self);
        self.ReturnIfAbrupt(fallible)
    }

    fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("ReturnIfAbrupt");

        let is_abrupt_completion = self.is_abrupt_completion(argument);

        //# 1. If argument is an abrupt completion, return argument.
        self.if_then_end(is_abrupt_completion, |_| |b| b.ret(Some(argument)));

        //# 2. Else if argument is a Completion Record, set argument to argument.[[Value]].
        self.record_get_slot(argument, InternalSlot::Value)
    }

    fn is_normal_completion(&mut self, completion: RegisterId) -> RegisterId {
        self.comment("is_normal_completion");

        let completion_type = self.record_get_slot(completion, InternalSlot::Type);
        let normal_completion = self.block.make_string(self.program.constant_str("normal"));
        self.compare_equal(completion_type, normal_completion)
    }

    fn is_abrupt_completion(&mut self, completion: RegisterId) -> RegisterId {
        self.comment("is_abrupt_completion");

        let is_normal_completion = self.is_normal_completion(completion);
        self.negate(is_normal_completion)
    }

    fn if_then<T>(&mut self, condition: RegisterId, then: T)
    where
        T: FnOnce(&mut Self),
    {
        // TODO: don't copy `if_then_end`
        let (okay_path, []) = self.function.start_block();
        let mut okay_path = okay_path.into_dynamic();
        let okay_path_id = okay_path.id;

        let (end_path, []) = self.function.start_block();
        let mut end_path = end_path.into_dynamic();
        let end_path_id = end_path.id;

        std::mem::swap(&mut self.block, &mut okay_path);

        let finalized_old_path =
            okay_path.jmpif_dynargs(condition, end_path_id, Vec::new(), okay_path_id, Vec::new());
        self.function.end_block_dyn(finalized_old_path);

        std::mem::swap(&mut self.block, &mut end_path);
        then(self);
        std::mem::swap(&mut self.block, &mut end_path);
    }

    fn if_then_else<T, E>(&mut self, condition: RegisterId, then: T, r#else: E)
    where
        T: FnOnce(&mut Self),
        E: FnOnce(&mut Self),
    {
        // TODO: don't duplicate `if_then_end`
        let (okay_path, []) = self.function.start_block();
        let mut okay_path = okay_path.into_dynamic();
        let okay_path_id = okay_path.id;

        let (then_path, []) = self.function.start_block();
        let mut then_path = then_path.into_dynamic();
        let then_path_id = then_path.id;

        let (else_path, []) = self.function.start_block();
        let mut else_path = else_path.into_dynamic();
        let else_path_id = then_path.id;

        std::mem::swap(&mut self.block, &mut okay_path);

        let finalized_old_path = okay_path.jmpif_dynargs(
            condition,
            then_path_id,
            Vec::new(),
            else_path_id,
            Vec::new(),
        );
        self.function.end_block_dyn(finalized_old_path);

        std::mem::swap(&mut self.block, &mut then_path);
        then(self);
        std::mem::swap(&mut self.block, &mut then_path);
        let finalized = then_path.jmp_dynargs(self.block.id, Vec::new());
        self.function.end_block_dyn(finalized);

        std::mem::swap(&mut self.block, &mut else_path);
        r#else(self);
        std::mem::swap(&mut self.block, &mut else_path);
        let finalized = else_path.jmp_dynargs(self.block.id, Vec::new());
        self.function.end_block_dyn(finalized);
    }

    fn if_then_x_else_y<T, E>(&mut self, condition: RegisterId, then: T, r#else: E) -> RegisterId
    where
        T: FnOnce(&mut Self) -> RegisterId,
        E: FnOnce(&mut Self) -> RegisterId,
    {
        // TODO: don't duplicate `if_then_else`
        let (okay_path, [combine]) = self.function.start_block();
        let mut okay_path = okay_path.into_dynamic();
        let okay_path_id = okay_path.id;

        let (then_path, []) = self.function.start_block();
        let mut then_path = then_path.into_dynamic();
        let then_path_id = then_path.id;

        let (else_path, []) = self.function.start_block();
        let mut else_path = else_path.into_dynamic();
        let else_path_id = then_path.id;

        std::mem::swap(&mut self.block, &mut okay_path);

        let finalized_old_path = okay_path.jmpif_dynargs(
            condition,
            then_path_id,
            Vec::new(),
            else_path_id,
            Vec::new(),
        );
        self.function.end_block_dyn(finalized_old_path);

        std::mem::swap(&mut self.block, &mut then_path);
        let then_val = then(self);
        std::mem::swap(&mut self.block, &mut then_path);
        let finalized = then_path.jmp_dynargs(self.block.id, vec![then_val]);
        self.function.end_block_dyn(finalized);

        std::mem::swap(&mut self.block, &mut else_path);
        let else_val = r#else(self);
        std::mem::swap(&mut self.block, &mut else_path);
        let finalized = else_path.jmp_dynargs(self.block.id, vec![else_val]);
        self.function.end_block_dyn(finalized);

        combine
    }

    fn if_then_end<T, E>(&mut self, condition: RegisterId, then: T)
    where
        T: FnOnce(&mut Self) -> E,
        E: FnOnce(DynBlockBuilder) -> DynFinalizedBlockBuilder,
    {
        let (okay_path, []) = self.function.start_block();
        let mut okay_path = okay_path.into_dynamic();
        let okay_path_id = okay_path.id;

        let (end_path, []) = self.function.start_block();
        let mut end_path = end_path.into_dynamic();
        let end_path_id = end_path.id;

        std::mem::swap(&mut self.block, &mut okay_path);

        let finalized_old_path =
            okay_path.jmpif_dynargs(condition, end_path_id, Vec::new(), okay_path_id, Vec::new());
        self.function.end_block_dyn(finalized_old_path);

        std::mem::swap(&mut self.block, &mut end_path);
        let end = then(self);
        std::mem::swap(&mut self.block, &mut end_path);

        let finalized = end(end_path);
        self.function.end_block_dyn(finalized);
    }

    fn slot_or_default(
        &mut self,
        record: RegisterId,
        slot: InternalSlot,
        default: RegisterId,
    ) -> RegisterId {
        let has_value_in_slot = self.record_has_slot(record, slot);
        self.if_then_x_else_y(
            has_value_in_slot,
            |b| b.record_get_slot(record, slot),
            |_| default,
        )
    }
}
