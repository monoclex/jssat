//! <https://tc39.es/ecma262/#sec-abstract-operations>

use crate::{
    frontend::builder::{DynBlockBuilder, DynFinalizedBlockBuilder, RegisterId},
    isa::InternalSlot,
};

#[cfg(test)]
use crate::frontend::builder::ProgramBuilder;

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
    /// Performs the ECMAScript operation of `?`
    fn Q<F>(&mut self, invoke: F) -> RegisterId
    where
        F: FnOnce(&mut Self) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-normalcompletion>
    fn NormalCompletion(&mut self, argument: RegisterId) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-returnifabrupt>
    fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-definepropertyorthrow>
    fn DefinePropertyOrThrow(
        &mut self,
        O: RegisterId,
        P: RegisterId,
        desc: RegisterId,
    ) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-set-o-p-v-throw>
    fn Set(&mut self, O: RegisterId, P: RegisterId, V: RegisterId, Throw: bool) -> RegisterId;

    /// <https://tc39.es/ecma262/#sec-throwcompletion>
    fn ThrowCompletion(&mut self, argument: RegisterId) -> RegisterId;

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

    fn copy_slot(&mut self, src_record: RegisterId, dest_record: RegisterId, slot: InternalSlot);

    fn is_normal_completion(&mut self, completion: RegisterId) -> RegisterId;

    fn is_abrupt_completion(&mut self, completion: RegisterId) -> RegisterId;
}

#[test]
fn manual_test_if_then() {
    let mut program = ProgramBuilder::new();
    let mut main = program.start_function_main();
    let b = main.start_block_main();
    main.end_block(b.ret(None));
    program.end_function(main);
    let (f, [cond]) = program.start_function();
    let mut e = Emitter::new(&mut program, f);
    e.comment("if");
    e.if_then(cond, |e| e.comment("then"));
    e.comment("end");
    e.finish(|b| b.ret(None));
    let ir = program.finish();
    panic!("{}", crate::frontend::display_jssatir::display(&ir));
}

#[test]
fn manual_test_if_then_else() {
    let mut program = ProgramBuilder::new();
    let mut main = program.start_function_main();
    let b = main.start_block_main();
    main.end_block(b.ret(None));
    program.end_function(main);
    let (f, [cond]) = program.start_function();
    let mut e = Emitter::new(&mut program, f);
    e.comment("if");
    e.if_then_else(cond, |e| e.comment("then"), |e| e.comment("else"));
    e.comment("end");
    e.finish(|b| b.ret(None));
    let ir = program.finish();
    panic!("{}", crate::frontend::display_jssatir::display(&ir));
}

#[test]
fn manual_test_if_then_x_else_y() {
    let mut program = ProgramBuilder::new();
    let mut main = program.start_function_main();
    let block = main.start_block_main();
    main.end_block(block.ret(None));
    program.end_function(main);
    let (func, [cond, x, y]) = program.start_function();
    let mut e = Emitter::new(&mut program, func);
    e.comment("if");
    let _ = e.if_then_x_else_y(
        cond,
        |e| {
            e.comment("then");
            x
        },
        |e| {
            e.comment("else");
            y
        },
    );
    e.comment("end");
    e.finish(|b| b.ret(None));
    let ir = program.finish();
    panic!("{}", crate::frontend::display_jssatir::display(&ir));
}

#[test]
fn manual_test_if_then_end() {
    let mut program = ProgramBuilder::new();
    let mut main = program.start_function_main();
    let b = main.start_block_main();
    main.end_block(b.ret(None));
    program.end_function(main);
    let (f, [cond]) = program.start_function();
    let mut e = Emitter::new(&mut program, f);
    e.comment("if");
    e.if_then_end(cond, |e| {
        e.comment("then");
        |mut e| {
            e.comment("end");
            e.ret(None)
        }
    });
    e.comment("done");
    e.finish(|b| b.ret(None));
    let ir = program.finish();
    panic!("{}", crate::frontend::display_jssatir::display(&ir));
}

#[allow(non_snake_case)]
impl<const PARAMS: usize> EmitterExt for Emitter<'_, PARAMS> {
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
        self.function
            .end_block_dyn(end_path.jmp_dynargs(self.block.id, vec![]));
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
        let else_path_id = else_path.id;

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
        debug_assert_eq!(okay_path_id, self.block.id);
        let finalized = then_path.jmp_dynargs(self.block.id, Vec::new());
        self.function.end_block_dyn(finalized);

        std::mem::swap(&mut self.block, &mut else_path);
        r#else(self);
        std::mem::swap(&mut self.block, &mut else_path);
        debug_assert_eq!(okay_path_id, self.block.id);
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
        let else_path_id = else_path.id;

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
        debug_assert_eq!(okay_path_id, self.block.id);
        let finalized = then_path.jmp_dynargs(self.block.id, vec![then_val]);
        self.function.end_block_dyn(finalized);

        std::mem::swap(&mut self.block, &mut else_path);
        let else_val = r#else(self);
        std::mem::swap(&mut self.block, &mut else_path);
        debug_assert_eq!(okay_path_id, self.block.id);
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

    fn copy_slot(&mut self, src_record: RegisterId, dest_record: RegisterId, slot: InternalSlot) {
        let value = self.record_get_slot(src_record, slot);
        self.record_set_slot(dest_record, slot, value);
    }

    /// <https://tc39.es/ecma262/#sec-normalcompletion>
    fn NormalCompletion(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("NormalCompletion");

        //# 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]: empty }.
        let completion_record = self.record_new();
        let normal = self.program.constant_str("normal");
        let normal = self.make_string(normal);
        self.record_set_slot(completion_record, InternalSlot::Type, normal);
        self.record_set_slot(completion_record, InternalSlot::Value, argument);
        let empty = self.program.constant_str("empty");
        let empty = self.make_string(empty);
        self.record_set_slot(completion_record, InternalSlot::Target, empty);
        completion_record
    }

    fn DefinePropertyOrThrow(
        &mut self,
        O: RegisterId,
        P: RegisterId,
        desc: RegisterId,
    ) -> RegisterId {
        self.comment("DefinePropertyOrThrow");

        //# 1. Assert: Type(O) is Object.
        //# 2. Assert: IsPropertyKey(P) is true.

        //# 3. Let success be ? O.[[DefineOwnProperty]](P, desc).
        let define_own_property = self.record_get_slot(O, InternalSlot::DefineOwnProperty);
        // TODO: currently `DefineOwnProperty` can't fail
        let success = self.call_virt_with_result(define_own_property, [O, P, desc]);
        // let success = self.ReturnIfAbrupt(success_try);

        //# 4. If success is false, throw a TypeError exception.
        let success_false = self.negate(success);

        self.if_then_x_else_y(
            success_false,
            |b| {
                // TODO: throw a type error properly
                let argument = b.make_undefined();
                b.ThrowCompletion(argument)
            },
            |b| {
                //# 5. Return success.
                b.NormalCompletion(success)
            },
        )
    }

    /// <https://tc39.es/ecma262/#sec-throwcompletion>
    fn ThrowCompletion(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("ThrowCompletion");

        //# 1. Return Completion { [[Type]]: throw, [[Value]]: argument, [[Target]]: empty }.
        let completion_record = self.record_new();
        let throw = self.program.constant_str("throw");
        let normal = self.block.make_string(throw);
        self.record_set_slot(completion_record, InternalSlot::Type, normal);
        self.record_set_slot(completion_record, InternalSlot::Value, argument);
        let empty = self.program.constant_str("empty");
        let empty = self.block.make_string(empty);
        self.record_set_slot(completion_record, InternalSlot::Target, empty);
        completion_record
    }

    fn Set(&mut self, O: RegisterId, P: RegisterId, V: RegisterId, Throw: bool) -> RegisterId {
        self.comment("Set");

        //# 1. Assert: Type(O) is Object.
        //# 2. Assert: IsPropertyKey(P) is true.
        //# 3. Assert: Type(Throw) is Boolean.
        //# 4. Let success be ? O.[[Set]](P, V, O).
        // TODO: use `Q`
        let set = self.record_get_slot(O, InternalSlot::Set);
        let success = self.call_virt_with_result(set, [O, P, V, O]);

        //# 5. If success is false and Throw is true, throw a TypeError exception.
        if Throw {
            // TODO: do this cleaner
            let r#false = self.make_bool(false);
            let cond = self.compare_equal(success, r#false);
            self.if_then_end(cond, |b| {
                let argument = b.make_null();
                let completion = b.ThrowCompletion(argument);
                move |b| b.ret(Some(completion))
            });
        }

        //# 6. Return success.
        success
    }
}
