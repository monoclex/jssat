#![allow(non_snake_case)]

use crate::frontend::builder::RegisterId;
use crate::frontend::emitter::*;
use crate::isa::{InternalSlot, ValueType};

pub trait EmitterExt<'builder, const P: usize> {
    fn Q(&mut self, argument: RegisterId) -> RegisterId;
    fn E(&mut self, argument: RegisterId) -> RegisterId;
    fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId;

    fn is_completion(&mut self, argument: RegisterId) -> RegisterId;
    fn is_normal_completion(&mut self, argument: RegisterId) -> RegisterId;
    fn is_abrupt_completion(&mut self, argument: RegisterId) -> RegisterId;

    fn NormalCompletion(&mut self, argument: RegisterId) -> RegisterId;
    fn ThrowCompletion(&mut self, argument: RegisterId) -> RegisterId;
}

impl<'bu, const P: usize> EmitterExt<'bu, P> for Emitter<'bu, P> {
    /// 5.2.3.4 ReturnIfAbrupt Shorthands
    ///
    /// Invocations of abstract operations and syntax-directed operations that
    /// are prefixed by ? indicate that ReturnIfAbrupt should be applied to the
    /// resulting Completion Record.
    fn Q(&mut self, argument: RegisterId) -> RegisterId {
        self.ReturnIfAbrupt(argument)
    }

    /// 5.2.3.4 ReturnIfAbrupt Shorthands
    ///
    /// Similarly, prefix ! is used to indicate that the following invocation
    /// of an abstract or syntax-directed operation will never return an abrupt
    /// completion and that the resulting Completion Record's [[Value]] field
    /// should be used in place of the return value of the operation.
    fn E(&mut self, argument: RegisterId) -> RegisterId {
        // 1. Let val be OperationName().
        let val = argument;

        // 2. Assert: val is never an abrupt completion.
        let is_abrupt_completion = self.is_abrupt_completion(val);
        let assertion = self.negate(is_abrupt_completion);
        self.assert(assertion, "val is never an abrupt completion.");

        // 3. If val is a Completion Record, set val to val.[[Value]].
        self.if_then(
            |e| e.is_completion(val),
            |e| ControlFlow::Carry(e.record_get_slot(val, InternalSlot::Value)),
        )
        .else_then(|_| ControlFlow::Carry(val))
        .end()
        .unwrap()
    }

    fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("5.2.3.3 ReturnIfAbrupt");

        // 1. If argument is an abrupt completion, return argument.
        self.if_then(
            |e| e.is_abrupt_completion(argument),
            |_| ControlFlow::Return(Some(argument)),
        )
        // 2. Else if argument is a Completion Record, set argument to
        //    argument.[[Value]]
        .else_then(|e| {
            ControlFlow::Carry(
                e.if_then(
                    |e| e.is_completion(argument),
                    |e| ControlFlow::Carry(e.record_get_slot(argument, InternalSlot::Value)),
                )
                .else_then(|e| ControlFlow::Carry(argument))
                .end()
                .unwrap(),
            )
        })
        .end()
        .unwrap()
    }

    fn is_completion(&mut self, argument: RegisterId) -> RegisterId {
        // we determine a completion record with two criteria:
        // 1. is it a record
        // 2. does it have a [[Type]] internal slot
        self.if_then(
            |e| e.is_type(argument, ValueType::Record),
            |e| {
                // we have to use an if statement because `record_has_slot` doesn't work on
                // non-records

                // TODO(isa/specification): have `record_get_slot` return a sentinal `null`
                //     value? this would be better for preventing lots of branching, but also
                //     cause users to have to guard against all record get slots
                //
                //     perhaps a new instruction, record_try_get_slot could be introduced for
                //     this purpose

                ControlFlow::Carry(e.record_has_slot(argument, InternalSlot::Type))
            },
        )
        .else_then(|e| ControlFlow::Carry(e.make_bool(false)))
        .end()
        .unwrap()
    }

    fn is_normal_completion(&mut self, argument: RegisterId) -> RegisterId {
        self.if_then(
            |e| e.is_completion(argument),
            |e| {
                let Type = e.record_get_slot(argument, InternalSlot::Type);
                let normal = e.make_normal();
                ControlFlow::Carry(e.compare_equal(Type, normal))
            },
        )
        .else_then(|e| ControlFlow::Carry(e.make_bool(false)))
        .end()
        .unwrap()
    }

    fn is_abrupt_completion(&mut self, argument: RegisterId) -> RegisterId {
        // NOTE: we cannot simply negate the results of `is_normal_completion`, as that
        //     would cause us to consider regular values as abrupt completions.

        self.if_then(
            |e| e.is_completion(argument),
            |e| {
                let Type = e.record_get_slot(argument, InternalSlot::Type);
                let normal = e.make_normal();
                let is_normal = e.compare_equal(Type, normal);
                ControlFlow::Carry(e.negate(is_normal))
            },
        )
        .else_then(|e| ControlFlow::Carry(e.make_bool(false)))
        .end()
        .unwrap()
    }

    fn NormalCompletion(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("6.2.3.2 NormalCompletion");

        // 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]:
        //    empty }
        let completion = self.record_new();

        let normal = self.make_normal();
        self.record_set_slot(completion, InternalSlot::Type, normal);

        self.record_set_slot(completion, InternalSlot::Value, argument);

        let empty = self.make_empty();
        self.record_set_slot(completion, InternalSlot::Target, empty);

        completion
    }

    fn ThrowCompletion(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("6.2.3.3 ThrowCompletion");

        // 1. Return Completion { [[Type]]: throw, [[Value]]: argument, [[Target]]:
        //    empty }.
        let completion = self.record_new();

        let throw = self.make_throw();
        self.record_set_slot(completion, InternalSlot::Type, throw);

        self.record_set_slot(completion, InternalSlot::Value, argument);

        let empty = self.make_empty();
        self.record_set_slot(completion, InternalSlot::Target, empty);

        completion
    }
}
