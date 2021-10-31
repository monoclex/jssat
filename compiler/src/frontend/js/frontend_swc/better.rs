#![allow(non_snake_case)]

use crate::frontend::builder::{FnSignature, ProgramBuilder, RegisterId};
use crate::frontend::emitter::*;
use crate::isa::{InternalSlot, ValueType};

pub trait EmitterExt<'builder, const P: usize> {
    fn Q(&mut self, argument: RegisterId) -> RegisterId;
    fn E(&mut self, argument: RegisterId) -> RegisterId;
    fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId;

    fn is_completion(&mut self, argument: RegisterId) -> RegisterId;
    fn is_normal_completion(&mut self, argument: RegisterId) -> RegisterId;
    fn is_abrupt_completion(&mut self, argument: RegisterId) -> RegisterId;

    fn is_absent(&mut self, record: RegisterId, slot: InternalSlot) -> RegisterId;
    fn is_undefined(&mut self, argument: RegisterId) -> RegisterId;
    fn is_not_undefined(&mut self, argument: RegisterId) -> RegisterId;
    fn is_null(&mut self, argument: RegisterId) -> RegisterId;
    fn is_not_null(&mut self, argument: RegisterId) -> RegisterId;
    fn is_true(&mut self, argument: RegisterId) -> RegisterId;
    fn is_false(&mut self, argument: RegisterId) -> RegisterId;

    fn get_slot_or_default(
        &mut self,
        record: RegisterId,
        slot: InternalSlot,
        default: RegisterId,
    ) -> RegisterId;

    /// Calls either the first method or the second method.
    fn call_either_with_result<const PARAMS: usize>(
        &mut self,
        call_first: RegisterId,
        call_second: RegisterId,
        first: FnSignature<PARAMS>,
        second: FnSignature<PARAMS>,
        args: [RegisterId; PARAMS],
    ) -> RegisterId;
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
                .else_then(|_| ControlFlow::Carry(argument))
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
            |e| e.is_type_of(argument, ValueType::Record),
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

    fn is_absent(&mut self, record: RegisterId, slot: InternalSlot) -> RegisterId {
        let is_present = self.record_has_slot(record, slot);
        self.negate(is_present)
    }

    fn is_undefined(&mut self, argument: RegisterId) -> RegisterId {
        self.if_then(
            |e| e.is_type_of(argument, ValueType::Trivial),
            |e| {
                let undefined = e.make_undefined();
                let is_undef = e.compare_equal(argument, undefined);
                ControlFlow::Carry(is_undef)
            },
        )
        .else_then(|e| ControlFlow::Carry(e.make_bool(false)))
        .end()
        .unwrap()
    }

    fn is_not_undefined(&mut self, argument: RegisterId) -> RegisterId {
        let is_undefined = self.is_undefined(argument);
        self.negate(is_undefined)
    }

    fn is_null(&mut self, argument: RegisterId) -> RegisterId {
        self.if_then(
            |e| e.is_type_of(argument, ValueType::Trivial),
            |e| {
                let undefined = e.make_null();
                let is_undef = e.compare_equal(argument, undefined);
                ControlFlow::Carry(is_undef)
            },
        )
        .else_then(|e| ControlFlow::Carry(e.make_bool(false)))
        .end()
        .unwrap()
    }

    fn is_not_null(&mut self, argument: RegisterId) -> RegisterId {
        let is_null = self.is_null(argument);
        self.negate(is_null)
    }

    fn is_true(&mut self, argument: RegisterId) -> RegisterId {
        let r#true = self.make_bool(true);
        self.compare_equal(argument, r#true)
    }

    fn is_false(&mut self, argument: RegisterId) -> RegisterId {
        let r#false = self.make_bool(false);
        self.compare_equal(argument, r#false)
    }

    fn get_slot_or_default(
        &mut self,
        record: RegisterId,
        slot: InternalSlot,
        default: RegisterId,
    ) -> RegisterId {
        self.if_then(
            |e| e.record_has_slot(record, slot),
            |e| ControlFlow::Carry(e.record_get_slot(record, slot)),
        )
        .else_then(|_| ControlFlow::Carry(default))
        .end()
        .unwrap()
    }

    fn call_either_with_result<const PARAMS: usize>(
        &mut self,
        call_first: RegisterId,
        call_second: RegisterId,
        first: FnSignature<PARAMS>,
        second: FnSignature<PARAMS>,
        args: [RegisterId; PARAMS],
    ) -> RegisterId {
        let is_both = self.and(call_first, call_second);
        let shouldnt_be_both = self.negate(is_both);
        self.assert(shouldnt_be_both, "cannot wish to call both functions");

        self.if_then(
            |_| call_first,
            |e| ControlFlow::Carry(e.call_with_result(first, args)),
        )
        .else_then(|e| ControlFlow::Carry(e.call_with_result(second, args)))
        .end()
        .unwrap()
    }
}

// TODO(refactor): y'know, maybe handwriting all of the ecmascript specification
//     in rust isn't really that good of an idea... maybe in the future could
//     translate ECMAScript to a small little DSL that's easily convertible into
//     this rust code for us. that way, it's far more maintainable and smaller
//     in terms of code.
method_syntax::method_syntax! {
    fn Number_sameValue(x, y) {
        let mut e: Emitter<2> = e;
        e.comment("6.1.6.1.14 Number::sameValue ( x, y )");

        // TODO(correctness): implement number equality properly. we don't have
        //     numbers representing decimal at the moment, so i can't do any of
        //     this

        //# 1. If x is NaN and y is NaN, return true.
        //# 2. If x is +0픽 and y is -0픽, return false.
        //# 3. If x is -0픽 and y is +0픽, return false.
        //# 4. If x is the same Number value as y, return true.
        e.if_then(|e| e.compare_equal(x, y), |e| ControlFlow::Return(Some(e.make_bool(true))));

        //# 5. Return false.
        let r#false = e.make_bool(false);
        e.finish(Some(r#false))
    }

    fn BigInt_equal(x, y) {
        let mut e: Emitter<2> = e;
        e.comment("6.1.6.2.13 BigInt::equal ( x, y )");

        //# The abstract operation BigInt::equal takes arguments x (a BigInt) and y (a BigInt).
        let x_is_bigint = e.is_type_of(x, ValueType::BigNumber);
        e.assert(x_is_bigint, "argument x must be a BigInt");

        let y_is_bigint = e.is_type_of(y, ValueType::BigNumber);
        e.assert(y_is_bigint, "argument y must be a BigInt");

        //# It returns true if ℝ(x) = ℝ(y) and false otherwise.
        let are_equal = e.compare_equal(x, y);
        e.finish(Some(are_equal))
    }

    fn BigInt_sameValue(x, y) {
        let mut e: Emitter<2> = e;
        e.comment("6.1.6.2.14 BigInt::sameValue ( x, y )");

        //# 1. Return BigInt::equal(x, y).
        let result = e.call_with_result(self.BigInt_equal, [x, y]);
        e.finish(Some(result))
    }

    fn NormalCompletion(argument) {
        let mut e: Emitter<1> = e;
        e.comment("6.2.3.2 NormalCompletion");

        //# 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]: empty }
        let completion = e.record_new();

        let normal = e.make_normal();
        e.record_set_slot(completion, InternalSlot::Type, normal);

        e.record_set_slot(completion, InternalSlot::Value, argument);

        let empty = e.make_empty();
        e.record_set_slot(completion, InternalSlot::Target, empty);

        e.finish(Some(completion))
    }

    fn ThrowCompletion(argument) {
        let mut e: Emitter<1> = e;
        e.comment("6.2.3.3 ThrowCompletion");

        //# 1. Return Completion { [[Type]]: throw, [[Value]]: argument, [[Target]]:
        //#    empty }.
        let completion = e.record_new();

        let throw = e.make_throw();
        e.record_set_slot(completion, InternalSlot::Type, throw);

        e.record_set_slot(completion, InternalSlot::Value, argument);

        let empty = e.make_empty();
        e.record_set_slot(completion, InternalSlot::Target, empty);

        e.finish(Some(completion))
    }

    fn IsAccessorDescriptor(Desc) {
        let mut e: Emitter<1> = e;
        e.comment("6.2.5.1 IsAccessorDescriptor ( Desc )");

        //# 1. If Desc is undefined, return false.
        e.if_then(|e| e.is_undefined(Desc), |e| ControlFlow::Return(Some(e.make_bool(false))));

        //# 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
        e.if_then(|e| {
            let has_get = e.record_has_slot(Desc, InternalSlot::Get);
            let get_absent = e.negate(has_get);

            let has_set = e.record_has_slot(Desc, InternalSlot::Set);
            let set_absent = e.negate(has_set);

            let both_absent = e.and(get_absent, set_absent);
            both_absent
        }, |e| ControlFlow::Return(Some(e.make_bool(false))));

        //# 3. Return true.
        let r#true = e.make_bool(true);
        e.finish(Some(r#true))
    }

    fn IsDataDescriptor(Desc) {
        let mut e: Emitter<1> = e;
        e.comment("6.2.5.2 IsDataDescriptor ( Desc )");

        //# 1. If Desc is undefined, return false.
        e.if_then(|e| e.is_undefined(Desc), |e| ControlFlow::Return(Some(e.make_bool(false))));

        //# 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
        let desc_value_present = e.record_has_slot(Desc, InternalSlot::Value);
        let desc_value_absent = e.is_false(desc_value_present);

        let desc_writable_present = e.record_has_slot(Desc, InternalSlot::Writable);
        let desc_writable_absent = e.is_false(desc_writable_present);

        let both_absent = e.and(desc_value_absent, desc_writable_absent);
        e.if_then(|e| both_absent, |e| ControlFlow::Return(Some(e.make_bool(false))));

        //# 3. Return true.
        let r#true = e.make_bool(true);
        e.finish(Some(r#true))
    }

    fn IsGenericDescriptor(Desc) {
        let mut e: Emitter<1> = e;
        e.comment("6.2.5.3 IsGenericDescriptor ( Desc )");

        //# 1. If Desc is undefined, return false.
        e.if_then(|e| e.is_undefined(Desc), |e| ControlFlow::Return(Some(e.make_bool(false))));

        //# 2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, return true.
        let is_accessor = e.call_with_result(self.IsAccessorDescriptor, [Desc]);
        let accessor_is_false = e.is_false(is_accessor);

        let is_data = e.call_with_result(self.IsDataDescriptor, [Desc]);
        let data_is_false = e.is_false(is_data);

        e.if_then(|e| e.and(accessor_is_false, data_is_false), |e| ControlFlow::Return(Some(e.make_bool(true))));

        //# 3. Return false.
        let r#false = e.make_bool(false);
        e.finish(Some(r#false))
    }

    fn IsExtensible(O) {
        let mut e: Emitter<1> = e;
        e.comment("7.2.5 IsExtensible ( O )");

        //# 1. Assert: Type(O) is Object.
        // TODO(correctness): check that `O` is a JS *object*, not a JSSAT record.
        let kind = e.is_type_of(O, ValueType::Record);

        //# 2. Return ? O.[[IsExtensible]]().
        let is_extensible = e.record_get_slot(O, InternalSlot::IsExtensible);
        let result_q = e.call_virt_dynargs_with_result(is_extensible, vec![O]);
        let result = e.Q(result_q);

        e.finish(Some(result))
    }

    fn IsPropertyKey(argument) {
        let mut e: Emitter<1> = e;
        e.comment("7.2.7 IsPropertyKey ( argument )");

        //# 1. If Type(argument) is String, return true.
        let is_string = e.is_type_of(argument, ValueType::Bytes);
        //# 2. If Type(argument) is Symbol, return true.
        let is_symbol = e.is_type_of(argument, ValueType::Symbol);
        //# 3. Return false.

        let is_string_or_symbol = e.or(is_string, is_symbol);
        e.finish(Some(is_string_or_symbol))
    }

    fn SameValue(x, y) {
        let mut e: Emitter<2> = e;
        e.comment("7.2.10 SameValue ( x, y )");

        //# 1. If Type(x) is different from Type(y), return false.
        e.if_then(|e| e.is_type_as(x, y), |e| ControlFlow::Return(Some(e.make_bool(false))));

        //# 2. If Type(x) is Number or BigInt, then
        let is_num = e.is_type_of(x, ValueType::Number);
        let is_bignum = e.is_type_of(x, ValueType::BigNumber);
        e.if_then(|e| e.or(is_num, is_bignum), |e| {
            //# a. Return ! Type(x)::sameValue(x, y).
            let same_value_e = e.call_either_with_result(is_num, is_bignum, self.Number_sameValue, self.BigInt_sameValue, [x, y]);
            let same_value = e.E(same_value_e);

            ControlFlow::Return(Some(same_value))
        });

        //# 3. Return ! SameValueNonNumeric(x, y).
        let same_e = e.call_with_result(self.SameValueNonNumeric, [x, y]);
        let same = e.E(same_e);

        e.finish(Some(same))
    }

    fn SameValueNonNumeric(x, y) {
        let mut e: Emitter<2> = e;
        e.comment("7.2.12 SameValueNonNumeric ( x, y )");
        // The abstract operation SameValueNonNumeric ... returns a completion
        // record whose [[Type]] is normal and whose [[Value]] is a Boolean

        //# 1. Assert: Type(x) is not Number or BigInt.
        let is_num = e.is_type_of(x, ValueType::Number);
        let isnt_num = e.negate(is_num);

        let is_bigint = e.is_type_of(x, ValueType::BigNumber);
        let isnt_bigint = e.negate(is_bigint);

        let is_neither = e.and(isnt_num, isnt_bigint);
        e.assert(is_neither, "Type(x) is not Number or BigInt");

        //# 2. Assert: Type(x) is the same as Type(y).
        let same_type = e.is_type_as(x, y);
        e.assert(same_type, "Type(x) is the same as Type(y)");

        // In JSSAT, "Undefined" and "Null" have the same type - trivial. thus,
        // we perform explicit equality:
        //# 3. If Type(x) is Undefined, return true.
        e.if_then(|e| e.is_undefined(x), |e| ControlFlow::Return(Some(e.make_bool(true))));

        //# 4. If Type(x) is Null, return true.
        e.if_then(|e| e.is_null(x), |e| ControlFlow::Return(Some(e.make_bool(true))));

        //# 5. If Type(x) is String, then
        e.if_then(|e| e.is_type_of(x, ValueType::Bytes), |e| {
            //# a. If x and y are exactly the same sequence of code units (same
            //#    length and same code units at corresponding indices), return
            //#    true; otherwise, return false.
            ControlFlow::Return(Some(e.compare_equal(x, y)))
        });

        //# 6. If Type(x) is Boolean, then
        e.if_then(|e| e.is_type_of(x, ValueType::Boolean), |e| {
            //# a. If x and y are both true or both false, return true; otherwise,
            //#    return false.
            ControlFlow::Return(Some(e.compare_equal(x, y)))
        });

        //# 7. If Type(x) is Symbol, then
        e.if_then(|e| e.is_type_of(x, ValueType::Symbol), |e| {
            //# a. If x and y are both the same Symbol value, return true; otherwise,
            //#    return false.
            ControlFlow::Return(Some(e.compare_equal(x, y)))
        });

        //# 8. If x and y are the same Object value, return true. Otherwise,
        //#    return false.
        let same = e.compare_equal(x, y);

        e.finish(Some(same))
    }

    fn MakeBasicObject(internalSlotsList) {
        let mut e: Emitter<1> = e;
        // <https://tc39.es/ecma262/#sec-makebasicobject>
        e.comment("7.3.1 MakeBasicObject ( internalSlotsList )");

        //# 1. Assert: internalSlotsList is a List of internal slot names.
        //# 2. Let obj be a newly created object with an internal slot for each name in
        //#    internalSlotsList.
        let obj = e.record_new();

        //# 3. Set obj's essential internal methods to the default ordinary object
        //# definitions specified in 10.1.

        let fnptr = e.make_fnptr(self.OrdinaryDefineOwnProperty.id);
        e.record_set_slot(obj, InternalSlot::DefineOwnProperty, fnptr);

        let fnptr = e.make_fnptr(self.OrdinaryGetOwnProperty.id);
        e.record_set_slot(obj, InternalSlot::GetOwnProperty, fnptr);

        let fnptr = e.make_fnptr(self.OrdinaryIsExtensible.id);
        e.record_set_slot(obj, InternalSlot::IsExtensible, fnptr);

        let fnptr = e.make_fnptr(self.OrdinarySet.id);
        e.record_set_slot(obj, InternalSlot::Set, fnptr);

        let fnptr = e.make_fnptr(self.OrdinaryGetPrototypeOf.id);
        e.record_set_slot(obj, InternalSlot::GetPrototypeOf, fnptr);

        let fnptr = e.make_fnptr(self.OrdinaryHasProperty.id);
        e.record_set_slot(obj, InternalSlot::HasProperty, fnptr);

        let fnptr = e.make_fnptr(self.OrdinaryGet.id);
        e.record_set_slot(obj, InternalSlot::Get, fnptr);

        //# 4. Assert: If the caller will not be overriding both obj's
        //#    [[GetPrototypeOf]] and [[SetPrototypeOf]] essential internal
        //#    methods, then internalSlotsList contains [[Prototype]].
        //# 5. Assert: If the caller will not be overriding all of obj's
        //#    [[SetPrototypeOf]], [[IsExtensible]], and [[PreventExtensions]]
        //#    essential internal methods, then internalSlotsList contains
        //#    [[Extensible]].

        //# 6. If internalSlotsList contains [[Extensible]], set obj.[[Extensible]] to
        //# true.
        // if internalSlotsList.contains(&InternalSlot::Extensible) {
        //     let r#true = e.make_bool(true);
        //     e.record_set_slot(obj, InternalSlot::Extensible, r#true);
        // }

        //# 7. Return obj.
        e.finish(Some(obj))
    }

    fn CreateRealm() {
        let mut e: Emitter<0> = e;
        // <https://tc39.es/ecma262/#sec-createrealm>
        e.comment("9.2.1 CreateRealm");

        //# 1. Let realmRec be a new Realm Record.
        let realmRec = e.record_new();

        //# 2. Perform CreateIntrinsics(realmRec).
        e.call(self.CreateIntrinsics, [realmRec]);

        //# 3. Set realmRec.[[GlobalObject]] to undefined.
        let undefined = e.make_undefined();
        e.record_set_slot(realmRec, InternalSlot::GlobalObject, undefined);

        //# 4. Set realmRec.[[GlobalEnv]] to undefined.
        e.record_set_slot(realmRec, InternalSlot::GlobalEnv, undefined);

        //# 5. Set realmRec.[[TemplateMap]] to a new empty List.
        // TODO: add intrinsics for empty lists
        e.record_set_slot(realmRec, InternalSlot::TemplateMap, undefined);

        //# 6. Return realmRec.
        e.finish(Some(realmRec))
    }

    fn CreateIntrinsics(realmRec) {
        let mut e: Emitter<1> = e;

        // <https://tc39.es/ecma262/#sec-createintrinsics>
        e.comment("9.2.2 CreateIntrinsics");

        //# 1. Let intrinsics be a new Record.
        let intrinsics = e.record_new();

        //# 2. Set realmRec.[[Intrinsics]] to intrinsics.
        e.record_set_slot(realmRec, InternalSlot::Intrinsics, intrinsics);

        //# 3. Set fields of intrinsics with the values listed in Table 8.
        //#    The field names are the names listed in column one of the table.
        //#    The value of each field is a new object value fully and
        //#    recursively populated with property values as defined by the
        //#    specification of each object in clauses 19 through 28.
        //#    All object property values are newly created object values.
        //#    All values that are built-in function objects are created by
        //#    performing CreateBuiltinFunction(steps, length, name, slots,
        //#    realmRec, prototype) where steps is the definition of that
        //#    function provided by this specification, name is the initial
        //#    value of the function's name property, length is the initial
        //#    value of the function's length property, slots is a list of the
        //#    names, if any, of the function's specified internal slots, and
        //#    prototype is the specified value of the function's [[Prototype]]
        //#    internal slot.
        //#    The creation of the intrinsics and their properties must be
        //#    ordered to avoid any dependencies upon objects that have not yet
        //#    been created.
        // TODO(conformance): implement step #3 above

        //# 4. Perform
        //#    AddRestrictedFunctionProperties(intrinsics.[[%Function.prototype%]],
        //#    realmRec).
        // insert a value into the record to not trip up the symbolic execution engine
        // this will stay un until we properly populate intrinsics
        let undefined = e.make_undefined();
        let null = e.make_null();
        e.record_set_slot(intrinsics, InternalSlot::FunctionPrototype, undefined);
        e.record_set_slot(intrinsics, InternalSlot::ObjectPrototype, null);

        let F = e
            .record_get_slot(intrinsics, InternalSlot::FunctionPrototype);
        e.call(self.AddRestrictedFunctionProperties, [F, realmRec]);

        //# 5. Return intrinsics.
        e.finish(Some(intrinsics))
    }

    fn ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current) {
        let mut e: Emitter<5> = e;
        e.comment("10.1.6.3 ValidateAndApplyPropertyDescriptor ( O, P, extensible, Desc, current )");

        //# 1. Assert: If O is not undefined, then IsPropertyKey(P) is true.
        e.if_then(|e| e.is_not_undefined(O), |e| {
            let is_prop_key = e.call_with_result(self.IsPropertyKey, [P]);
            e.assert(is_prop_key, "If O is not undefined, then IsPropertyKey(P) is true");
            ControlFlow::Fallthrough
        });

        //# 2. If current is undefined, then
        e.if_then(|e| e.is_undefined(current), |e| {
            //# a. If extensible is false, return false.
            e.if_then(|e| e.is_false(extensible), |e| ControlFlow::Return(Some(e.make_bool(false))));

            //# b. Assert: extensible is true.
            let assertion = e.is_true(extensible);
            e.assert(assertion, "extensible is true");

            //# c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
            let is_gen_desc = e.call_with_result(self.IsGenericDescriptor, [Desc]);
            let is_data_desc = e.call_with_result(self.IsDataDescriptor, [Desc]);

            e.if_then(|e| e.or(is_gen_desc, is_data_desc), |e| {
                //# i. If O is not undefined, create an own data property named
                //#    P of object O whose [[Value]], [[Writable]],
                //#    [[Enumerable]], and [[Configurable]] attribute values are
                //#    described by Desc. If the value of an attribute field of
                //#    Desc is absent, the attribute of the newly created property
                //#    is set to its default value.
                e.if_then(|e| e.is_not_undefined(O), |e| {
                    let undefined = e.make_undefined();
                    let r#false = e.make_bool(false);

                    let value = e.get_slot_or_default(Desc, InternalSlot::Value, undefined);
                    let writable = e.get_slot_or_default(Desc, InternalSlot::Writable, undefined);
                    let enumerable = e.get_slot_or_default(Desc, InternalSlot::Enumerable, r#false);
                    let configurable = e.get_slot_or_default(Desc, InternalSlot::Configurable, r#false);

                    let data_property = e.record_new();
                    e.record_set_slot(data_property, InternalSlot::Value, value);
                    e.record_set_slot(data_property, InternalSlot::Writable, writable);
                    e.record_set_slot(data_property, InternalSlot::Enumerable, enumerable);
                    e.record_set_slot(data_property, InternalSlot::Configurable, configurable);
                    e.record_set_prop(O, P, data_property);
                    ControlFlow::Fallthrough
                });

                ControlFlow::Fallthrough
            })
            //# d. Else,
            .else_then(|e| {
                //# i. Assert: ! IsAccessorDescriptor(Desc) is true.
                let is_accessor_desc_e = e.call_with_result(self.IsAccessorDescriptor, [Desc]);
                let is_accessor_desc = e.E(is_accessor_desc_e);
                e.assert(is_accessor_desc, "! IsAccessorDescriptor(Desc) is true");

                //# ii. If O is not undefined, create an own accessor property
                //#     named P of object O whose [[Get]], [[Set]], [[Enumerable]],
                //#     and [[Configurable]] attribute values are described by
                //#     Desc. If the value of an attribute field of Desc is absent,
                //#     the attribute of the newly created property is set to its
                //#     default value.
                e.if_then(|e| e.is_not_undefined(O), |e| {
                    let undefined = e.make_undefined();
                    let r#false = e.make_bool(false);

                    let get = e.get_slot_or_default(Desc, InternalSlot::Get, undefined);
                    let set = e.get_slot_or_default(Desc, InternalSlot::Set, undefined);
                    let enumerable = e.get_slot_or_default(Desc, InternalSlot::Enumerable, r#false);
                    let configurable = e.get_slot_or_default(Desc, InternalSlot::Configurable, r#false);

                    let data_property = e.record_new();
                    e.record_set_slot(data_property, InternalSlot::Get, get);
                    e.record_set_slot(data_property, InternalSlot::Set, set);
                    e.record_set_slot(data_property, InternalSlot::Enumerable, enumerable);
                    e.record_set_slot(data_property, InternalSlot::Configurable, configurable);
                    e.record_set_prop(O, P, data_property);
                    ControlFlow::Fallthrough
                });

                //# e. Return true.
                ControlFlow::Return(Some(e.make_bool(true)))
            });

            ControlFlow::Return(Some(e.make_bool(true)))
        });

        //# 3. If every field in Desc is absent, return true.
        let get_absent = e.is_absent(Desc, InternalSlot::Get);
        let set_absent = e.is_absent(Desc, InternalSlot::Set);
        let value_absent = e.is_absent(Desc, InternalSlot::Value);
        let writable_absent = e.is_absent(Desc, InternalSlot::Writable);
        let enumerable_absent = e.is_absent(Desc, InternalSlot::Enumerable);
        let configurable_absent = e.is_absent(Desc, InternalSlot::Configurable);

        let tmp1 = e.and(get_absent, set_absent);
        let tmp2 = e.and(value_absent, writable_absent);
        let tmp3 = e.and(enumerable_absent, configurable_absent);
        let tmp4 = e.and(tmp1, tmp2);
        let tmp5 = e.and(tmp4, tmp3);

        e.if_then(|_| tmp5, |e| ControlFlow::Return(Some(e.make_bool(true))));

        //# 4. If current.[[Configurable]] is false, then
        let configurable = e.record_get_slot(current, InternalSlot::Configurable);
        e.if_then(|e| e.is_false(configurable), |e| {
            //# a. If Desc.[[Configurable]] is present and its value is true,
            //#    return false.
            let is_present = e.record_has_slot(Desc, InternalSlot::Configurable);
            e.if_then(|e| is_present, |e| {
                let configurable = e.record_get_slot(Desc, InternalSlot::Configurable);
                e.if_then(|e| e.is_true(configurable), |e| ControlFlow::Return(Some(e.make_bool(true))));
                ControlFlow::Fallthrough
            });

            //# b. If Desc.[[Enumerable]] is present and ! SameValue(Desc.[[
            //#    Enumerable]], current.[[Enumerable]]) is false, return false.
            e.if_then(|e| e.record_has_slot(Desc, InternalSlot::Enumerable), |e| {
                let desc_enumerable = e.record_get_slot(Desc, InternalSlot::Enumerable);
                let curr_enumerable = e.record_get_slot(current, InternalSlot::Enumerable);

                let same_e = e.call_with_result(self.SameValue, [desc_enumerable, curr_enumerable]);
                let same = e.E(same_e);

                e.if_then(|e| e.negate(same), |e| ControlFlow::Return(Some(e.make_bool(false))));

                ControlFlow::Fallthrough
            });

            ControlFlow::Fallthrough
        });

        //# 5. If ! IsGenericDescriptor(Desc) is true, then
        let is_gen_desc_e = e.call_with_result(self.IsGenericDescriptor, [Desc]);
        let is_gen_desc = e.E(is_gen_desc_e);
        e.if_then(|e| e.negate(is_gen_desc), |e| {
            //# a. NOTE: No further validation is required.
            ControlFlow::Fallthrough
        })
        //# 6. Else if ! SameValue(! IsDataDescriptor(current), ! IsDataDescriptor(Desc)) is false, then
        .else_then(|e| {
            let idd_current_e = e.call_with_result(self.IsDataDescriptor, [current]);
            let idd_current = e.E(idd_current_e);

            let idd_desc_e = e.call_with_result(self.IsDataDescriptor, [Desc]);
            let idd_desc = e.E(idd_desc_e);

            let same_e = e.call_with_result(self.SameValue, [idd_current, idd_desc]);
            let same = e.E(same_e);

            e.if_then(|_| same, |e| {
                //# a. If current.[[Configurable]] is false, return false.
                let config = e.record_get_slot(current, InternalSlot::Configurable);
                e.if_then(|e| e.is_false(config), |e| ControlFlow::Return(Some(e.make_bool(false))));

                //# b. If IsDataDescriptor(current) is true, then
                let idd_curr = e.call_with_result(self.IsDataDescriptor, [current]);
                e.if_then(|e| e.is_true(idd_curr), |e| {
                    //# i. If O is not undefined, convert the property named P of object O from a data property to an
                    //# accessor property. Preserve the existing values of the converted property's [[Configurable]] and
                    //# [[Enumerable]] attributes and set the rest of the property's attributes to their default values.
                    ControlFlow::Fallthrough
                })
                //# c. Else,
                .else_then(|e| {
                    //# i. If O is not undefined, convert the property named P of object O from an accessor property to a
                    //# data property. Preserve the existing values of the converted property's [[Configurable]] and
                    //# [[Enumerable]] attributes and set the rest of the property's attributes to their default values.
                    ControlFlow::Fallthrough
                });

                ControlFlow::Fallthrough
            })
            //# 7. Else if IsDataDescriptor(current) and IsDataDescriptor(Desc) are both true, then
            .else_then(|e| {
                let idd_current = e.call_with_result(self.IsDataDescriptor, [current]);
                let idd_desc = e.call_with_result(self.IsDataDescriptor, [Desc]);
                let cur_idd = e.is_true(idd_current);
                let desc_idd = e.is_true(idd_desc);

                e.if_then(|e| e.and(cur_idd, desc_idd), |e| {
                    //# a. If current.[[Configurable]] is false and current.[[Writable]] is false, then
                    //# i. If Desc.[[Writable]] is present and Desc.[[Writable]] is true, return false.
                    //# ii. If Desc.[[Value]] is present and SameValue(Desc.[[Value]], current.[[Value]]) is false, return false.
                    //# iii. Return true.
                    ControlFlow::Fallthrough
                })
                //# 8. Else,
                .else_then(|e| {
                    //# a. Assert: ! IsAccessorDescriptor(current) and ! IsAccessorDescriptor(Desc) are both true.
                    //# b. If current.[[Configurable]] is false, then
                    //# i. If Desc.[[Set]] is present and SameValue(Desc.[[Set]], current.[[Set]]) is false, return false.
                    //# ii. If Desc.[[Get]] is present and SameValue(Desc.[[Get]], current.[[Get]]) is false, return false.
                    //# iii. Return true.
                    ControlFlow::Fallthrough
                });

                ControlFlow::Fallthrough
            });

            ControlFlow::Fallthrough
        });

        //# 9. If O is not undefined, then
        e.if_then(|e| e.is_not_undefined(O), |e| {
            //# a. For each field of Desc that is present, set the corresponding
            //#    attribute of the property named P of object O to the value of
            //#    the field.
            e.record_set_prop(O, P, Desc);
            ControlFlow::Fallthrough
        });

        //# 10. Return true.
        let r#true = e.make_bool(true);
        e.finish(Some(r#true))
    }

    fn OrdinaryDefineOwnProperty(O, P, Desc) {
        let mut e: Emitter<3> = e;
        e.comment("10.1.6.1 OrdinaryDefineOwnProperty ( O, P, Desc )");

        //# 1. Let current be ? O.[[GetOwnProperty]](P).
        let get_own_property = e.record_get_slot(O, InternalSlot::GetOwnProperty);
        let current_q = e.call_virt_dynargs_with_result(get_own_property, vec![O, P]);
        let current = e.Q(current_q);

        //# 2. Let extensible be ? IsExtensible(O).
        let extensible_q = e.call_with_result(self.IsExtensible, [O]);
        let extensible = e.Q(extensible_q);

        //# 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
        let result = e.call_with_result(self.ValidateAndApplyPropertyDescriptor, [O, P, extensible, Desc, current]);
        e.finish(Some(result))
    }

    fn AddRestrictedFunctionProperties(F, realm) {
        let mut e: Emitter<2> = e;

        // <https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties>
        e.comment("10.2.4 AddRestrictedFunctionProperties");

        // TODO(conformance): implement the rest of this method
        e.finish(None)
    }
}
