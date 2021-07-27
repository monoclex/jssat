//! <https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots>

use crate::{
    frontend::builder::{FnSignature, ProgramBuilder, RegisterId},
    isa::InternalSlot,
};

use super::{
    abstract_operations::{BlockBuilderExt, EmitterExt},
    Emitter,
};

#[allow(non_snake_case)]
pub fn create_ordinary_internal_methods(builder: &mut ProgramBuilder) -> OrdinaryInternalMethods {
    let ValidateAndApplyPropertyDescriptor =
        OrdinaryInternalMethods::make_ValidateAndApplyPropertyDescriptor(builder);
    let OrdinaryDefineOwnProperty = OrdinaryInternalMethods::make_OrdinaryDefineOwnProperty(
        builder,
        ValidateAndApplyPropertyDescriptor,
    );

    OrdinaryInternalMethods {
        OrdinaryGetPrototypeOf: OrdinaryInternalMethods::make_OrdinaryGetPrototypeOf(builder),
        OrdinarySetPrototypeOf: OrdinaryInternalMethods::make_OrdinarySetPrototypeOf(builder),
        OrdinaryDefineOwnProperty,
        ValidateAndApplyPropertyDescriptor,
    }
}

#[allow(non_snake_case)]
pub struct OrdinaryInternalMethods {
    pub OrdinaryGetPrototypeOf: FnSignature<1>,
    pub OrdinarySetPrototypeOf: FnSignature<2>,
    pub OrdinaryDefineOwnProperty: FnSignature<3>,
    pub ValidateAndApplyPropertyDescriptor: FnSignature<5>,
}

#[allow(non_snake_case)]
impl OrdinaryInternalMethods {
    /// <https://tc39.es/ecma262/#sec-ordinarygetprototypeof>
    fn make_OrdinaryGetPrototypeOf(builder: &mut ProgramBuilder) -> FnSignature<1> {
        let (mut f, [O]) = builder.start_function();
        let mut b = f.start_block_main();

        //# 1. Return O.[[Prototype]]
        let prototype = b.record_get_slot(O, InternalSlot::Prototype);
        f.end_block(b.ret(Some(prototype)));
        builder.end_function(f)
    }

    /// <https://tc39.es/ecma262/#sec-ordinarysetprototypeof>
    fn make_OrdinarySetPrototypeOf(builder: &mut ProgramBuilder) -> FnSignature<2> {
        let (mut f, [O, V]) = builder.start_function();
        let mut b = f.start_block_main();

        //# 1. Assert: Either Type(V) is Object or Type(V) is Null.
        //# 2. Let current be O.[[Prototype]].
        let current = b.record_get_slot(O, InternalSlot::Prototype);

        //# 3. If SameValue(V, current) is true, return true.
        //# 4. Let extensible be O.[[Extensible]].
        //# 5. If extensible is false, return false.
        //# 6. Let p be V.
        //# 7. Let done be false.
        //# 8. Repeat, while done is false,
        //# a. If p is null, set done to true.
        //# b. Else if SameValue(p, O) is true, return false.
        //# c. Else,
        //# i. If p.[[GetPrototypeOf]] is not the ordinary object internal method defined in 10.1.1, set done to true.
        //# ii. Else, set p to p.[[Prototype]].
        //# 9. Set O.[[Prototype]] to V.
        //# 10. Return true.
        let r#true = b.make_bool(true);
        f.end_block(b.ret(Some(r#true)));
        builder.end_function(f)
    }

    /// <https://tc39.es/ecma262/#sec-ordinarydefineownproperty>
    fn make_OrdinaryDefineOwnProperty(
        builder: &mut ProgramBuilder,
        ValidateAndApplyPropertyDescriptor: FnSignature<5>,
    ) -> FnSignature<3> {
        let (mut f, [O, P, Desc]) = builder.start_function();
        let mut b = Emitter::new(builder, f);

        //# 1. Let current be ? O.[[GetOwnProperty]](P).
        let get_own_property = b.record_get_slot(O, InternalSlot::GetOwnProperty);
        let current = b.call_virt_with_result(get_own_property, [O, P]);

        //# 2. Let extensible be ? IsExtensible(O).
        let extensible = b.Q(|b| b.IsExtensible(O));

        //# 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
        let result = b.call_with_result(
            ValidateAndApplyPropertyDescriptor,
            [O, P, extensible, Desc, current],
        );
        b.finish(|b| b.ret(Some(result)))
    }

    /// <https://tc39.es/ecma262/#sec-validateandapplypropertydescriptor>
    fn make_ValidateAndApplyPropertyDescriptor(builder: &mut ProgramBuilder) -> FnSignature<5> {
        let (mut f, [O, P, extensible, Desc, current]) = builder.start_function();
        let mut b = Emitter::new(builder, f);

        let r#true = b.make_bool(true);
        let r#false = b.make_bool(false);
        let undefined = b.make_undefined();

        //# 1. Assert: If O is not undefined, then IsPropertyKey(P) is true.
        //# 2. If current is undefined, then
        let is_undefined = b.compare_equal(current, undefined);
        b.if_then_end(is_undefined, |b| {
            //# a. If extensible is false, return false.
            let is_false = b.compare_equal(extensible, r#false);
            b.if_then_end(is_false, |_| |b| b.ret(Some(r#false)));

            //# b. Assert: extensible is true.
            //# c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
            let is_generic = b.IsGenericDescriptor(Desc);
            let is_data = b.IsDataDescriptor(Desc);
            let is_generic_or_data = b.or(is_generic, is_data);

            b.if_then_else(
                is_generic_or_data,
                |b| {
                    //# i. If O is not undefined, create an own data property
                    //#    named P of object O whose [[Value]], [[Writable]],
                    //#    [[Enumerable]], and [[Configurable]] attribute values
                    //#    are described by Desc. If the value of an attribute
                    //#    field of Desc is absent, the attribute of the newly
                    //#    created property is set to its default value.
                    let is_undefined = b.compare_equal(O, undefined);
                    let is_not_undefined = b.negate(is_undefined);
                    b.if_then(is_not_undefined, |b| {
                        let value = b.slot_or_default(Desc, InternalSlot::Value, undefined);
                        let writable = b.slot_or_default(Desc, InternalSlot::Writable, undefined);
                        let enumerable = b.slot_or_default(Desc, InternalSlot::Enumerable, r#false);
                        let configurable =
                            b.slot_or_default(Desc, InternalSlot::Configurable, r#false);

                        let data_property = b.record_new();
                        b.record_set_slot(data_property, InternalSlot::Value, value);
                        b.record_set_slot(data_property, InternalSlot::Writable, writable);
                        b.record_set_slot(data_property, InternalSlot::Enumerable, enumerable);
                        b.record_set_slot(data_property, InternalSlot::Configurable, configurable);
                        b.record_set_prop(O, P, data_property);
                    });
                },
                //# d. Else,
                |b| {
                    //# i. Assert: ! IsAccessorDescriptor(Desc) is true.
                    //# ii. If O is not undefined, create an own accessor
                    //#     property named P of object O whose [[Get]], [[Set]],
                    //#     [[Enumerable]], and [[Configurable]] attribute
                    //#     values are described by Desc. If the value of an
                    //#     attribute field of Desc is absent, the attribute of
                    //#     the newly created property is set to its default value.
                    let is_undefined = b.compare_equal(O, undefined);
                    let is_not_undefined = b.negate(is_undefined);
                    b.if_then(is_not_undefined, |b| {
                        let get = b.slot_or_default(Desc, InternalSlot::Get, undefined);
                        let set = b.slot_or_default(Desc, InternalSlot::Set, undefined);
                        let enumerable = b.slot_or_default(Desc, InternalSlot::Enumerable, r#false);
                        let configurable =
                            b.slot_or_default(Desc, InternalSlot::Configurable, r#false);

                        let data_property = b.record_new();
                        b.record_set_slot(data_property, InternalSlot::Get, get);
                        b.record_set_slot(data_property, InternalSlot::Set, set);
                        b.record_set_slot(data_property, InternalSlot::Enumerable, enumerable);
                        b.record_set_slot(data_property, InternalSlot::Configurable, configurable);
                        b.record_set_prop(O, P, data_property);
                    });
                },
            );

            //# e. Return true.
            |b| b.ret(Some(r#true))
        });

        //# 3. If every field in Desc is absent, return true.
        // TODO: instruction to check if record is empty
        let TODO_INSTRUCTION_RECORD_IS_EMPTY = 1;

        //# 4. If current.[[Configurable]] is false, then
        let configurable = b.record_get_slot(current, InternalSlot::Configurable);
        let is_false = b.compare_equal(configurable, r#false);
        b.if_then(is_false, |b| {
            //# a. If Desc.[[Configurable]] is present and its value is true, return false.
            let has_configurable = b.record_has_slot(Desc, InternalSlot::Configurable);
            b.if_then(has_configurable, |b| {
                let value = b.record_get_slot(Desc, InternalSlot::Configurable);
                let value_is_true = b.compare_equal(value, r#true);
                b.if_then_end(value_is_true, |_| |b| b.ret(Some(r#false)));
            })

            //# b. If Desc.[[Enumerable]] is present and ! SameValue(Desc.[[Enumerable]], current.[[Enumerable]]) is false, return false.
        });

        //# 5. If ! IsGenericDescriptor(Desc) is true, then
        let is_generic_descriptor = b.IsGenericDescriptor(Desc);
        b.if_then_else(
            is_generic_descriptor,
            |_| {

                //# a. NOTE: No further validation is required.
            },
            |b| {
                let TODO_ASSERTION_INSTRUCTION = 1;

                //# 6. Else if ! SameValue(! IsDataDescriptor(current), ! IsDataDescriptor(Desc)) is false, then
                //# a. If current.[[Configurable]] is false, return false.
                //# b. If IsDataDescriptor(current) is true, then
                //# i. If O is not undefined, convert the property named P of object O from a data property to an accessor property. Preserve the existing values of the converted property's [[Configurable]] and [[Enumerable]] attributes and set the rest of the property's attributes to their default values.
                //# c. Else,
                //# i. If O is not undefined, convert the property named P of object O from an accessor property to a data property. Preserve the existing values of the converted property's [[Configurable]] and [[Enumerable]] attributes and set the rest of the property's attributes to their default values.
                //# 7. Else if IsDataDescriptor(current) and IsDataDescriptor(Desc) are both true, then
                //# a. If current.[[Configurable]] is false and current.[[Writable]] is false, then
                //# i. If Desc.[[Writable]] is present and Desc.[[Writable]] is true, return false.
                //# ii. If Desc.[[Value]] is present and SameValue(Desc.[[Value]], current.[[Value]]) is false, return false.
                //# iii. Return true.
                //# 8. Else,
                //# a. Assert: ! IsAccessorDescriptor(current) and ! IsAccessorDescriptor(Desc) are both true.
                //# b. If current.[[Configurable]] is false, then
                //# i. If Desc.[[Set]] is present and SameValue(Desc.[[Set]], current.[[Set]]) is false, return false.
                //# ii. If Desc.[[Get]] is present and SameValue(Desc.[[Get]], current.[[Get]]) is false, return false.
                //# iii. Return true.
            },
        );

        //# 9. If O is not undefined, then
        let is_undefined = b.compare_equal(O, undefined);
        let is_not_undefined = b.negate(is_undefined);
        b.if_then(is_not_undefined, |b| {
            //# a. For each field of Desc that is present, set the corresponding
            //#    attribute of the property named P of object O to the value of
            //#    the field.
        });

        //# 10. Return true.
        b.finish(|b| b.ret(Some(r#true)))
    }
}
