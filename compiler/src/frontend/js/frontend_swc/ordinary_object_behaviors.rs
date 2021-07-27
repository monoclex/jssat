//! <https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots>

use crate::{
    frontend::builder::{FnSignature, ProgramBuilder, RegisterId},
    isa::InternalSlot,
};

use super::{
    abstract_operations::{BlockBuilderExt, EmitterExt},
    descriptors::Descriptors,
    Emitter,
};

#[allow(non_snake_case)]
pub fn create_ordinary_internal_methods(
    builder: &mut ProgramBuilder,
    descriptors: Descriptors,
) -> OrdinaryInternalMethods {
    let ValidateAndApplyPropertyDescriptor =
        OrdinaryInternalMethods::make_ValidateAndApplyPropertyDescriptor(builder, descriptors);
    let OrdinaryDefineOwnProperty = OrdinaryInternalMethods::make_OrdinaryDefineOwnProperty(
        builder,
        ValidateAndApplyPropertyDescriptor,
    );
    let CreateDataProperty = OrdinaryInternalMethods::make_CreateDataProperty(builder);
    let OrdinarySetWithOwnDescriptor = OrdinaryInternalMethods::make_OrdinarySetWithOwnDescriptor(
        builder,
        CreateDataProperty,
        descriptors,
    );

    OrdinaryInternalMethods {
        OrdinaryGetPrototypeOf: OrdinaryInternalMethods::make_OrdinaryGetPrototypeOf(builder),
        OrdinarySetPrototypeOf: OrdinaryInternalMethods::make_OrdinarySetPrototypeOf(builder),
        OrdinaryDefineOwnProperty,
        ValidateAndApplyPropertyDescriptor,
        OrdinaryGetOwnProperty: OrdinaryInternalMethods::make_OrdinaryGetOwnProperty(
            builder,
            descriptors,
        ),
        OrdinaryIsExtensible: OrdinaryInternalMethods::make_OrdinaryIsExtensible(builder),
        CreateDataProperty,
        OrdinarySet: OrdinaryInternalMethods::make_OrdinarySet(
            builder,
            OrdinarySetWithOwnDescriptor,
        ),
        OrdinarySetWithOwnDescriptor,
        OrdinaryHasProperty: OrdinaryInternalMethods::make_OrdinaryHasProperty(builder),
    }
}

#[allow(non_snake_case)]
#[derive(Clone, Copy)]
pub struct OrdinaryInternalMethods {
    pub OrdinaryGetPrototypeOf: FnSignature<1>,
    pub OrdinarySetPrototypeOf: FnSignature<2>,
    pub OrdinaryDefineOwnProperty: FnSignature<3>,
    pub ValidateAndApplyPropertyDescriptor: FnSignature<5>,
    pub OrdinaryGetOwnProperty: FnSignature<2>,
    pub OrdinaryIsExtensible: FnSignature<1>,
    pub OrdinarySetWithOwnDescriptor: FnSignature<5>,
    pub OrdinarySet: FnSignature<4>,
    pub CreateDataProperty: FnSignature<3>,
    pub OrdinaryHasProperty: FnSignature<2>,
}

#[allow(non_snake_case)]
impl OrdinaryInternalMethods {
    /// <https://tc39.es/ecma262/#sec-ordinaryhasproperty>
    fn make_OrdinaryHasProperty(builder: &mut ProgramBuilder) -> FnSignature<2> {
        let (f, [O, P]) = builder.start_function();
        let mut b = Emitter::new(builder, f);

        let r#true = b.make_bool(true);
        let r#false = b.make_bool(false);

        //# 1. Assert: IsPropertyKey(P) is true.
        //# 2. Let hasOwn be ? O.[[GetOwnProperty]](P).
        // TODO: handle fallibility
        let get_own_prop = b.record_get_slot(O, InternalSlot::GetOwnProperty);
        let hasOwn = b.call_virt_with_result(get_own_prop, [O, P]);

        //# 3. If hasOwn is not undefined, return true.
        let undef = b.make_undefined();
        let is_undef = b.compare_equal(hasOwn, undef);
        let is_not_undef = b.negate(is_undef);
        b.if_then_end(is_not_undef, |_| |b| b.ret(Some(r#true)));

        //# 4. Let parent be ? O.[[GetPrototypeOf]]().
        // TODO: falliblity
        let get_prot_of = b.record_get_slot(O, InternalSlot::GetPrototypeOf);
        let parent = b.call_virt_with_result(get_prot_of, [O]);

        //# 5. If parent is not null, then
        let null = b.make_null();
        let is_null = b.compare_equal(parent, null);
        let not_null = b.negate(is_null);
        b.if_then_end(not_null, |b| {
            //# a. Return ? parent.[[HasProperty]](P).
            let has_prop = b.record_get_slot(parent, InternalSlot::HasProperty);
            let result = b.call_virt_with_result(has_prop, [parent, P]);
            move |b| b.ret(Some(result))
        });

        //# 6. Return false.
        b.finish(|b| b.ret(Some(r#false)))
    }

    /// <https://tc39.es/ecma262/#sec-createdataproperty>
    fn make_CreateDataProperty(builder: &mut ProgramBuilder) -> FnSignature<3> {
        let (f, [O, P, V]) = builder.start_function();
        let mut b = Emitter::new(builder, f);

        //# 1. Assert: Type(O) is Object.
        //# 2. Assert: IsPropertyKey(P) is true.
        //# 3. Let newDesc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true }.
        let newDesc = b.record_new();
        let r#true = b.make_bool(true);
        b.record_set_slot(newDesc, InternalSlot::Value, V);
        b.record_set_slot(newDesc, InternalSlot::Writable, r#true);
        b.record_set_slot(newDesc, InternalSlot::Enumerable, r#true);
        b.record_set_slot(newDesc, InternalSlot::Configurable, r#true);

        //# 4. Return ? O.[[DefineOwnProperty]](P, newDesc).
        let define_own_prop = b.record_get_slot(O, InternalSlot::DefineOwnProperty);
        let result = b.call_virt_with_result(define_own_prop, [O, P, newDesc]);

        // TODO: care about fallibleness
        b.finish(|b| b.ret(Some(result)))
    }

    /// <https://tc39.es/ecma262/#sec-ordinaryset>
    fn make_OrdinarySet(
        builder: &mut ProgramBuilder,
        OrdinarySetWithOwnDescriptor: FnSignature<5>,
    ) -> FnSignature<4> {
        let (f, [O, P, V, Receiver]) = builder.start_function();
        let mut b = Emitter::new(builder, f);

        //# 1. Assert: IsPropertyKey(P) is true.
        //# 2. Let ownDesc be ? O.[[GetOwnProperty]](P).
        let get_own_prop = b.record_get_slot(O, InternalSlot::GetOwnProperty);
        // TODO: use `?` (atm no fallible GetOwnProperty)
        let ownDesc = b.call_virt_with_result(get_own_prop, [O, P]);

        //# 3. Return OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
        let result = b.call_with_result(OrdinarySetWithOwnDescriptor, [O, P, V, Receiver, ownDesc]);

        b.finish(|b| b.ret(Some(result)))
    }

    /// <https://tc39.es/ecma262/#sec-ordinarygetprototypeof>
    fn make_OrdinaryGetPrototypeOf(builder: &mut ProgramBuilder) -> FnSignature<1> {
        let (mut f, [O]) = builder.start_function();
        let mut b = f.start_block_main();
        b.comment("OrdinaryGetPrototypeOf");

        //# 1. Return O.[[Prototype]]
        let prototype = b.record_get_slot(O, InternalSlot::Prototype);
        f.end_block(b.ret(Some(prototype)));
        builder.end_function(f)
    }

    /// <https://tc39.es/ecma262/#sec-ordinarysetprototypeof>
    fn make_OrdinarySetPrototypeOf(builder: &mut ProgramBuilder) -> FnSignature<2> {
        let (mut f, [O, V]) = builder.start_function();
        let mut b = f.start_block_main();
        b.comment("OrdinarySetPrototypeOf");

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
        b.comment("OrdinaryDefineOwnProperty");

        //# 1. Let current be ? O.[[GetOwnProperty]](P).
        let get_own_property = b.record_get_slot(O, InternalSlot::GetOwnProperty);
        let current = b.call_virt_with_result(get_own_property, [O, P]);

        //# 2. Let extensible be ? IsExtensible(O).
        // TODO: do we really need the `?`? this doesn't return a result atm
        // let extensible = b.Q(|b| b.IsExtensible(O));
        let extensible = b.IsExtensible(O);

        //# 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
        let result = b.call_with_result(
            ValidateAndApplyPropertyDescriptor,
            [O, P, extensible, Desc, current],
        );
        b.finish(|b| b.ret(Some(result)))
    }

    /// <https://tc39.es/ecma262/#sec-validateandapplypropertydescriptor>
    fn make_ValidateAndApplyPropertyDescriptor(
        builder: &mut ProgramBuilder,
        descriptors: Descriptors,
    ) -> FnSignature<5> {
        let (f, [O, P, extensible, Desc, current]) = builder.start_function();
        let mut b = Emitter::new(builder, f);
        b.comment("ValidateAndApplyPropertyDescriptor");

        let r#true = b.make_bool(true);
        let r#false = b.make_bool(false);
        let undefined = b.make_undefined();

        //# 1. Assert: If O is not undefined, then IsPropertyKey(P) is true.
        //# 2. If current is undefined, then
        let is_undefined = b.compare_equal(current, undefined);
        b.if_then_end(is_undefined, |b| {
            //# a. If extensible is false, return false.
            let is_false = b.compare_equal(extensible, r#false);
            b.if_then_end(is_false, |b| |b| b.ret(Some(r#false)));

            //# b. Assert: extensible is true.
            //# c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
            let is_generic = b.call_with_result(descriptors.IsGenericDescriptor, [Desc]);
            let is_data = b.call_with_result(descriptors.IsDataDescriptor, [Desc]);
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
                b.if_then_end(value_is_true, |b| |b| b.ret(Some(r#false)));
            })

            //# b. If Desc.[[Enumerable]] is present and ! SameValue(Desc.[[Enumerable]], current.[[Enumerable]]) is false, return false.
        });

        //# 5. If ! IsGenericDescriptor(Desc) is true, then
        let is_generic_descriptor = b.call_with_result(descriptors.IsGenericDescriptor, [Desc]);
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

    /// <https://tc39.es/ecma262/#sec-ordinarysetwithowndescriptor>
    fn make_OrdinarySetWithOwnDescriptor(
        builder: &mut ProgramBuilder,
        CreateDataProperty: FnSignature<3>,
        descriptors: Descriptors,
    ) -> FnSignature<5> {
        let (f, [O, P, V, Receiver, ownDesc]) = builder.start_function();
        let mut b = Emitter::new(builder, f);

        let r#true = b.make_bool(true);
        let r#false = b.make_bool(false);

        //# 1. Assert: IsPropertyKey(P) is true.
        //# 2. If ownDesc is undefined, then
        let undef = b.make_undefined();
        let is_undef = b.compare_equal(ownDesc, undef);
        let ownDesc = b.if_then_x_else_y(
            is_undef,
            |b| {
                //# a. Let parent be ? O.[[GetPrototypeOf]]().
                let get_prototype_of = b.record_get_slot(O, InternalSlot::GetPrototypeOf);
                // TODO: no fallible `GetPrototypeOf`s yet
                let parent = b.call_virt_with_result(get_prototype_of, [O]);

                //# b. If parent is not null, then
                let null = b.make_null();
                let is_null = b.compare_equal(parent, null);
                let is_not_null = b.negate(is_null);

                b.if_then_end(is_not_null, |b| {
                    //# i. Return ? parent.[[Set]](P, V, Receiver).
                    let set = b.record_get_slot(parent, InternalSlot::Set);
                    let result = b.call_virt_with_result(set, [parent, P, V, Receiver]);
                    // TODO: no fallible Set
                    move |b| b.ret(Some(result))
                });

                //# c. Else,
                //# i. Set ownDesc to the PropertyDescriptor { [[Value]]:
                //#    undefined, [[Writable]]: true, [[Enumerable]]: true,
                //#    [[Configurable]]: true }.
                let ownDesc = b.record_new();
                b.record_set_slot(ownDesc, InternalSlot::Value, undef);
                b.record_set_slot(ownDesc, InternalSlot::Writable, r#true);
                b.record_set_slot(ownDesc, InternalSlot::Enumerable, r#true);
                b.record_set_slot(ownDesc, InternalSlot::Configurable, r#true);
                ownDesc
            },
            |_| ownDesc,
        );

        //# 3. If IsDataDescriptor(ownDesc) is true, then
        let is_data = b.call_with_result(descriptors.IsDataDescriptor, [ownDesc]);
        b.if_then_end(is_data, |b| {
            //# a. If ownDesc.[[Writable]] is false, return false.
            let writable = b.record_get_slot(ownDesc, InternalSlot::Writable);
            let is_false = b.negate(writable);
            b.if_then_end(is_false, |_| |b| b.ret(Some(writable)));

            //# b. If Type(Receiver) is not Object, return false.
            // TODO: add type insts

            //# c. Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
            let get_own_property = b.record_get_slot(Receiver, InternalSlot::GetOwnProperty);
            // TODO: code for fallible ops
            let existingDescriptor = b.call_virt_with_result(get_own_property, [Receiver, P]);

            //# d. If existingDescriptor is not undefined, then
            let undef = b.make_undefined();
            let is_undef = b.compare_equal(existingDescriptor, undef);
            let is_not_undef = b.negate(is_undef);

            b.if_then_end(is_not_undef, |b| {
                //# i. If IsAccessorDescriptor(existingDescriptor) is true, return false.
                let is_accessor =
                    b.call_with_result(descriptors.IsAccessorDescriptor, [existingDescriptor]);
                b.if_then_end(is_accessor, |_| |b| b.ret(Some(r#false)));

                //# ii. If existingDescriptor.[[Writable]] is false, return false.
                let is_writable = b.record_get_slot(existingDescriptor, InternalSlot::Writable);
                let is_not_writable = b.negate(is_writable);
                b.if_then_end(is_not_writable, |_| |b| b.ret(Some(r#false)));

                //# iii. Let valueDesc be the PropertyDescriptor { [[Value]]: V }.
                let valueDesc = b.record_new();
                b.record_set_slot(valueDesc, InternalSlot::Value, V);

                //# iv. Return ? Receiver.[[DefineOwnProperty]](P, valueDesc).
                let define_own_prop = b.record_get_slot(Receiver, InternalSlot::DefineOwnProperty);
                let result = b.call_virt_with_result(define_own_prop, [Receiver, P, valueDesc]);
                move |b| b.ret(Some(result))
            });

            //# e. Else,
            //# i. Assert: Receiver does not currently have a property P.
            //# ii. Return ? CreateDataProperty(Receiver, P, V).
            let result = b.call_with_result(CreateDataProperty, [Receiver, P, V]);
            move |b| b.ret(Some(result))
        });

        //# 4. Assert: IsAccessorDescriptor(ownDesc) is true.
        //# 5. Let setter be ownDesc.[[Set]].
        let setter = b.record_get_slot(ownDesc, InternalSlot::Set);

        //# 6. If setter is undefined, return false.
        let is_undef = b.compare_equal(setter, undef);
        b.if_then_end(is_undef, |_| |b| b.ret(Some(r#false)));

        //# 7. Perform ? Call(setter, Receiver, « V »).
        // TODO: implement this

        //# 8. Return true.
        b.finish(|b| b.ret(Some(r#true)))
    }

    /// <https://tc39.es/ecma262/#sec-ordinarygetownproperty>
    fn make_OrdinaryGetOwnProperty(
        builder: &mut ProgramBuilder,
        descriptors: Descriptors,
    ) -> FnSignature<2> {
        let (f, [O, P]) = builder.start_function();
        let mut b = Emitter::new(builder, f);
        b.comment("OrdinaryGetOwnProperty");

        //# 1. Assert: IsPropertyKey(P) is true.
        //# 2. If O does not have an own property with key P, return undefined.
        let has_property = b.record_has_prop(O, P);
        let doesnt_have_prop = b.negate(has_property);
        b.if_then_end(doesnt_have_prop, |b| {
            let undef = b.make_undefined();
            move |b| b.ret(Some(undef))
        });

        //# 3. Let D be a newly created Property Descriptor with no fields.
        let D = b.record_new();

        //# 4. Let X be O's own property whose key is P.
        let X = b.record_get_prop(O, P);

        //# 5. If X is a data property, then
        let is_data_prop = b.call_with_result(descriptors.IsDataDescriptor, [X]);
        b.if_then_else(
            is_data_prop,
            |b| {
                //# a. Set D.[[Value]] to the value of X's [[Value]] attribute.
                b.copy_slot(X, D, InternalSlot::Value);

                //# b. Set D.[[Writable]] to the value of X's [[Writable]] attribute.
                b.copy_slot(X, D, InternalSlot::Writable);
            },
            //# 6. Else,
            |b| {
                //# a. Assert: X is an accessor property.
                //# b. Set D.[[Get]] to the value of X's [[Get]] attribute.
                b.copy_slot(X, D, InternalSlot::Get);

                //# c. Set D.[[Set]] to the value of X's [[Set]] attribute.
                b.copy_slot(X, D, InternalSlot::Set);
            },
        );

        //# 7. Set D.[[Enumerable]] to the value of X's [[Enumerable]] attribute.
        b.copy_slot(X, D, InternalSlot::Enumerable);

        //# 8. Set D.[[Configurable]] to the value of X's [[Configurable]] attribute.
        b.copy_slot(X, D, InternalSlot::Configurable);

        //# 9. Return D.
        b.finish(|b| b.ret(Some(D)))
    }

    /// <https://tc39.es/ecma262/#sec-ordinaryisextensible>
    pub fn make_OrdinaryIsExtensible(builder: &mut ProgramBuilder) -> FnSignature<1> {
        let (f, [O]) = builder.start_function();
        let mut b = Emitter::new(builder, f);
        b.comment("OrdinaryIsExtensible");

        let extensible = b.record_get_slot(O, InternalSlot::Extensible);

        b.finish(|b| b.ret(Some(extensible)))
    }
}
