use crate::{
    frontend::builder::{FnSignature, ProgramBuilder},
    isa::InternalSlot,
};

use super::{abstract_operations::EmitterExt, Emitter};

// TODO: figure out a clena way to do this?

#[allow(non_snake_case)]
#[derive(Clone, Copy)]
pub struct Descriptors {
    pub IsAccessorDescriptor: FnSignature<1>,
    pub IsDataDescriptor: FnSignature<1>,
    pub IsGenericDescriptor: FnSignature<1>,
}

#[allow(non_snake_case)]
impl Descriptors {
    pub fn new(builder: &mut ProgramBuilder) -> Self {
        let IsAccessorDescriptor = Descriptors::IsAccessorDescriptor(builder);
        let IsDataDescriptor = Descriptors::IsDataDescriptor(builder);
        let IsGenericDescriptor =
            Descriptors::IsGenericDescriptor(builder, IsAccessorDescriptor, IsDataDescriptor);

        Self {
            IsAccessorDescriptor,
            IsDataDescriptor,
            IsGenericDescriptor,
        }
    }

    /// <https://tc39.es/ecma262/#sec-isaccessordescriptor>
    fn IsAccessorDescriptor(builder: &mut ProgramBuilder) -> FnSignature<1> {
        let (f, [Desc]) = builder.start_function();
        let mut b = Emitter::new(builder, f);
        b.comment("IsAccessorDescriptor");

        let r#true = b.make_bool(true);
        let r#false = b.make_bool(false);

        //# 1. If Desc is undefined, return false.
        let undefined = b.make_undefined();
        let is_undefined = b.compare_equal(Desc, undefined);
        b.if_then_end(is_undefined, |_| |b| b.ret(Some(r#false)));

        //# 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
        let has_get = b.record_has_slot(Desc, InternalSlot::Get);
        let get_absent = b.negate(has_get);
        let has_set = b.record_has_slot(Desc, InternalSlot::Set);
        let set_absent = b.negate(has_set);
        let both_are_absent = b.and(get_absent, set_absent);
        b.if_then_end(both_are_absent, |_| |b| b.ret(Some(r#false)));

        //# 3. Return true.
        b.finish(|b| b.ret(Some(r#true)))
    }

    /// <https://tc39.es/ecma262/#sec-isdatadescriptor>
    fn IsDataDescriptor(builder: &mut ProgramBuilder) -> FnSignature<1> {
        let (f, [Desc]) = builder.start_function();
        let mut b = Emitter::new(builder, f);
        b.comment("IsDataDescriptor");

        let r#true = b.make_bool(true);
        let r#false = b.make_bool(false);

        //# 1. If Desc is undefined, return false.
        let undefined = b.make_undefined();
        let is_undefined = b.compare_equal(Desc, undefined);
        b.if_then_end(is_undefined, |_| |b| b.ret(Some(r#false)));

        //# 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
        let has_value = b.record_has_slot(Desc, InternalSlot::Value);
        let value_absent = b.negate(has_value);
        let has_writable = b.record_has_slot(Desc, InternalSlot::Writable);
        let writable_absent = b.negate(has_writable);
        let both_are_absent = b.and(value_absent, writable_absent);
        b.if_then_end(both_are_absent, |_| |b| b.ret(Some(r#false)));

        //# 3. Return true.
        b.finish(|b| b.ret(Some(r#true)))
    }

    /// <https://tc39.es/ecma262/#sec-isgenericdescriptor>
    fn IsGenericDescriptor(
        builder: &mut ProgramBuilder,
        IsAccessorDescriptor: FnSignature<1>,
        IsDataDescriptor: FnSignature<1>,
    ) -> FnSignature<1> {
        let (f, [Desc]) = builder.start_function();
        let mut b = Emitter::new(builder, f);
        b.comment("IsGenericDescriptor");

        let r#true = b.make_bool(true);
        let r#false = b.make_bool(false);
        //# 1. If Desc is undefined, return false.
        let undefined = b.make_undefined();
        let is_undefined = b.compare_equal(Desc, undefined);
        b.if_then_end(is_undefined, |_| |b| b.ret(Some(r#false)));

        //# 2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, return true.
        let is_accessor = b.call_with_result(IsAccessorDescriptor, [Desc]);
        let is_accessor_false = b.compare_equal(is_accessor, r#false);
        let is_data = b.call_with_result(IsDataDescriptor, [Desc]);
        let is_data_false = b.compare_equal(is_data, r#false);
        let both_false = b.and(is_accessor_false, is_data_false);
        b.if_then_end(both_false, |_| |b| b.ret(Some(r#true)));

        //# 3. Return false.
        b.finish(|b| b.ret(Some(r#false)))
    }
}
