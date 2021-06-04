//! # JSSAT Runtime
//!
//! This crate

use mimalloc::MiMalloc;
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};
use widestring::U16String;

#[global_allocator]
static ALLOCATOR: MiMalloc = MiMalloc;

macro_rules! notnull {
    ($x:ident) => {
        #[cfg(debug_assertions)]
        if $x.is_null() {
            panic!(concat!("`", stringify!($x), "` was null"));
        }
    };
}

macro_rules! not0 {
    ($x:ident) => {
        #[cfg(debug_assertions)]
        if $x == 0 {
            panic!(concat!("`", stringify!($x), "` was zero"));
        }
    };
}

// TODO: fill this in with a definition
// when we use `std` and try to link using `clang -flto`, we get an error if
// this method is missing.
#[no_mangle]
pub extern "C" fn __rust_probestack() {
    // SAFETY: this method needs a proper implementation to prevent against
    // stack clashing. by marking this `unsafe`, we hope to capture the essence
    // of how improper it is to leave this blank.
    #[allow(unused_unsafe)]
    unsafe {}
}

// TODO: include information necessary to the runtime, such as garbage
// collection or job queues or whatever
pub struct Runtime {}

#[no_mangle]
pub extern "C" fn jssatrt_runtime_new() -> *mut Runtime {
    let runtime = Runtime {};

    let runtime_ptr = Box::into_raw(Box::new(runtime));

    runtime_ptr
}

#[no_mangle]
pub extern "C" fn jssatrt_runtime_drop(runtime_ptr: *mut Runtime) {
    notnull!(runtime_ptr);

    let runtime = unsafe { Box::from_raw(runtime_ptr) };
    drop(runtime);
}

#[derive(Debug, Clone)]
pub enum Value {
    Constant(&'static [u8]),
    List(Vec<u8>),
    // strings could be classified as a List, but we're not doing that just so
    // it's easier.
    // TODO: revisit the list/string decision? the idea was to abstract arrays
    // and strings under the same thing, but maybe that's not a good idea? shrug
    String(U16String),
    // use an `Rc` so that when we clone the reference they point to the same
    // thing
    Record(Rc<RefCell<HashMap<Key, Value>>>),
    Number(f64),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Key {
    // since all internal slots are specified in code, we can intern them
    // thus, we don't have to do any string comparisons
    InternalSlot(usize),
    // TODO: perform interning for fields
    Field(U16String),
}

/// Marks an object as root. This is automatically performed when creating new
/// objects, or when getting values from fields. Typically you will only ever
/// need to concern yourself with unmarking roots.
#[no_mangle]
pub extern "C" fn jssatrt_value_tracing_markroot(_runtime: *const Runtime, _value: *const Value) {
    notnull!(_runtime);
    notnull!(_value);

    todo!()
}

/// Unmarks an object as a root. This means it is no longer reachable by the
/// code. The only way it will remain alive is through references in other
/// objects.
#[no_mangle]
pub extern "C" fn jssatrt_value_tracing_unmarkroot(_runtime: *const Runtime, _value: *const Value) {
    notnull!(_runtime);
    notnull!(_value);

    todo!()
}

/// Uses tracing garbage collection to keep track of the newly created object.
/// Automatically marks the object as a root.
#[no_mangle]
pub extern "C" fn jssatrt_record_tracing_new(_runtime: *const Runtime) -> *const Value {
    notnull!(_runtime);

    // TODO: use the `_runtime` to allocate objects into it

    let value = Value::Record(Rc::new(RefCell::new(HashMap::new())));
    let value_ptr = Box::into_raw(Box::new(value));

    value_ptr
}

#[no_mangle]
pub extern "C" fn jssatrt_record_set(
    _runtime: *const Runtime,
    record: *const Value,
    key: *const Key,
    value: *const Value,
) {
    notnull!(_runtime);
    notnull!(record);
    notnull!(key);
    notnull!(value);

    // TODO: validate safety
    let record = unsafe { &*record };
    let key = unsafe { &*key };
    let value = unsafe { &*value };

    let record = match record {
        Value::Record(record) => record,
        _ => panic!("did not receive record"),
    };

    record.borrow_mut().insert(key.clone(), value.clone());
}

#[no_mangle]
pub extern "C" fn jssatrt_record_get(
    _runtime: *const Runtime,
    output: *mut *const Value,
    record: *const Value,
    key: *const Key,
) {
    notnull!(_runtime);
    notnull!(record);
    notnull!(key);

    // TODO: validate safety
    let record = unsafe { &*record };
    let key = unsafe { &*key };

    let record = match record {
        Value::Record(record) => record,
        _ => panic!("did not receive record"),
    };

    let record = record.borrow();
    let value = record.get(key);

    if let Some(value) = value {
        // TODO: validate safety
        unsafe {
            // SAFETY: the pointer to `value` is as valid as the pointer to `record`
            *output = value as *const Value;
        }
    }
}

#[no_mangle]
pub extern "C" fn jssatrt_key_new_fromvalue(
    _runtime: *const Runtime,
    value: *const Value,
) -> *const Key {
    notnull!(_runtime);
    notnull!(value);

    todo!()
}

#[no_mangle]
pub extern "C" fn jssatrt_key_new_fromslot(_runtime: *const Runtime, slot: usize) -> *const Key {
    notnull!(_runtime);

    let key = Key::InternalSlot(slot);
    let key_ptr = Box::into_raw(Box::new(key));

    key_ptr
}

#[no_mangle]
pub extern "C" fn jssatrt_constant_new(
    _runtime: *const Runtime,
    ptr: *const u8,
    len: usize,
) -> *const Value {
    notnull!(_runtime);
    notnull!(ptr);
    not0!(len);

    // TODO: validate this code
    let slice = unsafe { std::slice::from_raw_parts::<'static>(ptr, len) };

    // TODO: this is inefficient, need to rethink this
    let constant_ptr = Box::into_raw(Box::new(Value::Constant(slice)));
    constant_ptr
}

#[no_mangle]
pub extern "C" fn jssatrt_print(
    _runtime: *const Runtime,
    _environment: *const Value,
    arguments: *const Value,
) {
    notnull!(_runtime);
    notnull!(_environment);
    notnull!(arguments);

    // TODO: validate safety
    let arguments = unsafe { &*arguments };

    println!("{:?}", arguments);
}
