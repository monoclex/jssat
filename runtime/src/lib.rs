#![no_std]
#![feature(default_alloc_error_handler)]
#![feature(lang_items)]

use core::{mem::ManuallyDrop, panic::PanicInfo};
use hashbrown::HashMap;

mod module;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[panic_handler]
fn panic_handler(_: &PanicInfo) -> ! {
    loop {}
}

#[lang = "eh_personality"]
fn eh_personality(_: i32) {}

#[no_mangle]
fn rust_oom() -> ! {
    loop {}
}

#[repr(C)]
pub union FFIMap {
    abi_size: [u8; 64],
    hashmap: ManuallyDrop<Map>,
}

#[test]
fn ffimap_is_64_bytes() {
    assert_eq!(64, core::mem::size_of::<FFIMap>());
}

pub struct Map {
    map: HashMap<Key, Value>,
}

impl Into<FFIMap> for Map {
    fn into(self) -> FFIMap {
        FFIMap {
            hashmap: ManuallyDrop::new(self),
        }
    }
}

impl Into<Map> for FFIMap {
    fn into(self) -> Map {
        ManuallyDrop::into_inner(unsafe { self.hashmap })
    }
}

pub enum Key {
    // String(),
    Number(f64),
}

pub enum Value {
    // String(String),
    Number(f64),
    Map(),
}

#[no_mangle]
pub extern "C" fn make_map() -> FFIMap {
    Map {
        map: HashMap::new(),
    }
    .into()
}

#[no_mangle]
pub extern "C" fn map_set_f64_f64(map: &mut FFIMap, key: f64, value: f64) {
    // TODO: set the value
}

#[no_mangle]
pub extern "C" fn drop_map(map: FFIMap) {
    let map: Map = map.into();
    core::mem::drop(map);
}

#[no_mangle]
pub extern "C" fn yield_3() -> i32 {
    module::what()
}

#[no_mangle]
pub extern "C" fn schedule(job: fn() -> ()) {}
