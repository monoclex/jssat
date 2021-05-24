use alloc::vec::Vec;

static mut GARBAGE_COLLECTOR_HEAP: Vec<GcObject> = Vec::new();

pub enum GcObject {
    Tombstone,
    Object(),
}

#[no_mangle]
pub extern "C" fn add_tombstone() {
    let heap = unsafe { &mut GARBAGE_COLLECTOR_HEAP };
    heap.push(GcObject::Tombstone);
}
