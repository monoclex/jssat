use std::fmt::Display;

use crate::runtime::Runtime;
use widestring::U16String;

pub struct String(U16String);

/// Creates a new UTF-16 string, associating the string with the specified runtime.
///
/// The `length` argument is the number of elements, **not** the number of bytes.
#[no_mangle]
pub unsafe extern "C" fn jssatrt_string_new_utf16(
    runtime: *const Runtime,
    payload: *const u16,
    length: usize,
) -> *const String {
    notnull!(runtime);

    let string = if length == 0 {
        String::new_utf16(&[])
    } else {
        notnull!(payload);
        not0!(length);
        let payload = std::slice::from_raw_parts(payload, length);
        String::new_utf16(payload)
    };

    // TODO: use the runtime to allocate the string into for garbage collection
    Box::into_raw(Box::new(string))
}

impl String {
    pub fn new_utf16(payload: &[u16]) -> Self {
        Self(U16String::from_vec(payload))
    }
}

impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string_lossy())
    }
}
