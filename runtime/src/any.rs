use std::fmt::Display;

use crate::{runtime::Runtime, string::String};

pub enum Any {
    // TODO: should strings be their own type, or abstracted into a List-like
    //       type? the benefits for putting strings into a list-like type is
    //       that if optimizations for arrays come, the same optimizations that
    //       apply for strings will apply for arrays. the downsides is that it
    //       makes strings harder to work with.
    String(String),
}

pub unsafe extern "C" fn jssatrt_any_new_string(
    runtime: *const Runtime,
    string: *const String,
) -> *const Any {
    notnull!(runtime);
    notnull!(string);

    let string = *Box::from_raw(string as *mut String);
    let any: Any = string.into();
    Box::into_raw(Box::new(any))
}

impl Display for Any {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Any::String(string) => write!(f, "{}", string),
        }
    }
}

impl Into<Any> for String {
    fn into(self) -> Any {
        Any::String(self)
    }
}
