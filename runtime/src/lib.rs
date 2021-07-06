//! # JSSAT Runtime
//!
//! This crate

use any::Any;
use mimalloc::MiMalloc;
use runtime::Runtime;

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

pub mod any;
pub mod runtime;
pub mod string;

// // TODO: fill this in with a definition
// // when we use `std` and try to link using `clang -flto`, we get an error if
// // this method is missing.
// #[no_mangle]
// pub extern "C" fn __rust_probestack() {
//     // SAFETY: this method needs a proper implementation to prevent against
//     // stack clashing. by marking this `unsafe`, we hope to capture the essence
//     // of how improper it is to leave this blank.
//     #[allow(unused_unsafe)]
//     unsafe {}
// }

/// # Safety
///
/// Valid only if a valid `_runtime` and `arguments` are passed to it.
#[no_mangle]
pub unsafe extern "C" fn jssatrt_print_any(_runtime: *const Runtime, arguments: *const Any) {
    notnull!(_runtime);
    notnull!(arguments);

    // TODO: validate safety
    let any = &*arguments;
    println!("{}", any);
}
