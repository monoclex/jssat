//! Responsible for the creation of ParseNodes as accoridng to ECMAScript
//!
//! In ECMAScript, the program is parsed into a series of `ParseNode`s which
//! are then walked over to execute the program. This module is responsible for
//! the parsing of JavaScript into a series of `ParseNode` structures in pure
//! Rust, and is also responsible for the code that maps these parse nodes to
//! ECMAScript instructions.

pub fn x() -> HelloWorld {
    HelloWorld
}

with_builtin_macros::with_builtin! {
    let $path = concat!(env!("OUT_DIR"), "/parse_nodes.rs") in {
        #[path = $path]
        mod generated_code;
        pub use generated_code::*;
    }
}
