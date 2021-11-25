//! The JSSAT implementation of the ECMAScript specification
//!
//! This module implements all the ECMAScript algorithms specified within the
//! ECMAScript specification. In order to use the generated ECMAScript methods,
//! simply call [`use_ecma262`].

use jssat_ir::frontend::builder::ProgramBuilder;

/// Includes all algorithms specified in ECMA262 into a JSSAT program. To use
/// the algorithms from a JSSAT program, the return type [`ECMA262Methods`] has
/// a field for each method with the [`FnSignature`] for that method. Calling
/// the method will do exactly what the ECMAScript specification says it should.
pub fn use_ecma262(program_builder: &mut ProgramBuilder) -> ECMA262Methods {
    ECMA262Methods::new(program_builder)
}

// thanks to Yandros ꜰʀ-ᴇꜱ on the Rust Programming Language Community Server for
// helping out with this - pressing F12 on a method will go to the source code
//
// https://discord.com/channels/273534239310479360/335502067432947748/906949057379966996
with_builtin_macros::with_builtin! {
    let $path = concat!(env!("OUT_DIR"), "/ECMA262Methods_irfile.rs") in {
        #[path = $path]
        mod generated_code;
        pub use generated_code::*;
    }
}
