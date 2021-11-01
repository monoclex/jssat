//! The JSSAT implementation of the ECMAScript specification
//!
//! This module implements all the ECMAScript algorithms specified within the
//! ECMAScript specification. In order to use the generated ECMAScript methods,
//! simply call [`use_ecma262`].

#![allow(non_snake_case)]

use crate::frontend::builder::ProgramBuilder;

/// Includes all algorithms specified in ECMA262 into a JSSAT program. To use
/// the algorithms from a JSSAT program, the return type [`ECMA262Methods`] has
/// a field for each method with the [`FnSignature`] for that method. Calling
/// the method will do exactly what the ECMAScript specification says it should.
pub fn use_ecma262(program_builder: &mut ProgramBuilder) -> ECMA262Methods {
    ECMA262Methods::new(program_builder)
}

use crate::{
    frontend::{
        builder::{FnSignature, RegisterId},
        emitter::{ControlFlow, Emitter},
    },
    isa::{InternalSlot, TrivialItem, ValueType},
};

include!(concat!(env!("OUT_DIR"), "/ecma262_irfile.rs"));
