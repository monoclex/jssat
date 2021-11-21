//! Because the lifted program has a lot of individual, tiny functions, it is
//! extremely ideal to run parallel optimizations on it. These optimizations,
//! because they are individual in nature, cannot do much but will shrink the
//! amount of work that needs to be done by the other parts of the compiler.
//!
//! In addition, whole-program optimization is done after individual
//! optimizations.

use crate::codegen::TypedProgram;

pub fn opt(program: TypedProgram) -> TypedProgram {
    program
}
