//! Contains code necessary to simulate ECMAScript hosts, for replicating the
//! host environment of JavaScript code.

mod jssat_host;
pub use jssat_host::JSSATHostEnvironment;

use crate::frontend::builder::{DynBlockBuilder, ProgramBuilder, RegisterId};

use super::ecmascript::ECMA262Methods;

/// State necessary for hosts to hook their own features into a JavaScript JSSAT
/// program.
pub struct HostHookState<'scope> {
    pub ecma_methods: &'scope ECMA262Methods,
    pub program: &'scope mut ProgramBuilder,
    pub block: &'scope mut DynBlockBuilder,
    pub threaded_global: RegisterId,
    pub realm: RegisterId,
    pub global_object: RegisterId,
}

/// Trait that represents something that modifies the environment of an
/// ECMAScript program to be suitable for a particular host.
pub trait HostEnvironment {
    fn inject<'s>(&mut self, hook: HostHookState<'s>);
}
