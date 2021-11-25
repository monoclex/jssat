//! Contains code necessary to simulate ECMAScript hosts, for replicating the
//! host environment of JavaScript code.

mod jssat_host;
pub use jssat_host::JSSATHostEnvironment;

use jssat_ir::frontend::builder::{DynBlockBuilder, ProgramBuilder, RegisterId};

use super::{ast::parse_nodes::Dealer, ecmascript::ECMA262Methods};

/// State necessary for hosts to hook their own features into a JavaScript JSSAT
/// program.
pub struct HostHookState<'scope> {
    pub ecma_methods: &'scope ECMA262Methods,
    pub parse_nodes: &'scope Dealer,
    pub program: &'scope mut ProgramBuilder,
    pub block: &'scope mut DynBlockBuilder,
    pub threaded_global: RegisterId,
    pub realm: RegisterId,
    pub global_object: RegisterId,
}

/// Trait that represents something that modifies the environment of an
/// ECMAScript program to be suitable for a particular host.
pub trait HostEnvironment {
    fn inject(&mut self, hook: HostHookState);
}
