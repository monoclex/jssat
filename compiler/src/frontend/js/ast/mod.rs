//! Responsible for the creation of ParseNodes as accoridng to ECMAScript
//!
//! In ECMAScript, the program is parsed into a series of `ParseNode`s which
//! are then walked over to execute the program. This module is responsible for
//! the parsing of JavaScript into a series of `ParseNode` structures in pure
//! Rust, and is also responsible for the code that maps these parse nodes to
//! ECMAScript instructions.

use crate::frontend::builder::{DynBlockBuilder, ProgramBuilder, RegisterId};

use super::ecmascript::ECMA262Methods;

pub mod emit_nodes;
pub mod parse_nodes;
mod parser;

pub fn parse_script(script: &str) -> parse_nodes::Script {
    parser::parse_script(script)
}

pub fn emit_nodes(
    program: &mut ProgramBuilder,
    block: &mut DynBlockBuilder,
    ecma_methods: &ECMA262Methods,
    visit_initial_node: impl FnOnce(&mut emit_nodes::NodeEmitter),
) -> RegisterId {
    let mut node_emitter = emit_nodes::NodeEmitter::new(block, program, ecma_methods);
    visit_initial_node(&mut node_emitter);

    let last_visited = node_emitter
        .last_completed
        .expect("expected the node emitter to visit a node");
    last_visited.parse_node
}
