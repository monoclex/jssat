//! JSSAT IR File
//!
//! An IR file is a file used to specify functions in JSSAT IR. It can then be
//! used to generate Rust code, which provides a simple hook in order to include
//! any IR file within a [`ProgramBuilder`].
//!
//! This module provides a JSSAT IR parser, AST, and Rust code generator for
//! JSSAT IR files.

#![feature(bindings_after_at)]

mod ast;
pub use ast::*;

mod codegen_rs;
pub use codegen_rs::*;

mod parser;
pub use parser::*;

mod threaded_state;

pub fn generate(name: &str, code: &str) -> String {
    let mut ast = parser::parse(code);
    threaded_state::thread_state(&mut ast);
    codegen_rs::gen(name, ast)
}
