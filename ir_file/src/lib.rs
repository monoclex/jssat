//! JSSAT IR File
//!
//! An IR file is a file used to specify functions in JSSAT IR. It can then be
//! used to generate Rust code, which provides a simple hook in order to include
//! any IR file within a [`ProgramBuilder`].
//!
//! This module provides a JSSAT IR parser, AST, and Rust code generator for
//! JSSAT IR files.

mod ast;
pub use ast::*;

mod codegen;
pub use codegen::*;

mod parser;
pub use parser::*;
