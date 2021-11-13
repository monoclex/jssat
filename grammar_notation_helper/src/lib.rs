//! A macro crate the parses ECMAScript Grammar Notation, and generates
//! structures.

#![feature(drain_filter)]

mod ast;
use ast::*;

pub fn generate(code: &str) -> String {
    let mut production = Production::from_json(code);

    production.simplify();

    format!(
        "
pub struct HelloWorld;

/*
{:#?}
*/

",
        production
    )
}
