//! A macro crate the parses ECMAScript Grammar Notation, and generates
//! structures.

mod ast;
use ast::*;

pub fn generate(code: &str) -> String {
    let production = Production::from_json(code);
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
