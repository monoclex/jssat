//! A macro crate the parses ECMAScript Grammar Notation, and generates
//! structures.

#![feature(drain_filter)]

mod ast;
use ast::*;

mod codegen_rs;

pub fn generate(code: &str) -> String {
    eprintln!("parsing productions");
    let mut productions = Production::from_json(code);
    remove_optionals(&mut productions);

    eprintln!("generating productions");
    codegen_rs::generate(productions)
}

#[test]
fn what() {
    crate::generate(include_str!(
        "../../compiler/src/frontend/js/ast/parse_nodes.json"
    ));
    panic!();
}
