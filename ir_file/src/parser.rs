use super::*;
use lexpr::{Parser, Value};

pub fn parse(code: &str) -> AST {
    let mut parser = Parser::from_str(code);

    for datum in parser.datum_iter().map(Result::unwrap) {}

    todo!()
}
