//! A macro crate the parses ECMAScript Grammar Notation, and generates
//! structures.

#![feature(drain_filter)]

mod ast;
use ast::*;

mod codegen_rs;

pub fn generate(code: &str) -> String {
    eprintln!("parsing productions");
    let productions = Production::from_json(code);

    let ast_len = productions.ast.len();
    let len = ast_len + productions.one_of_ast.len();
    eprintln!("{} productions found", len);

    std::iter::once(
        "
pub struct LineTerminator;
pub struct IdentifierName(String);
pub struct RegularExpressionLiteral(String);
pub struct NullLiteral;
pub struct StringLiteral(String);
pub struct NumericLiteral(f64);
pub struct BooleanLiteral(bool);

/// NoSubstitutionTemplate :: ` TemplateCharacters opt `
pub struct NoSubstitutionTemplate(String);

/// TemplateHead :: ` TemplateCharacters opt ${
pub struct TemplateHead(String);

/// TemplateMiddle :: } TemplateCharacters opt $ {
pub struct TemplateMiddle(String);

/// TemplateTail :: } TemplateCharacters opt
pub struct TemplateTail(String);
"
        .to_string(),
    )
    .chain(
        (productions.ast.iter())
            .map(codegen_rs::generate_production)
            .enumerate()
            .map(|(idx, code)| {
                eprintln!("{} / {} productions generated", idx + 1, len);
                code
            }),
    )
    .chain(
        (productions.one_of_ast.iter())
            .map(codegen_rs::generate_one_of_production)
            .enumerate()
            .map(|(idx, code)| {
                eprintln!("{} / {} productions generated", ast_len + idx + 1, len);
                code
            }),
    )
    .collect::<Vec<_>>()
    .join("\n")
}
