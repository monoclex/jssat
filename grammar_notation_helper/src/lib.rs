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
#[derive(Debug, Clone)]
pub struct LineTerminator;

#[derive(Debug, Clone)]
pub struct IdentifierName(pub String);

#[derive(Debug, Clone)]
pub struct RegularExpressionLiteral(pub String);

#[derive(Debug, Clone)]
pub struct NullLiteral;

#[derive(Debug, Clone)]
pub struct StringLiteral(pub String);

#[derive(Debug, Clone)]
pub struct NumericLiteral(pub f64);

#[derive(Debug, Clone)]
pub struct BooleanLiteral(pub bool);

/// NoSubstitutionTemplate :: ` TemplateCharacters opt `
#[derive(Debug, Clone)]
pub struct NoSubstitutionTemplate(pub String);

/// TemplateHead :: ` TemplateCharacters opt ${
#[derive(Debug, Clone)]
pub struct TemplateHead(pub String);

/// TemplateMiddle :: } TemplateCharacters opt $ {
#[derive(Debug, Clone)]
pub struct TemplateMiddle(pub String);

/// TemplateTail :: } TemplateCharacters opt
#[derive(Debug, Clone)]
pub struct TemplateTail(pub String);
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
