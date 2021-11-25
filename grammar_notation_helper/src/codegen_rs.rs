use std::array::IntoIter;

use crate::ast::*;
use codegen::*;
use convert_case::{Case, Casing};

pub fn generate(productions: Productions) -> String {
    let ast_len = productions.ast.len();
    let len = ast_len + productions.one_of_ast.len();
    eprintln!("{} productions found", len);

    let prelude = "#![allow(unused_variables)]
#![allow(non_snake_case)]

use jssat_ir::isa::Atom;
use jssat_ir::frontend::builder::ProgramBuilder;

use derive_more::Display;

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
";

    let mut str = prelude.to_string();
    let mut formatter = Formatter::new(&mut str);

    generate_combinatory_enum(&productions, &mut formatter);

    generate_atoms(&productions, &mut formatter);

    for production in productions.ast.iter() {
        generate_production(production, &mut formatter);
    }

    for one_of_production in productions.one_of_ast.iter() {
        generate_one_of_production(one_of_production, &mut formatter);
    }

    generate_visitor_ast(&productions, &mut formatter);

    str
}

pub fn generate_production(production: &Production, formatter: &mut Formatter) {
    let mut parse_node = Enum::new(&production.name);
    parse_node.vis("pub");

    parse_node.derive("Debug").derive("Clone");

    let mut impls: Vec<Impl> = vec![];

    let mut variant_names = vec![];

    for (idx, body) in production.body.iter().enumerate() {
        // as a hack, we shove the documentation in the same spot that the name goes
        // this is because there is no .doc method
        // TODO(refactor): submit a PR to codegen to add a `.doc()` method to
        //   variants on enums

        let variant_name = format!("Variant{}", idx);

        let name = format!(
            "
/// ```ignore
/// {}
/// ```
{}",
            body.source, variant_name
        );
        let name = name.trim();
        let mut variant = Variant::new(name);

        let inner_parse_nodes = body.sequence.iter().filter_map(|r| match r {
            Rule::Name(name) => Some(name),
            _ => None,
        });

        let mut elems = 0;

        inner_parse_nodes.for_each(|name| {
            let ident = format!("Box<{}>", name.name);

            let ident = match name.optional.is_some() {
                true => format!("Option<{}>", ident),
                false => ident,
            };

            // TODO(performance): use cool graphs to figure out when enums are
            //   recursive and automatically insert `Box<>`s when necessary
            variant.tuple(ident.as_str());
            elems += 1;
        });

        variant_names.push((variant_name, elems));

        parse_node.push_variant(variant);
    }

    impls.push({
        let mut enum_impl = Impl::new(&production.name);
        let f = enum_impl
            .new_fn("variant_idx")
            .arg_ref_self()
            .ret("usize")
            .vis("pub");

        f.line("match self");

        let mut variants = Block::new("");
        for (idx, (name, elems)) in variant_names.into_iter().enumerate() {
            let tuple = match elems {
                0 => "".to_string(),
                n => format!(
                    "({})",
                    std::iter::repeat("_")
                        .take(n)
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            };

            variants.line(format!("Self::{}{} => {},", name, tuple, idx));
        }

        f.push_block(variants);

        enum_impl
    });

    parse_node.fmt(formatter).unwrap();

    for r#impl in impls {
        r#impl.fmt(formatter).unwrap();
    }
}

pub fn generate_one_of_production(one_of: &OneOfProduction, formatter: &mut Formatter) {
    let mut parse_node = Enum::new(&one_of.name);
    parse_node.vis("pub");

    parse_node.derive("Debug").derive("Clone");

    for terminal in &one_of.terminals {
        parse_node.push_variant(Variant::new(&symbols_to_ascii(terminal.as_str())));
    }

    parse_node.fmt(formatter).unwrap();
}

fn symbols_to_ascii(symbols: &str) -> String {
    let mut s = String::new();

    for char in symbols.chars() {
        if char.is_ascii_alphabetic() {
            s.push(char);
            continue;
        }

        let str = match char {
            '*' => "Star",
            '=' => "Eq",
            '/' => "Slash",
            '%' => "Percent",
            '+' => "Plus",
            '-' => "Dash",
            '<' => "Lt",
            '>' => "Gt",
            '&' => "Amp",
            '^' => "Carrot",
            '|' => "Pipe",
            other => panic!("unrecognized symbol to shorten: {}", other),
        };

        s.push_str(str);
    }

    s
}

fn all_names(productions: &Productions) -> impl Iterator<Item = &str> {
    let builtin_names = [
        "LineTerminator",
        "IdentifierName",
        "RegularExpressionLiteral",
        "NullLiteral",
        "StringLiteral",
        "NumericLiteral",
        "BooleanLiteral",
        "NoSubstitutionTemplate",
        "TemplateHead",
        "TemplateMiddle",
        "TemplateTail",
    ];
    let production_names = productions.ast.iter().map(|x| x.name.as_str());
    let one_of_names = productions.one_of_ast.iter().map(|x| x.name.as_str());

    IntoIter::new(builtin_names)
        .chain(production_names)
        .chain(one_of_names)
}

fn generate_combinatory_enum(productions: &Productions, formatter: &mut Formatter) {
    let mut combinatory = Enum::new("ParseNodeKind");
    combinatory.vis("pub");

    // #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    IntoIter::new([
        "Clone",
        "Copy",
        "Debug",
        "PartialEq",
        "Eq",
        "Hash",
        "Display",
    ])
    .for_each(|name| {
        combinatory.derive(name);
    });

    for name in all_names(productions) {
        combinatory.push_variant(Variant::new(name));
    }

    combinatory.fmt(formatter).unwrap();
}

fn generate_visitor_ast(productions: &Productions, formatter: &mut Formatter) {
    let mut visitor = Trait::new("Visitor");
    visitor.vis("pub");

    visitor
        .new_fn("pre_visit")
        .arg_mut_self()
        .arg("kind", "ParseNodeKind")
        .arg("variant_idx", "usize");
    visitor.new_fn("post_visit").arg_mut_self();

    let mut gen = |n: &str| {
        visitor
            .new_fn(&format!("visit_{}", n.to_case(Case::Snake)))
            .arg_mut_self()
            .arg("node", format!("&{}", n))
            .push_block(Block::new(""));
    };

    gen("LineTerminator");
    gen("IdentifierName");
    gen("RegularExpressionLiteral");
    gen("NullLiteral");
    gen("StringLiteral");
    gen("NumericLiteral");
    gen("BooleanLiteral");
    gen("NoSubstitutionTemplate");
    gen("TemplateHead");
    gen("TemplateMiddle");
    gen("TemplateTail");

    for one_of in productions.one_of_ast.iter() {
        gen(&one_of.name);
    }

    for production in productions.ast.iter() {
        let f = visitor.new_fn(&format!("visit_{}", production.name.to_case(Case::Snake)));
        f.arg_mut_self();
        f.arg("node", format!("&{}", &production.name));

        f.line(format!(
            "self.pre_visit(ParseNodeKind::{}, node.variant_idx());",
            production.name
        ));

        f.line(format!(
            "self.visit_impl_{}(node);",
            production.name.to_case(Case::Snake)
        ));

        f.line("self.post_visit();");

        let f = visitor.new_fn(&format!(
            "visit_impl_{}",
            production.name.to_case(Case::Snake)
        ));
        f.arg_mut_self();
        f.arg("node", format!("&{}", &production.name));

        f.line("match node ");
        f.push_block({
            let mut block = Block::new("");

            for (idx, body) in production.body.iter().enumerate() {
                let named_variants = body.sequence.iter().flat_map(|r| r.as_name());

                // generate `Self::Variant(a, b, c) => `
                let mut destructure = format!("{}::Variant{}", &production.name, idx);

                let variant_name = |x| format!("r#elem{}", x);
                let destructures = (named_variants.clone().enumerate())
                    .map(|(idx, _)| variant_name(idx))
                    .collect::<Vec<_>>()
                    .join(", ");

                if !destructures.is_empty() {
                    destructure.push('(');
                    destructure.push_str(&destructures);
                    destructure.push(')');
                }

                destructure.push_str(" => ");

                // generate the code after destructuring element

                let mut visit = Block::new("");

                for (idx, variant) in named_variants.enumerate() {
                    // conditionally visit it if it's optional
                    if variant.optional.is_some() {
                        visit.line(format!("if let Some({0}) = {0}", variant_name(idx)));

                        let mut visit_block = Block::new("");

                        visit_block.line(format!(
                            "self.visit_{}({});",
                            variant.name.to_case(Case::Snake),
                            variant_name(idx)
                        ));
                        visit.push_block(visit_block);
                    } else {
                        visit.line(format!(
                            "self.visit_{}({});",
                            variant.name.to_case(Case::Snake),
                            variant_name(idx)
                        ));
                    }
                }

                // stitch it to our match
                block.line(destructure);
                block.push_block(visit);
            }

            block
        });
    }

    visitor.fmt(formatter).unwrap();
}

fn generate_atoms(productions: &Productions, formatter: &mut Formatter) {
    let mut dealer = Struct::new("Dealer");
    dealer.vis("pub");

    for name in all_names(productions) {
        dealer.field(&format!("pub {}", name), "Atom");
    }

    dealer.fmt(formatter).unwrap();

    let mut dealer_impl = Impl::new("Dealer");

    let new = dealer_impl
        .new_fn("new")
        .arg("program", "&mut ProgramBuilder")
        .ret("Self")
        .vis("pub");

    new.line("Self");
    let mut fields = Block::new("");

    for name in all_names(productions) {
        fields.line(format!("{0}: program.dealer.deal({0:?}),", name));
    }

    new.push_block(fields);

    let translate = dealer_impl
        .new_fn("translate")
        .arg_ref_self()
        .arg("node", "ParseNodeKind")
        .ret("Atom")
        .vis("pub");

    translate.line("match node");

    let mut patterns = Block::new("");

    for name in all_names(productions) {
        patterns.line(format!("ParseNodeKind::{0} => self.{0},", name));
    }

    translate.push_block(patterns);

    dealer_impl.fmt(formatter).unwrap();
}
