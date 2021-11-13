use crate::ast::*;
use codegen::*;

pub fn generate_production(production: &Production) -> String {
    let mut parse_node = Enum::new(&production.name);
    parse_node.vis("pub");

    for (idx, body) in production.body.iter().enumerate() {
        let mut variant = Variant::new(format!("Variant{}", idx).as_str());

        let inner_parse_nodes = body.iter().filter_map(|r| match r {
            Rule::Name(name) => Some(name),
            _ => None,
        });

        inner_parse_nodes.for_each(|name| {
            let name = match name.optional.is_some() {
                true => format!("Option<{}>", name.name),
                false => name.name.clone(),
            };

            // TODO(performance): use cool graphs to figure out when enums are
            //   recursive and automatically insert `Box<>`s when necessary
            variant.tuple(format!("Box<{}>", name).as_str());
        });

        parse_node.push_variant(variant);
    }

    let mut s = String::new();
    let mut formatter = Formatter::new(&mut s);
    parse_node.fmt(&mut formatter).unwrap();

    s
}

pub fn generate_one_of_production(one_of: &OneOfProduction) -> String {
    let mut parse_node = Enum::new(&one_of.name);
    parse_node.vis("pub");

    for terminal in &one_of.terminals {
        parse_node.push_variant(Variant::new(&symbols_to_ascii(terminal.as_str())));
    }

    let mut s = String::new();
    let mut formatter = Formatter::new(&mut s);
    parse_node.fmt(&mut formatter).unwrap();

    s
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
