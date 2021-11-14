use crate::ast::*;
use codegen::*;

pub fn generate_production(production: &Production) -> String {
    let mut s = String::new();
    let mut formatter = Formatter::new(&mut s);

    let mut parse_node = Enum::new(&production.name);
    parse_node.vis("pub");

    parse_node.derive("Debug").derive("Clone");

    let mut impls: Vec<Impl> = vec![];

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

        inner_parse_nodes.for_each(|name| {
            let ident = format!("Box<{}>", name.name);

            let ident = match name.optional.is_some() {
                true => format!("Option<{}>", ident),
                false => ident,
            };

            // TODO(performance): use cool graphs to figure out when enums are
            //   recursive and automatically insert `Box<>`s when necessary
            variant.tuple(ident.as_str());
        });

        parse_node.push_variant(variant);

        // if this rule has a child with a single standalone element, we will
        // implement `From` for it automatically. e.g.
        //
        // ```
        // Rule :
        //   Child
        // ```
        //
        // will produce a
        //
        // impl From<Child> for Rule {}
        //
        // meaning we have an `f : Child -> Rule`

        // we don't want to generate the rule if there are *any* other outstanding
        // tokens around this rule. otherwise, we'll generate from implementations
        // for things that may mean different things - for example,
        //
        // ```
        // Expr :
        //   `++` ChildExpr
        //   `--` ChildExpr
        // ```
        //
        // These two have a single `ChildExpr` for both rules, but we don't want to
        // auto generate `From` impls as we would imply more than we intend to (i.e.,
        // that a `ChildExpr -> Expr` is safe and always exists by automatically
        // coercing it into a `++ChildExpr` which is different from a
        // `ChildExpr` standalone)
        let can_generate_from = body.sequence.len() == 1 && {
            let n = body.sequence[0].as_name();
            matches!(n.map(|n| n.optional.is_none()), Some(true))
        };

        if can_generate_from {
            let child = body.sequence[0].as_name().unwrap();

            // commented out because this isn't very useful
            // let mut from = Impl::new(&production.name);
            // from.impl_trait(format!("From<{}>", child.name));

            // let f = from.new_fn("from");
            // f.arg("child", &child.name);
            // f.ret("Self");
            // f.line(format!(
            //     "{}::{}(child.into())",
            //     &production.name, &variant_name
            // ));

            // impls.push(from);
        }
    }

    parse_node.fmt(&mut formatter).unwrap();

    for r#impl in impls {
        r#impl.fmt(&mut formatter).unwrap();
    }

    s
}

pub fn generate_one_of_production(one_of: &OneOfProduction) -> String {
    let mut parse_node = Enum::new(&one_of.name);
    parse_node.vis("pub");

    parse_node.derive("Debug").derive("Clone");

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
