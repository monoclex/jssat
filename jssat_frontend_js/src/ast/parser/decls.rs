use crate::ast::parse_nodes::{ArgumentList, IdentifierName};

use super::super::parse_nodes as js;
use super::ToParseNode;
use swc_ecmascript::ast::{self as swc};

impl ToParseNode<js::HoistableDeclaration> for swc::FnDecl {
    fn to_parse_node(self) -> js::HoistableDeclaration {
        let f = self.function;

        match (f.is_async, f.is_generator) {
            (false, false) => {
                js::HoistableDeclaration::Variant0(parse_function_declaration(self.ident, f).into())
            }
            (_, _) => todo!(),
        }
    }
}

fn parse_function_declaration(ident: swc::Ident, f: swc::Function) -> js::FunctionDeclaration {
    let name = swc::BindingIdent {
        id: ident,
        type_ann: None,
    }
    .to_parse_node();

    let formal_params = f.params.to_parse_node();

    let body = f.body.and_then(|x| match x.to_parse_node() {
        js::Block::Variant0 => None,
        js::Block::Variant1(x) => Some(x),
    });

    let body = match body {
        Some(x) => js::FunctionStatementList::Variant1(x),
        None => js::FunctionStatementList::Variant0,
    };

    let body = js::FunctionBody::Variant0(body.into());

    js::FunctionDeclaration::Variant0(name.into(), formal_params.into(), body.into())
}

impl ToParseNode<js::FormalParameters> for Vec<swc::Param> {
    fn to_parse_node(mut self) -> js::FormalParameters {
        // handle [empty]
        if self.is_empty() {
            return js::FormalParameters::Variant0;
        }

        // handle FunctionRestParameter
        if self.len() == 1 {
            let it = self.pop().unwrap();

            if it.pat.is_rest() {
                let rest = it.pat.rest().unwrap();
                return js::FormalParameters::Variant1(parse_function_rest_parameter(rest).into());
            }

            self.push(it);
        }

        // handle etc
        let parse_params = handle_formal_param_list(self);

        let mut formals = vec![];
        let mut rest = None;

        for p in parse_params {
            assert!(matches!(rest, None));
            match p {
                ParseParam::Formal(x) => formals.push(x),
                ParseParam::Rest(x) => rest = Some(x),
            }
        }

        //    [a, b, c, d]
        // -> ((((a), b), c), d)
        let mut formal_param_list = js::FormalParameterList::Variant0(formals.remove(0).into());

        while !formals.is_empty() {
            formal_param_list = js::FormalParameterList::Variant1(
                formal_param_list.into(),
                formals.remove(0).into(),
            );
        }

        match rest {
            Some(rest) => js::FormalParameters::Variant4(formal_param_list.into(), rest.into()),
            // doesn't maatter if we choose 2 or 3
            None => js::FormalParameters::Variant2(formal_param_list.into()),
        }
    }
}

enum ParseParam {
    Formal(js::FormalParameter),
    Rest(js::FunctionRestParameter),
}

fn handle_formal_param_list(args: Vec<swc::Param>) -> Vec<ParseParam> {
    fn binding_ident(it: swc::BindingIdent) -> js::FormalParameter {
        let k = it.to_parse_node();
        let single = js::SingleNameBinding::Variant0(k.into());
        let elem = js::BindingElement::Variant0(single.into());
        js::FormalParameter::Variant0(elem.into())
    }

    args.into_iter()
        .map(|param| match param.pat {
            swc::Pat::Ident(name) => ParseParam::Formal(binding_ident(name)),
            swc::Pat::Array(_) => todo!(),
            swc::Pat::Rest(rest) => ParseParam::Rest(parse_function_rest_parameter(rest)),
            swc::Pat::Object(_) => todo!(),
            swc::Pat::Assign(_) => todo!(),
            swc::Pat::Invalid(_) => todo!(),
            swc::Pat::Expr(_) => todo!(),
        })
        .collect()
}

fn parse_function_rest_parameter(pat: swc::RestPat) -> js::FunctionRestParameter {
    todo!()
}

impl ToParseNode<js::LexicalDeclaration> for swc::VarDecl {
    fn to_parse_node(self) -> js::LexicalDeclaration {
        todo!()
    }
}

impl ToParseNode<js::ClassDeclaration> for swc::ClassDecl {
    fn to_parse_node(self) -> js::ClassDeclaration {
        todo!()
    }
}
