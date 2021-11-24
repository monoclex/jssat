#![allow(unused_imports)]
#![allow(clippy::match_single_binding)]

use crate::frontend::js::ast::parse_nodes::{ArgumentList, IdentifierName};

use super::super::parse_nodes as js;
use super::ToParseNode;
use swc_ecmascript::ast as swc;

impl ToParseNode<js::Expression> for swc::Expr {
    fn to_parse_node(self) -> js::Expression {
        parse(self)
    }
}

impl ToParseNode<js::BindingIdentifier> for swc::BindingIdent {
    fn to_parse_node(self) -> js::BindingIdentifier {
        let identifier = self.id.to_parse_node();
        js::BindingIdentifier::Variant0(identifier.into())
    }
}

fn parse(expr: swc::Expr) -> js::Expression {
    use swc::Expr::*;

    match expr {
        Seq(seq) => seq
            .exprs
            .into_iter()
            .map(|e| parse_assignment(*e))
            .fold(None, |a: Option<js::Expression>, b| {
                Some(match a {
                    None => js::Expression::Variant0(b.into()),
                    Some(e) => js::Expression::Variant1(e.into(), b.into()),
                })
            })
            .unwrap(),
        other => js::Expression::Variant0(parse_assignment(other).into()),
    }
}

fn parse_assignment(expr: swc::Expr) -> js::AssignmentExpression {
    use swc::Expr::*;

    match expr {
        other => js::AssignmentExpression::Variant0(parse_conditional(other).into()),
    }
}

fn parse_conditional(expr: swc::Expr) -> js::ConditionalExpression {
    use swc::Expr::*;

    match expr {
        other => js::ConditionalExpression::Variant0(parse_short_circuit(other).into()),
    }
}

fn parse_short_circuit(expr: swc::Expr) -> js::ShortCircuitExpression {
    use swc::Expr::*;

    match expr {
        other => js::ShortCircuitExpression::Variant0(parse_logical_or(other).into()),
    }
}

fn parse_logical_or(expr: swc::Expr) -> js::LogicalORExpression {
    use swc::Expr::*;

    match expr {
        other => js::LogicalORExpression::Variant0(parse_logical_and(other).into()),
    }
}

fn parse_logical_and(expr: swc::Expr) -> js::LogicalANDExpression {
    use swc::Expr::*;

    match expr {
        other => js::LogicalANDExpression::Variant0(parse_bitwise_or(other).into()),
    }
}

fn parse_bitwise_or(expr: swc::Expr) -> js::BitwiseORExpression {
    use swc::Expr::*;

    match expr {
        other => js::BitwiseORExpression::Variant0(parse_bitwise_xor(other).into()),
    }
}

fn parse_bitwise_xor(expr: swc::Expr) -> js::BitwiseXORExpression {
    use swc::Expr::*;

    match expr {
        other => js::BitwiseXORExpression::Variant0(parse_bitwise_and(other).into()),
    }
}

fn parse_bitwise_and(expr: swc::Expr) -> js::BitwiseANDExpression {
    use swc::Expr::*;

    match expr {
        other => js::BitwiseANDExpression::Variant0(parse_equality(other).into()),
    }
}

fn parse_equality(expr: swc::Expr) -> js::EqualityExpression {
    use swc::Expr::*;

    match expr {
        other => js::EqualityExpression::Variant0(parse_relational(other).into()),
    }
}

fn parse_relational(expr: swc::Expr) -> js::RelationalExpression {
    use swc::Expr::*;

    match expr {
        other => js::RelationalExpression::Variant0(parse_shift(other).into()),
    }
}

fn parse_shift(expr: swc::Expr) -> js::ShiftExpression {
    use swc::Expr::*;

    match expr {
        other => js::ShiftExpression::Variant0(parse_additive(other).into()),
    }
}

fn parse_additive(expr: swc::Expr) -> js::AdditiveExpression {
    use swc::Expr::*;

    match expr {
        other => js::AdditiveExpression::Variant0(parse_multiplicative(other).into()),
    }
}

fn parse_multiplicative(expr: swc::Expr) -> js::MultiplicativeExpression {
    use swc::Expr::*;

    match expr {
        other => js::MultiplicativeExpression::Variant0(parse_exponentiation(other).into()),
    }
}

fn parse_exponentiation(expr: swc::Expr) -> js::ExponentiationExpression {
    use swc::Expr::*;

    match expr {
        other => js::ExponentiationExpression::Variant0(parse_unary(other).into()),
    }
}

fn parse_unary(expr: swc::Expr) -> js::UnaryExpression {
    use swc::Expr::*;

    match expr {
        other => js::UnaryExpression::Variant0(parse_update(other).into()),
    }
}

fn parse_update(expr: swc::Expr) -> js::UpdateExpression {
    use swc::Expr::*;

    match expr {
        other => js::UpdateExpression::Variant0(parse_left_hand_side(other).into()),
    }
}

fn parse_left_hand_side(expr: swc::Expr) -> js::LeftHandSideExpression {
    use swc::Expr::*;

    if let Call(call_expr) = expr {
        return js::LeftHandSideExpression::Variant1(parse_call(call_expr).into());
    }

    match expr {
        other => js::LeftHandSideExpression::Variant0(parse_new(other).into()),
    }
}

fn parse_call(expr: swc::CallExpr) -> js::CallExpression {
    if let Some(cceaah) = parse_cover_call_expression_and_arrow_head(expr.clone()) {
        return js::CallExpression::Variant0(cceaah.into());
    }

    if let swc::ExprOrSuper::Super(_) = expr.callee {
        let args = parse_arguments(expr.args);
        let super_call = js::SuperCall::Variant0(args.into());
        return js::CallExpression::Variant1(super_call.into());
    }

    todo!()
}

fn parse_cover_call_expression_and_arrow_head(
    expr: swc::CallExpr,
) -> Option<js::CoverCallExpressionAndAsyncArrowHead> {
    if let swc::ExprOrSuper::Expr(invoking_expr) = expr.callee {
        let member = parse_member(*invoking_expr);
        let args = parse_arguments(expr.args);
        return Some(js::CoverCallExpressionAndAsyncArrowHead::Variant0(
            member.into(),
            args.into(),
        ));
    }

    None
}

fn parse_arguments(exprs: Vec<swc::ExprOrSpread>) -> js::Arguments {
    if exprs.is_empty() {
        return js::Arguments::Variant0;
    }

    let arg_list = exprs
        .into_iter()
        .fold(None, |acc: Option<js::ArgumentList>, x| {
            let expr = parse_assignment(*x.expr);
            let spreads = x.spread.is_some();

            Some(match (acc, spreads) {
                (None, false) => ArgumentList::Variant0(expr.into()),
                (None, true) => ArgumentList::Variant1(expr.into()),
                (Some(args), false) => ArgumentList::Variant2(args.into(), expr.into()),
                (Some(args), true) => ArgumentList::Variant3(args.into(), expr.into()),
            })
        })
        .unwrap();

    // it doesn't matter if we use Variant1 or Variant2
    js::Arguments::Variant1(arg_list.into())
}

fn parse_new(expr: swc::Expr) -> js::NewExpression {
    use swc::Expr::*;

    match expr {
        other => js::NewExpression::Variant0(parse_member(other).into()),
    }
}

fn parse_member(expr: swc::Expr) -> js::MemberExpression {
    use swc::Expr::*;

    match expr {
        other => js::MemberExpression::Variant0(parse_primary(other).into()),
    }
}

fn parse_primary(expr: swc::Expr) -> js::PrimaryExpression {
    use swc::Expr::*;
    use swc::Lit::*;

    match expr {
        Ident(ident) => {
            let identifier = ident.to_parse_node();
            let ident_ref = js::IdentifierReference::Variant0(identifier.into());

            js::PrimaryExpression::Variant1(ident_ref.into())
        }
        Lit(lit) => {
            let literal = match lit {
                Str(x) => js::Literal::Variant3(js::StringLiteral(x.value.to_string()).into()),
                Bool(_) => todo!(),
                Null(_) => todo!(),
                Num(_) => todo!(),
                BigInt(_) => todo!(),
                Regex(_) => todo!(),
                JSXText(_) => todo!(),
            };

            js::PrimaryExpression::Variant2(literal.into())
        }
        _ => todo!(),
    }
}

impl ToParseNode<js::Identifier> for swc::Ident {
    fn to_parse_node(self) -> js::Identifier {
        let name = js::IdentifierName(self.sym.to_string());
        js::Identifier::Variant0(name.into())
    }
}
