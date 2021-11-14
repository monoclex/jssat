use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::parser::{Parser, Syntax};

use crate::frontend::js::ast::parse_nodes::IdentifierName;

use super::parse_nodes as js;
use swc_ecmascript::ast as swc;

pub fn parse_script(script: &str) -> js::Script {
    let swc_script = to_swc_script(script.to_string());
    swc_script.to_parse_node()
}

fn to_swc_script(source: String) -> swc::Script {
    // https://github.com/Starlight-JS/starlight/blob/4c4ce5d0178fb28c3b2a044d572473baaf057b73/crates/starlight/src/vm.rs#L275-L300
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));
    let fm = cm.new_source_file(FileName::Anon, source);

    let mut parser = Parser::new(
        Syntax::Es(Default::default()),
        StringInput::from(fm.as_ref()),
        None,
    );

    for err in parser.take_errors() {
        err.into_diagnostic(&handler).emit();
    }

    parser.parse_script().expect("script to parse")
}

trait ToParseNode<P> {
    fn to_parse_node(self) -> P;
}

impl ToParseNode<js::Script> for swc::Script {
    fn to_parse_node(self) -> js::Script {
        if self.body.is_empty() {
            return js::Script::Variant0(None);
        }

        let script_body = js::ScriptBody::Variant0(self.body.to_parse_node().into());

        js::Script::Variant0(Some(script_body.into()))
    }
}

impl ToParseNode<js::StatementList> for Vec<swc::Stmt> {
    // a recursive algo to create a recursive data structure
    fn to_parse_node(mut self) -> js::StatementList {
        // we want to create a list nil/cons-like structure

        // we will consider the following scenarios:
        //
        // [] - nothing
        // [ x ] - single item
        // [ ...n, x ] - `n` items

        // []: we know this can never happen because the only time
        //     we convert a vec of statements to a StatementList
        //     is when there is one or more elements
        assert!(!self.is_empty(), "should not be empty");

        // [ x ]: this would be a Cons(x, nil) - or in our case, a
        //        StatementList::Variant0
        //
        //        so we simply construct the list and we're done
        if self.len() == 1 {
            let stmt = self.pop().unwrap();
            return js::StatementList::Variant0(stmt.to_parse_node().into());
        }

        // [ ...n, x ]: take the first item, and append it to the end
        //              of calling `to_parse_node` for the other items
        let stmt = self.remove(0);
        let js_stmt = stmt.to_parse_node().into();
        let rest = self.to_parse_node().into();
        js::StatementList::Variant1(rest, js_stmt)
    }
}

impl ToParseNode<js::StatementListItem> for swc::Stmt {
    fn to_parse_node(self) -> js::StatementListItem {
        use js::IterationStatement as IS;
        use js::Statement::*;
        use swc::Stmt::*;

        let s = |x: js::Statement| js::StatementListItem::Variant0(x.into());
        let i = |x: js::IterationStatement| {
            s(Variant5(js::BreakableStatement::Variant0(x.into()).into()))
        };

        match self {
            Block(x) => s(Variant0(x.to_parse_node().into())),
            Empty(x) => s(Variant2(x.to_parse_node().into())),
            Debugger(x) => s(Variant13(x.to_parse_node().into())),
            With(x) => s(Variant9(x.to_parse_node().into())),
            Return(x) => s(Variant8(x.to_parse_node().into())),
            Labeled(x) => s(Variant10(x.to_parse_node().into())),
            Break(x) => s(Variant7(x.to_parse_node().into())),
            Continue(x) => s(Variant6(x.to_parse_node().into())),
            If(x) => s(Variant4(x.to_parse_node().into())),
            Switch(x) => {
                let breakable = js::BreakableStatement::Variant1(x.to_parse_node().into());
                s(Variant5(breakable.into()))
            }
            Throw(x) => s(Variant11(x.to_parse_node().into())),
            Try(x) => s(Variant12(x.to_parse_node().into())),
            While(x) => i(IS::Variant1(x.to_parse_node().into())),
            DoWhile(x) => i(IS::Variant0(x.to_parse_node().into())),
            For(x) => i(IS::Variant2(x.to_parse_node().into())),
            ForIn(_) | ForOf(_) => todo!(),
            // ForIn(x) => s(Variant15(x.to_parse_node().into())),
            // ForOf(x) => s(Variant16(x.to_parse_node().into())),
            Decl(x) => js::StatementListItem::Variant1(x.to_parse_node().into()),
            Expr(x) => s(Variant3(x.to_parse_node().into())),
        }
    }
}

impl ToParseNode<js::BlockStatement> for swc::BlockStmt {
    fn to_parse_node(self) -> js::BlockStatement {
        todo!()
    }
}
impl ToParseNode<js::EmptyStatement> for swc::EmptyStmt {
    fn to_parse_node(self) -> js::EmptyStatement {
        js::EmptyStatement::Variant0
    }
}
impl ToParseNode<js::DebuggerStatement> for swc::DebuggerStmt {
    fn to_parse_node(self) -> js::DebuggerStatement {
        js::DebuggerStatement::Variant0
    }
}
impl ToParseNode<js::WithStatement> for swc::WithStmt {
    fn to_parse_node(self) -> js::WithStatement {
        todo!()
    }
}
impl ToParseNode<js::ReturnStatement> for swc::ReturnStmt {
    fn to_parse_node(self) -> js::ReturnStatement {
        todo!()
    }
}
impl ToParseNode<js::LabelledStatement> for swc::LabeledStmt {
    fn to_parse_node(self) -> js::LabelledStatement {
        todo!()
    }
}
impl ToParseNode<js::BreakStatement> for swc::BreakStmt {
    fn to_parse_node(self) -> js::BreakStatement {
        todo!()
    }
}
impl ToParseNode<js::ContinueStatement> for swc::ContinueStmt {
    fn to_parse_node(self) -> js::ContinueStatement {
        todo!()
    }
}
impl ToParseNode<js::IfStatement> for swc::IfStmt {
    fn to_parse_node(self) -> js::IfStatement {
        todo!()
    }
}

impl ToParseNode<js::SwitchStatement> for swc::SwitchStmt {
    fn to_parse_node(self) -> js::SwitchStatement {
        todo!()
    }
}

impl ToParseNode<js::ThrowStatement> for swc::ThrowStmt {
    fn to_parse_node(self) -> js::ThrowStatement {
        todo!()
    }
}

impl ToParseNode<js::TryStatement> for swc::TryStmt {
    fn to_parse_node(self) -> js::TryStatement {
        todo!()
    }
}

impl ToParseNode<js::WhileStatement> for swc::WhileStmt {
    fn to_parse_node(self) -> js::WhileStatement {
        todo!()
    }
}

impl ToParseNode<js::DoWhileStatement> for swc::DoWhileStmt {
    fn to_parse_node(self) -> js::DoWhileStatement {
        todo!()
    }
}

impl ToParseNode<js::ForStatement> for swc::ForStmt {
    fn to_parse_node(self) -> js::ForStatement {
        todo!()
    }
}

impl ToParseNode<js::ForInOfStatement> for swc::ForInStmt {
    fn to_parse_node(self) -> js::ForInOfStatement {
        todo!()
    }
}

impl ToParseNode<js::ForInOfStatement> for swc::ForOfStmt {
    fn to_parse_node(self) -> js::ForInOfStatement {
        todo!()
    }
}

impl ToParseNode<js::Declaration> for swc::Decl {
    fn to_parse_node(self) -> js::Declaration {
        todo!()
    }
}

impl ToParseNode<js::ExpressionStatement> for swc::ExprStmt {
    fn to_parse_node(self) -> js::ExpressionStatement {
        js::ExpressionStatement::Variant0(self.expr.to_parse_node().into())
    }
}

impl ToParseNode<js::Expression> for swc::Expr {
    fn to_parse_node(self) -> js::Expression {
        use swc::Expr::*;

        // `js::Expression` is phrased as recursive descent
        // so we order the expressions based on that

        match self {
            // AssignmentExpression
            // ConditionalExpression
            // ShortCircuitExpression
            // LogicalORExpression
            // CoalesceExpression
            // YieldExpression
            // ArrowFunction
            // AsyncArrowFunction
            This(_) => todo!(),
            Array(_) => todo!(),
            Object(_) => todo!(),
            Fn(_) => todo!(),
            Unary(_) => todo!(),
            Update(_) => todo!(),
            Bin(_) => todo!(),
            Assign(_) => todo!(),
            Member(_) => todo!(),
            Cond(_) => todo!(),
            Call(call_expr) => {
                let args = match call_expr.args.len() {
                    0 => js::Arguments::Variant0,
                    _ => todo!(),
                };

                let call_expr = match call_expr.callee {
                    swc::ExprOrSuper::Super(_) => {
                        let super_call = js::SuperCall::Variant0(args.into());
                        js::CallExpression::Variant1(super_call.into())
                    }
                    swc::ExprOrSuper::Expr(_) => todo!(),
                };

                let x = call_expr;
                let x = js::LeftHandSideExpression::Variant1(x.into());
                let x = js::UpdateExpression::Variant0(x.into());
                let x = js::UnaryExpression::Variant0(x.into());
                let x = js::ExponentiationExpression::Variant0(x.into());
                let x = js::MultiplicativeExpression::Variant0(x.into());
                let x = js::AdditiveExpression::Variant0(x.into());
                let x = js::ShiftExpression::Variant0(x.into());
                let x = js::RelationalExpression::Variant0(x.into());
                let x = js::EqualityExpression::Variant0(x.into());
                let x = js::BitwiseANDExpression::Variant0(x.into());
                let x = js::BitwiseXORExpression::Variant0(x.into());
                let x = js::BitwiseORExpression::Variant0(x.into());
                let x = js::LogicalANDExpression::Variant0(x.into());
                let x = js::LogicalORExpression::Variant0(x.into());
                let x = js::ShortCircuitExpression::Variant0(x.into());
                let x = js::ConditionalExpression::Variant0(x.into());
                let x = js::AssignmentExpression::Variant0(x.into());
                js::Expression::Variant0(x.into())
            }
            New(_) => todo!(),
            Seq(_) => todo!(),
            Ident(ident) => {
                let x = ident.sym.to_string();
                let x = IdentifierName(x);
                let x = js::Identifier::Variant0(x.into());
                let x = js::IdentifierReference::Variant0(x.into());
                let x = js::PrimaryExpression::Variant1(x.into());
                let x = js::MemberExpression::Variant0(x.into());
                let x = js::NewExpression::Variant0(x.into());
                let x = js::LeftHandSideExpression::Variant0(x.into());
                let x = js::UpdateExpression::Variant0(x.into());
                let x = js::UnaryExpression::Variant0(x.into());
                let x = js::ExponentiationExpression::Variant0(x.into());
                let x = js::MultiplicativeExpression::Variant0(x.into());
                let x = js::AdditiveExpression::Variant0(x.into());
                let x = js::ShiftExpression::Variant0(x.into());
                let x = js::RelationalExpression::Variant0(x.into());
                let x = js::EqualityExpression::Variant0(x.into());
                let x = js::BitwiseANDExpression::Variant0(x.into());
                let x = js::BitwiseXORExpression::Variant0(x.into());
                let x = js::BitwiseORExpression::Variant0(x.into());
                let x = js::LogicalANDExpression::Variant0(x.into());
                let x = js::LogicalORExpression::Variant0(x.into());
                let x = js::ShortCircuitExpression::Variant0(x.into());
                let x = js::ConditionalExpression::Variant0(x.into());
                let x = js::AssignmentExpression::Variant0(x.into());
                js::Expression::Variant0(x.into())
            }
            Lit(lit) => {
                let x = match lit {
                    swc::Lit::Str(s) => {
                        js::Literal::Variant3(js::StringLiteral(s.value.to_string()).into())
                    }
                    swc::Lit::Bool(_) => todo!(),
                    swc::Lit::Null(_) => todo!(),
                    swc::Lit::Num(_) => todo!(),
                    swc::Lit::BigInt(_) => todo!(),
                    swc::Lit::Regex(_) => todo!(),
                    swc::Lit::JSXText(_) => todo!(),
                };

                let x = js::PrimaryExpression::Variant2(x.into());
                let x = js::MemberExpression::Variant0(x.into());
                let x = js::NewExpression::Variant0(x.into());
                let x = js::LeftHandSideExpression::Variant0(x.into());
                let x = js::UpdateExpression::Variant0(x.into());
                let x = js::UnaryExpression::Variant0(x.into());
                let x = js::ExponentiationExpression::Variant0(x.into());
                let x = js::MultiplicativeExpression::Variant0(x.into());
                let x = js::AdditiveExpression::Variant0(x.into());
                let x = js::ShiftExpression::Variant0(x.into());
                let x = js::RelationalExpression::Variant0(x.into());
                let x = js::EqualityExpression::Variant0(x.into());
                let x = js::BitwiseANDExpression::Variant0(x.into());
                let x = js::BitwiseXORExpression::Variant0(x.into());
                let x = js::BitwiseORExpression::Variant0(x.into());
                let x = js::LogicalANDExpression::Variant0(x.into());
                let x = js::LogicalORExpression::Variant0(x.into());
                let x = js::ShortCircuitExpression::Variant0(x.into());
                let x = js::ConditionalExpression::Variant0(x.into());
                let x = js::AssignmentExpression::Variant0(x.into());
                js::Expression::Variant0(x.into())
            }
            Tpl(_) => todo!(),
            TaggedTpl(_) => todo!(),
            Arrow(_) => todo!(),
            Class(_) => todo!(),
            Yield(_) => todo!(),
            MetaProp(_) => todo!(),
            Await(_) => todo!(),
            Paren(_) => todo!(),
            PrivateName(_) => todo!(),
            OptChain(_) => todo!(),
            Invalid(_) => todo!(),
            JSXMember(_) | JSXNamespacedName(_) | JSXEmpty(_) | JSXElement(_) | JSXFragment(_)
            | TsTypeAssertion(_) | TsConstAssertion(_) | TsNonNull(_) | TsAs(_) => {
                panic!("unsupported JSX or TS syntax")
            }
        }
    }
}
