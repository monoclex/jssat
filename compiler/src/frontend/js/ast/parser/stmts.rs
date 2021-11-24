use super::super::parse_nodes as js;
use super::ToParseNode;
use swc_ecmascript::ast as swc;

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
            Block(x) => s(Variant0(
                js::BlockStatement::Variant0(x.to_parse_node().into()).into(),
            )),
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

impl ToParseNode<js::Block> for swc::BlockStmt {
    fn to_parse_node(mut self) -> js::Block {
        let first = match self.stmts.pop() {
            None => return js::Block::Variant0(None),
            Some(x) => x,
        };

        let mut list = js::StatementList::Variant0(first.to_parse_node().into());

        for stmt in self.stmts {
            let next = stmt.to_parse_node();
            list = js::StatementList::Variant1(list.into(), next.into());
        }

        js::Block::Variant0(Some(list.into()))
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
        match self {
            swc::Decl::Fn(x) => js::Declaration::Variant0(x.to_parse_node().into()),
            swc::Decl::Class(x) => js::Declaration::Variant1(x.to_parse_node().into()),
            swc::Decl::Var(x) => js::Declaration::Variant2(x.to_parse_node().into()),
            swc::Decl::TsInterface(_)
            | swc::Decl::TsTypeAlias(_)
            | swc::Decl::TsEnum(_)
            | swc::Decl::TsModule(_) => panic!("typescript not supported"),
        }
    }
}

impl ToParseNode<js::ExpressionStatement> for swc::ExprStmt {
    fn to_parse_node(self) -> js::ExpressionStatement {
        js::ExpressionStatement::Variant0(self.expr.to_parse_node().into())
    }
}
