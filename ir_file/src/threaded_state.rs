//! If the presence of a `get-global` expression is detected, this converts the
//! entirety of an AST into carrying a piece of "threaded state" throughout all
//! functions in the application.

use crate::{Assign, Expression, ExpressionData, Statement, StatementData, AST};

pub fn thread_state(ast: &mut AST) -> bool {
    if has_global_expr(ast) {
        do_threading(ast);
        assert!(!has_global_expr(ast));
        true
    } else {
        false
    }
}

fn threaded_global() -> Expression {
    Expression {
        span: None,
        data: ExpressionData::VarReference {
            variable: "jssatGlobal".into(),
        },
    }
}

fn has_global_expr(ast: &AST) -> bool {
    ast.sections
        .iter()
        .any(|section| section.body.iter().any(statement_has_global))
}

fn expr_is_global(expression: &Expression) -> bool {
    match &expression.data {
        ExpressionData::GetGlobal => true,
        ExpressionData::If {
            condition,
            then,
            r#else,
        } => {
            expr_is_global(condition)
                || then.0.iter().any(statement_has_global)
                || expr_is_global(&then.1)
                || r#else.0.iter().any(statement_has_global)
                || expr_is_global(&r#else.1)
        }
        ExpressionData::LetIn {
            variable: _,
            be_bound_to,
            r#in,
        } => {
            expr_is_global(be_bound_to)
                || r#in.0.iter().any(statement_has_global)
                || expr_is_global(&r#in.1)
        }
        ExpressionData::RecordGetProp { record, property } => {
            expr_is_global(record) || expr_is_global(property)
        }
        ExpressionData::RecordGetSlot { record, slot: _ } => expr_is_global(record),
        ExpressionData::RecordHasProp { record, property } => {
            expr_is_global(record) || expr_is_global(property)
        }
        ExpressionData::RecordHasSlot { record, slot: _ } => expr_is_global(record),
        ExpressionData::ListGet { list, property } => {
            expr_is_global(list) || expr_is_global(property)
        }
        ExpressionData::ListHas { list, property } => {
            expr_is_global(list) || expr_is_global(property)
        }
        ExpressionData::ListLen { list } => expr_is_global(list),
        ExpressionData::CallStatic {
            function_name: _,
            args,
        } => args.iter().any(expr_is_global),
        ExpressionData::CallVirt { fn_ptr, args } => {
            expr_is_global(fn_ptr) || args.iter().any(expr_is_global)
        }
        ExpressionData::BinOp { kind: _, lhs, rhs } => expr_is_global(lhs) || expr_is_global(rhs),
        ExpressionData::Negate { expr } => expr_is_global(expr),
        ExpressionData::IsTypeOf { expr, kind: _ } => expr_is_global(expr),
        ExpressionData::IsTypeAs { lhs, rhs } => expr_is_global(lhs) || expr_is_global(rhs),
        ExpressionData::GetFnPtr { function_name: _ }
        | ExpressionData::Unreachable
        | ExpressionData::RecordNew
        | ExpressionData::ListNew
        | ExpressionData::MakeBytes { bytes: _ }
        | ExpressionData::MakeAtom { atom: _ }
        | ExpressionData::MakeInteger { value: _ }
        | ExpressionData::MakeBoolean { value: _ }
        | ExpressionData::VarReference { variable: _ } => false,
    }
}

fn statement_has_global(statement: &Statement) -> bool {
    match &statement.data {
        crate::StatementData::Assign(a) => expr_is_global(&a.value),
        crate::StatementData::If {
            condition,
            then,
            r#else,
        } => {
            expr_is_global(condition)
                || then.iter().any(statement_has_global)
                || r#else
                    .as_ref()
                    .map(|x| x.iter().any(statement_has_global))
                    .unwrap_or(false)
        }
        crate::StatementData::RecordSetProp {
            record,
            prop,
            value,
        } => {
            expr_is_global(record)
                || expr_is_global(prop)
                || value.as_ref().map(|x| expr_is_global(x)).unwrap_or(false)
        }
        crate::StatementData::RecordSetSlot {
            record,
            slot: _,
            value,
        } => expr_is_global(record) || value.as_ref().map(|x| expr_is_global(x)).unwrap_or(false),
        crate::StatementData::ListSet { list, prop, value } => {
            expr_is_global(list)
                || expr_is_global(prop)
                || value.as_ref().map(|x| expr_is_global(x)).unwrap_or(false)
        }
        crate::StatementData::Return { expr } => {
            expr.as_ref().map(|x| expr_is_global(x)).unwrap_or(false)
        }
        crate::StatementData::CallStatic {
            function_name: _,
            args,
        } => args.iter().any(expr_is_global),
        crate::StatementData::CallVirt { fn_ptr, args } => {
            expr_is_global(fn_ptr) || args.iter().any(expr_is_global)
        }
        crate::StatementData::Assert { expr, message: _ } => expr_is_global(expr),
        crate::StatementData::Loop {
            init,
            cond,
            next,
            body,
        } => {
            init.iter().any(|a| expr_is_global(&a.value))
                || expr_is_global(cond)
                || next.iter().any(|a| expr_is_global(&a.value))
                || body.iter().any(statement_has_global)
        }
    }
}

fn do_threading(ast: &mut AST) {
    for section in ast.sections.iter_mut() {
        section.header.parameters.insert(0, "jssatGlobal".into());

        thread_statements(&mut section.body);
    }
}

fn optional_do<T>(it: &mut Option<T>, then: impl FnOnce(&mut T)) {
    it.as_mut().map(then);
}

fn thread_statements(statements: &mut Vec<Statement>) {
    statements.iter_mut().for_each(thread_statement);
}

fn thread_statement(statement: &mut Statement) {
    match &mut statement.data {
        StatementData::Assign(assign) => thread_assign(assign),
        StatementData::If {
            condition,
            then,
            r#else,
        } => {
            thread_expression(condition);
            thread_statements(then);
            optional_do(r#else, thread_statements);
        }
        StatementData::RecordSetProp {
            record,
            prop,
            value,
        } => {
            thread_expression(record);
            thread_expression(prop);
            optional_do(value, thread_expression);
        }
        StatementData::RecordSetSlot {
            record,
            slot: _,
            value,
        } => {
            thread_expression(record);
            optional_do(value, thread_expression);
        }
        StatementData::ListSet { list, prop, value } => {
            thread_expression(list);
            thread_expression(prop);
            optional_do(value, thread_expression);
        }
        StatementData::Return { expr } => {
            optional_do(expr, thread_expression);
        }
        StatementData::CallStatic {
            function_name: _,
            args,
        } => {
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        StatementData::CallVirt { fn_ptr, args } => {
            thread_expression(fn_ptr);
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        StatementData::Assert { expr, message: _ } => {
            thread_expression(expr);
        }
        StatementData::Loop {
            init,
            cond,
            next,
            body,
        } => {
            thread_assigns(init);
            thread_expression(cond);
            thread_assigns(next);
            thread_statements(body);
        }
    }
}

fn thread_assigns(assigns: &mut Vec<Assign>) {
    assigns.iter_mut().for_each(thread_assign);
}

fn thread_assign(assign: &mut Assign) {
    thread_expression(&mut assign.value);
}

fn thread_expressions(expressions: &mut Vec<Expression>) {
    expressions.iter_mut().for_each(thread_expression);
}

fn thread_expression(expression: &mut Expression) {
    match &mut expression.data {
        ExpressionData::GetGlobal => {
            // replace the `GetGlobal` expression as we're threading it through all state
            // now
            expression.data = ExpressionData::VarReference {
                variable: "jssatGlobal".into(),
            };
        }
        ExpressionData::If {
            condition,
            then,
            r#else,
        } => {
            thread_expression(condition);
            let (statements, expression) = then;
            thread_statements(statements);
            thread_expression(expression);
            let (statements, expression) = r#else;
            thread_statements(statements);
            thread_expression(expression);
        }
        ExpressionData::VarReference { variable: _ } => {}
        ExpressionData::LetIn {
            variable: _,
            be_bound_to,
            r#in,
        } => {
            thread_expression(be_bound_to);
            let (statements, expression) = r#in;
            thread_statements(statements);
            thread_expression(expression);
        }
        ExpressionData::Unreachable => {}
        ExpressionData::RecordNew => {}
        ExpressionData::RecordGetProp { record, property } => {
            thread_expression(record);
            thread_expression(property);
        }
        ExpressionData::RecordGetSlot { record, slot: _ } => {
            thread_expression(record);
        }
        ExpressionData::RecordHasProp { record, property } => {
            thread_expression(record);
            thread_expression(property);
        }
        ExpressionData::RecordHasSlot { record, slot: _ } => {
            thread_expression(record);
        }
        ExpressionData::ListNew => {}
        ExpressionData::ListGet { list, property } => {
            thread_expression(list);
            thread_expression(property);
        }
        ExpressionData::ListHas { list, property } => {
            thread_expression(list);
            thread_expression(property);
        }
        ExpressionData::ListLen { list } => {
            thread_expression(list);
        }
        ExpressionData::GetFnPtr { function_name: _ } => {}
        ExpressionData::CallStatic {
            function_name: _,
            args,
        } => {
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        ExpressionData::CallVirt { fn_ptr, args } => {
            thread_expression(fn_ptr);
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        ExpressionData::MakeAtom { atom: _ } => {}
        ExpressionData::MakeBytes { bytes: _ } => {}
        ExpressionData::MakeInteger { value: _ } => {}
        ExpressionData::MakeBoolean { value: _ } => {}
        ExpressionData::BinOp { kind: _, lhs, rhs } => {
            thread_expression(lhs);
            thread_expression(rhs);
        }
        ExpressionData::Negate { expr } => {
            thread_expression(expr);
        }
        ExpressionData::IsTypeOf { expr, kind: _ } => {
            thread_expression(expr);
        }
        ExpressionData::IsTypeAs { lhs, rhs } => {
            thread_expression(lhs);
            thread_expression(rhs);
        }
    }
}
