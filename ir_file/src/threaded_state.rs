//! If the presence of a `get-global` expression is detected, this converts the
//! entirety of an AST into carrying a piece of "threaded state" throughout all
//! functions in the application.

use crate::{Assign, Expression, Statement, AST};

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
    Expression::VarReference {
        variable: "jssatGlobal".into(),
    }
}

fn has_global_expr(ast: &AST) -> bool {
    ast.sections
        .iter()
        .any(|section| section.body.iter().any(statement_has_global))
}

fn expr_is_global(expression: &Expression) -> bool {
    match expression {
        Expression::GetGlobal => true,
        Expression::If {
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
        Expression::LetIn {
            variable: _,
            be_bound_to,
            r#in,
        } => {
            expr_is_global(be_bound_to)
                || r#in.0.iter().any(statement_has_global)
                || expr_is_global(&r#in.1)
        }
        Expression::RecordGetProp { record, property } => {
            expr_is_global(record) || expr_is_global(property)
        }
        Expression::RecordGetSlot { record, slot: _ } => expr_is_global(record),
        Expression::RecordHasProp { record, property } => {
            expr_is_global(record) || expr_is_global(property)
        }
        Expression::RecordHasSlot { record, slot: _ } => expr_is_global(record),
        Expression::ListGet { list, property } => expr_is_global(list) || expr_is_global(property),
        Expression::ListHas { list, property } => expr_is_global(list) || expr_is_global(property),
        Expression::ListLen { list } => expr_is_global(list),
        Expression::CallStatic {
            function_name: _,
            args,
        } => args.iter().any(expr_is_global),
        Expression::CallVirt { fn_ptr, args } => {
            expr_is_global(fn_ptr) || args.iter().any(expr_is_global)
        }
        Expression::BinOp { kind: _, lhs, rhs } => expr_is_global(lhs) || expr_is_global(rhs),
        Expression::Negate { expr } => expr_is_global(expr),
        Expression::IsTypeOf { expr, kind: _ } => expr_is_global(expr),
        Expression::IsTypeAs { lhs, rhs } => expr_is_global(lhs) || expr_is_global(rhs),
        Expression::GetFnPtr { function_name: _ }
        | Expression::Unreachable
        | Expression::RecordNew
        | Expression::ListNew
        | Expression::MakeBytes { bytes: _ }
        | Expression::MakeAtom { atom: _ }
        | Expression::MakeInteger { value: _ }
        | Expression::MakeBoolean { value: _ }
        | Expression::VarReference { variable: _ } => false,
    }
}

fn statement_has_global(statement: &Statement) -> bool {
    match statement {
        crate::Statement::Assign(a) => expr_is_global(&a.value),
        crate::Statement::If {
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
        crate::Statement::RecordSetProp {
            record,
            prop,
            value,
        } => {
            expr_is_global(record)
                || expr_is_global(prop)
                || value.as_ref().map(|x| expr_is_global(x)).unwrap_or(false)
        }
        crate::Statement::RecordSetSlot {
            record,
            slot: _,
            value,
        } => expr_is_global(record) || value.as_ref().map(|x| expr_is_global(x)).unwrap_or(false),
        crate::Statement::ListSet { list, prop, value } => {
            expr_is_global(list)
                || expr_is_global(prop)
                || value.as_ref().map(|x| expr_is_global(x)).unwrap_or(false)
        }
        crate::Statement::Return { expr } => {
            expr.as_ref().map(|x| expr_is_global(x)).unwrap_or(false)
        }
        crate::Statement::CallStatic {
            function_name: _,
            args,
        } => args.iter().any(expr_is_global),
        crate::Statement::CallVirt { fn_ptr, args } => {
            expr_is_global(fn_ptr) || args.iter().any(expr_is_global)
        }
        crate::Statement::Assert { expr, message: _ } => expr_is_global(expr),
        crate::Statement::Loop {
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
    match statement {
        Statement::Assign(assign) => thread_assign(assign),
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            thread_expression(condition);
            thread_statements(then);
            optional_do(r#else, thread_statements);
        }
        Statement::RecordSetProp {
            record,
            prop,
            value,
        } => {
            thread_expression(record);
            thread_expression(prop);
            optional_do(value, thread_expression);
        }
        Statement::RecordSetSlot {
            record,
            slot: _,
            value,
        } => {
            thread_expression(record);
            optional_do(value, thread_expression);
        }
        Statement::ListSet { list, prop, value } => {
            thread_expression(list);
            thread_expression(prop);
            optional_do(value, thread_expression);
        }
        Statement::Return { expr } => {
            optional_do(expr, thread_expression);
        }
        Statement::CallStatic {
            function_name: _,
            args,
        } => {
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        Statement::CallVirt { fn_ptr, args } => {
            thread_expression(fn_ptr);
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        Statement::Assert { expr, message: _ } => {
            thread_expression(expr);
        }
        Statement::Loop {
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
    match expression {
        Expression::GetGlobal => {
            // replace the `GetGlobal` expression as we're threading it through all state
            // now
            *expression = Expression::VarReference {
                variable: "jssatGlobal".into(),
            };
        }
        Expression::If {
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
        Expression::VarReference { variable: _ } => {}
        Expression::LetIn {
            variable: _,
            be_bound_to,
            r#in,
        } => {
            thread_expression(be_bound_to);
            let (statements, expression) = r#in;
            thread_statements(statements);
            thread_expression(expression);
        }
        Expression::Unreachable => {}
        Expression::RecordNew => {}
        Expression::RecordGetProp { record, property } => {
            thread_expression(record);
            thread_expression(property);
        }
        Expression::RecordGetSlot { record, slot: _ } => {
            thread_expression(record);
        }
        Expression::RecordHasProp { record, property } => {
            thread_expression(record);
            thread_expression(property);
        }
        Expression::RecordHasSlot { record, slot: _ } => {
            thread_expression(record);
        }
        Expression::ListNew => {}
        Expression::ListGet { list, property } => {
            thread_expression(list);
            thread_expression(property);
        }
        Expression::ListHas { list, property } => {
            thread_expression(list);
            thread_expression(property);
        }
        Expression::ListLen { list } => {
            thread_expression(list);
        }
        Expression::GetFnPtr { function_name: _ } => {}
        Expression::CallStatic {
            function_name: _,
            args,
        } => {
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        Expression::CallVirt { fn_ptr, args } => {
            thread_expression(fn_ptr);
            thread_expressions(args);
            args.insert(0, threaded_global());
        }
        Expression::MakeAtom { atom: _ } => {}
        Expression::MakeBytes { bytes: _ } => {}
        Expression::MakeInteger { value: _ } => {}
        Expression::MakeBoolean { value: _ } => {}
        Expression::BinOp { kind: _, lhs, rhs } => {
            thread_expression(lhs);
            thread_expression(rhs);
        }
        Expression::Negate { expr } => {
            thread_expression(expr);
        }
        Expression::IsTypeOf { expr, kind: _ } => {
            thread_expression(expr);
        }
        Expression::IsTypeAs { lhs, rhs } => {
            thread_expression(lhs);
            thread_expression(rhs);
        }
    }
}
