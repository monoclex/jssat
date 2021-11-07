use crate::{frontend::builder::ProgramBuilder, lifted::lift};

use super::*;
use Value::*;

with_builtin_macros::with_builtin! {
    let $path = concat!(env!("OUT_DIR"), "/Tests_irfile.rs") in {
        #[path = $path]
        mod generated_code;
        pub use generated_code::*;
    }
}

fn prepare() -> (Tests, Interpreter<'static>) {
    let mut builder = ProgramBuilder::new();
    builder.create_blank_entrypoint();
    let tests = Tests::new(&mut builder);

    let ir = builder.finish();
    let lifted = lift(ir);
    let lifted = Box::leak(Box::new(lifted));
    let run = InterpreterBuilder::new(lifted);
    let run = Box::leak(Box::new(run));
    let run = run.build();
    (tests, run)
}

macro_rules! list {
    () => {
        Value::List(Default::default())
    };
    ($x: expr, $($rest: expr),+) => {{
        let list_value = Value::List(Default::default());
        let mut list = list_value.try_into_list_mut().unwrap();
        list!(>>, list, $x, $($rest),+);
        drop(list);
        list_value
    }};
    (>>, $list: ident, $x: expr, $($rest: expr),+) => {{
        $list.push($x);
        list!(>>, $list, $($rest),+)
    }};
    (>>, $list: ident, $x: expr) => {{
        $list.push($x);
    }};
}

#[test]
fn can_add() {
    let (tests, run) = prepare();
    let results = run
        .execute_fn_id(tests.Add.id.map_context(), vec![Number(2), Number(3)])
        .unwrap();
    assert!(matches!(results, Some(Number(5))));
}

#[test]
fn can_get() {
    let (tests, run) = prepare();

    let results = run
        .execute_fn_id(
            tests.GetList.id.map_context(),
            vec![list![Number(1), Number(2), Number(3)], Number(1)],
        )
        .unwrap();
    assert!(matches!(results, Some(Number(2))));
}
