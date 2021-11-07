use crate::{frontend::builder::ProgramBuilder, lifted::lift};

use super::*;

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

#[test]
fn can_add() {
    let (tests, run) = prepare();
    let results = run
        .execute_fn_id(
            tests.Add.id.map_context(),
            vec![Value::Number(2), Value::Number(3)],
        )
        .unwrap();
    assert!(matches!(results, Some(Value::Number(5))));
}
