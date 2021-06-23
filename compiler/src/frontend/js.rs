use swc_ecmascript::ast::Script;

use crate::{
    frontend::{builder::*, ir::*},
    name::DebugName,
};

pub fn traverse(_script: Script) -> IR {
    let mut builder = ProgramBuilder::new();

    let hello_world = builder.constant(DebugName::new("hello_world"), "Hello, World!".into());
    let print = builder.external_function(
        "print".into(),
        vec![FFIValueType::Runtime, FFIValueType::Any],
        FFIReturnType::Void,
    );

    let mut main = builder.start_function(DebugName::new("main"));
    main.mark_main();

    let mut entry = main.start_block(DebugName::new("entry"));
    entry.mark_entrypoint();
    entry.call_external_function(print, vec![Value::Runtime, Value::Constant(hello_world)]);
    main.end_block(entry.ret(None));

    builder.end_function(main);

    builder.finish()
}
