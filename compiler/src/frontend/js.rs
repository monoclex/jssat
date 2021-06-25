use swc_ecmascript::ast::Script;

use crate::{
    frontend::{builder::*, ir::*},
    name::DebugName,
};

pub fn traverse(_script: Script) -> IR {
    let mut builder = ProgramBuilder::new();

    let utf16_message = "Hello, World!"
        .encode_utf16()
        .into_iter()
        .flat_map(|x| std::array::IntoIter::new(x.to_ne_bytes()))
        .collect::<Vec<_>>();

    let hello_world = builder.constant(DebugName::new("hello_world"), utf16_message);
    let print = builder.external_function(
        "jssatrt_print".into(),
        vec![FFIValueType::Runtime, FFIValueType::Any, FFIValueType::Any],
        FFIReturnType::Void,
    );

    let mut print_stub = builder.start_function(DebugName::new("print_stub"));
    let print_stub_id = print_stub.id;
    {
        let mut block = print_stub.start_block();
        block.mark_entrypoint();
        // let jssatrt = block.get_runtime();
        let print_string = print_stub.parameter(DebugName::new("string"));
        block.call(print_stub_id, vec![print_string]);
        // block.call_external_function(print, vec![jssatrt, print_string, print_string]);
        print_stub.end_block(block.ret(None));
    }
    builder.end_function(print_stub);

    let mut main = builder.start_function(DebugName::new("main"));
    main.mark_main();
    {
        let mut entry = main.start_block();
        entry.mark_entrypoint();
        let string = entry.make_string(hello_world);
        entry.call(print_stub_id, vec![string]);
        main.end_block(entry.ret(None));
    }
    builder.end_function(main);

    builder.finish()
}
