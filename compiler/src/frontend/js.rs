use std::borrow::Borrow;

#[cfg(feature = "link-swc")]
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
#[cfg(feature = "link-swc")]
use swc_ecmascript::{
    ast::Script,
    parser::{Parser, Syntax},
};

use crate::{
    frontend::{builder::*, ir::*},
    id::IdCompat,
    name::DebugName,
};

#[cfg(not(feature = "link-swc"))]
fn to_script(_source: String) -> () {}

#[cfg(feature = "link-swc")]
fn to_script(source: String) -> Script {
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

    let script = parser.parse_script().expect("script to parse");
    script
}

pub fn traverse(source: String) -> IR {
    let _script = to_script(source);

    #[cfg(feature = "link-swc")]
    panic!("at this time, can't parse JS -> IR. leave link-swc off");

    let mut builder = ProgramBuilder::new();

    let print_stub = {
        let (mut print_stub, [any]) = builder.start_function("print_stub");

        let print = builder.external_function(
            "jssatrt_print_any",
            [FFIValueType::Runtime, FFIValueType::Any],
            FFIReturnType::Void,
        );

        let mut block = print_stub.start_block_main();
        let runtime = block.get_runtime();
        block.call_external_function(print, [runtime, any]);
        print_stub.end_block(block.ret(None));

        builder.end_function(print_stub)
    };

    let main = {
        let mut main = builder.start_function_main();

        let hello_world = builder.constant_str_utf16("hello_world", "Hello, World!".into());

        let mut block = main.start_block_main();
        let hello_world = block.make_string(hello_world);
        block.call(print_stub, [hello_world]);
        main.end_block(block.ret(None));

        builder.end_function(main)
    };

    builder.finish()
}
