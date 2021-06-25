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

    let utf16_message = "Hello, World!"
        .encode_utf16()
        .into_iter()
        .flat_map(|x| std::array::IntoIter::new(x.to_ne_bytes()))
        .collect::<Vec<_>>();

    let hello_world = builder.constant(DebugName::new("hello_world"), utf16_message);
    let print = builder.external_function(
        "jssatrt_print_any".into(),
        vec![FFIValueType::Runtime, FFIValueType::Any],
        FFIReturnType::Void,
    );

    let mut print_stub = builder.start_function(DebugName::new("print_stub"));
    let print_stub_id = print_stub.id;
    {
        let mut block = print_stub.start_block();
        block.mark_entrypoint();
        let jssatrt = block.get_runtime();
        let print_string = print_stub.parameter(DebugName::new("string"));
        // block.call(print_stub_id, vec![print_string]);
        block.call_external_function(print, vec![jssatrt, print_string]);
        print_stub.end_block(block.ret(None));
    }
    builder.end_function(print_stub);

    let mut recurse_me = builder.start_function(DebugName::new("recurse_me"));
    let recurse_me_id = recurse_me.id;
    {
        let mut block = recurse_me.start_block();
        block.mark_entrypoint();
        let arg = recurse_me.parameter(DebugName::new("argument"));
        let result = block.call_with_result(recurse_me_id, vec![arg]);
        recurse_me.end_block(block.ret(Some(result)));
    }
    builder.end_function(recurse_me);

    let mut main = builder.start_function(DebugName::new("main"));
    main.mark_main();
    {
        let mut entry = main.start_block();
        entry.mark_entrypoint();
        let string = entry.make_string(hello_world);
        // let string = entry.call_with_result(recurse_me_id, vec![string]);
        entry.call(print_stub_id, vec![string]);
        main.end_block(entry.ret(None));
    }
    builder.end_function(main);

    builder.finish()
}
