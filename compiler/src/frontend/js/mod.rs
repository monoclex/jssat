mod frontend_pseudo;

// #[cfg(not(feature = "link-swc"))]
pub use frontend_pseudo::traverse;

pub mod ecmascript;

/*
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::{
    ast::Script,
    parser::{Parser, Syntax},
};

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

    parser.parse_script().expect("script to parse")
}
*/
