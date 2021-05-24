#![feature(option_result_contains)]

use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::parser::{Parser, Syntax};

pub mod ast_traversal;
pub mod backend;
pub mod ir;
pub mod types;

fn main() {
    let ir = ir::builder::ex();
    eprintln!("{:#?}", ir);

    let annotations = types::annotate(&ir);
    eprintln!("{:#?}", annotations);

    let llvm_ir = backend::build(&ir, &annotations);
    eprintln!("OUTPUT LLVM IR (use unix pipes to redirect this into a file):");
    println!("{}", llvm_ir);

    let runtime_library = include_bytes!(env!("JSSATRT_PATH"));
    println!("included runtime size: {}", runtime_library.len());

    let file_name = std::env::args()
        .into_iter()
        .skip(1)
        .next()
        .expect("expected file name");

    let content = std::fs::read_to_string(file_name).expect("expected to read file");

    // https://github.com/Starlight-JS/starlight/blob/4c4ce5d0178fb28c3b2a044d572473baaf057b73/crates/starlight/src/vm.rs#L275-L300
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));
    let fm = cm.new_source_file(FileName::Anon, content);

    let mut parser = Parser::new(
        Syntax::Es(Default::default()),
        StringInput::from(fm.as_ref()),
        None,
    );

    for err in parser.take_errors() {
        err.into_diagnostic(&handler).emit();
    }

    let script = parser.parse_script().expect("script to parse");

    println!("{:#?}", script);
}
