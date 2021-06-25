#![feature(command_access)]
#![feature(box_patterns)]
#![feature(bool_to_option)]

use std::{io::Write, process::Command};

use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::parser::{Parser, Syntax};

pub mod backend;
pub mod frontend;
pub mod id;
pub mod name;

fn preview(command: &Command) -> String {
    let mut preview = String::new();

    preview.push_str(command.get_program().to_str().unwrap());

    for arg in command.get_args() {
        preview.push_str("\n\t");
        preview.push_str(arg.to_str().unwrap());
    }

    preview
}

fn link_binary(build: &[u8]) {
    let runtime_library = include_bytes!(env!("JSSATRT_PATH"));
    println!("included runtime size: {}", runtime_library.len());

    let mut runtime_object = tempfile::NamedTempFile::new().unwrap();
    let mut build_object = tempfile::NamedTempFile::new().unwrap();

    runtime_object.write_all(runtime_library).unwrap();
    build_object.write_all(build).unwrap();

    #[cfg(target_os = "windows")]
    let artifact = "jssatout.exe";
    #[cfg(target_os = "linux")]
    let artifact = "jssatout";

    let mut build = cc::Build::new();

    // sensible defaults for `OPT_LEVEL`, `TARGET`, and `HOST`
    if let Err(_) = std::env::var("OPT_LEVEL") {
        build.opt_level(3);
    }

    if let Err(_) = std::env::var("TARGET") {
        build.target(crate::backend::llvm::target_triplet().as_str());
    }

    if let Err(_) = std::env::var("HOST") {
        build.host(crate::backend::llvm::target_triplet().as_str());
    }

    build.flag_if_supported("-flto");

    let mut command = build.get_compiler().to_command();

    command.arg("-o").arg(artifact);

    #[cfg(target_os = "linux")]
    {
        command
            .arg(format!("{}", build_object.path().display()))
            .arg(format!("{}", runtime_object.path().display()))
            .arg("-pthread")
            .arg("-ldl");
    }

    // // TODO: is this the right way to add `advapi32` to the linker flags?
    // //       we need advapi32 for mimalloc
    // #[cfg(target_os = "windows")]
    // build.object("advapi32");

    // build.file(runtime_object.path()).file(build_object.path());

    // build.compile(artifact);

    eprintln!("invoking: {}", preview(&command));
    assert!(command.spawn().unwrap().wait().unwrap().success());

    #[cfg(not(any(target_os = "linux", target_os = "windows")))]
    std::compile_error!("unimplemented platform");
}

fn main() {
    // let file_name = std::env::args()
    //     .into_iter()
    //     .skip(1)
    //     .next()
    //     .expect("expected file name");

    // let content = std::fs::read_to_string(file_name).expect("expected to read file");
    let content = "print('Hello, World!');".to_owned();

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

    let ir = frontend::js::traverse(script);
    eprintln!("{:#?}", ir);

    let annotations = frontend::type_annotater::annotate(&ir);
    eprintln!("{:#?}", annotations);

    let build = backend::compile(&ir, &annotations);
    eprintln!("OUTPUT LLVM IR (use unix pipes to redirect this into a file):");
    println!("{}", build.llvm_ir);

    link_binary(build.obj.as_slice());
}
