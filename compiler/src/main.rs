#![feature(command_access)]
#![feature(box_patterns)]
#![feature(bool_to_option)]
#![feature(const_panic)]
#![feature(const_fn)]
#![feature(const_option)]
#![feature(const_fn_trait_bound)]
#![feature(box_syntax)]
#![feature(bindings_after_at)]
#![feature(entry_insert)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(unsafe_cell_raw_get)]
#![deny(clippy::disallowed_method)]

use std::{io::Write, process::Command};

use crate::frontend::{display::display, display_jssatir};

pub mod backend;
pub mod frontend;
pub mod id;
pub mod interner;
pub mod isa;
pub mod lifted;
pub mod my_tests;
pub mod poor_hashmap;
pub mod retag;
pub mod symbolic_execution;

/// can't have nice things :'( https://github.com/rust-lang/rust/issues/62633
pub trait UnwrapNone {
    fn expect_none(self, msg: &str);

    // lazy/hacky code but w/e
    fn expect_free(self);
}

impl<T> UnwrapNone for Option<T> {
    #[track_caller]
    fn expect_none(self, msg: &str) {
        assert!(matches!(self, None), "{}", msg);
    }

    #[track_caller]
    fn expect_free(self) {
        self.expect_none("must be free insertion slot");
    }
}

impl<L, R> UnwrapNone for bimap::Overwritten<L, R> {
    fn expect_none(self, msg: &str) {
        assert!(
            matches!(self, bimap::Overwritten::<L, R>::Neither),
            "{}",
            msg
        );
    }

    fn expect_free(self) {
        self.expect_none("must be free insertion slot");
    }
}

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
    if std::env::var("OPT_LEVEL").is_err() {
        build.opt_level(3);
    }

    if std::env::var("TARGET").is_err() {
        build.target(crate::backend::llvm::target_triplet().as_str());
    }

    if std::env::var("HOST").is_err() {
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

    let ir = frontend::js::traverse(content);
    println!("{}", display_jssatir::display(&ir));

    let lifted = lifted::lift(ir);
    let lifted_ref = Box::leak(Box::new(lifted));
    let program = symbolic_execution::execute(lifted_ref);

    println!("=== PRE OPT ===");
    println!("{}", display(&program));

    let mut program = program;

    // this will probably run our opts enough
    for _ in 0..4 {
        program = frontend::asm_opt_const_elim::opt_constant_elimination(program);
        println!("=== MID OPT1 ===");
        println!("{}", display(&program));

        program = frontend::asm_opt_dead_register_elim::opt_dead_register_elimination(program);
        println!("=== MID OPT2 ===");
        println!("{}", display(&program));
    }

    println!("=== POST OPT ===");
    println!("{}", display(&program));

    let build = backend::compile(program);
    eprintln!("OUTPUT LLVM IR (use unix pipes to redirect this into a file):");
    println!("{}", build.llvm_ir);

    link_binary(build.obj.as_slice());
}
