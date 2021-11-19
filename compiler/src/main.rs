#![feature(command_access)]
#![feature(box_patterns)]
#![feature(bool_to_option)]
#![feature(const_panic)]
#![feature(const_option)]
#![feature(const_fn_trait_bound)]
#![feature(box_syntax)]
#![feature(bindings_after_at)]
#![feature(entry_insert)]
#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(unsafe_cell_raw_get)]
#![feature(allocator_api)]
#![deny(clippy::disallowed_method)]
#![feature(slice_pattern)]
#![feature(associated_type_bounds)]

use std::{io::Write, process::Command, time::Instant};

use crate::frontend::{
    builder::ProgramBuilder,
    js::{ast::parse_nodes::Visitor, ecmascript::ECMA262Methods},
};

pub mod backend;
pub mod codegen;
pub mod collections;
pub mod frontend;
pub mod id;
pub mod interner;
pub mod interpreter;
pub mod isa;
pub mod lifted;
pub mod my_tests;
pub mod opt;
pub mod retag;
pub mod symbolic_execution;
pub mod types;

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
    #[track_caller]
    fn expect_none(self, msg: &str) {
        assert!(
            matches!(self, bimap::Overwritten::<L, R>::Neither),
            "{}",
            msg
        );
    }

    #[track_caller]
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

    // let content = std::fs::read_to_string(file_name).expect("expected to read
    // file");
    let content = r#"
/*
function x() {
    
}

x();
*/
print('Hello, World!');

"#
    .to_owned();

    println!("converting script to parse nodes");
    let script = time(|| frontend::js::ast::parse_script(&content));
    println!("traversed script: {:#?}", script);

    let mut builder = ProgramBuilder::new();
    let mut f = builder.start_function_main();
    let main_id = f.id;
    let mut b = f.start_block_main();

    let threaded_global = b.record_new();

    let methods = ECMA262Methods::new(&mut builder);

    println!("visiting parse nodes of script");
    let entry_parse_node = time(|| {
        frontend::js::ast::emit_nodes(&mut builder, &mut b, &methods, |visitor| {
            visitor.visit_script(&script)
        })
    });

    b.call(methods.InitializeJSSATThreadedGlobal, [threaded_global]);

    b.call(methods.InitializeHostDefinedRealm, [threaded_global]);

    // for some reason, ecmascript spec doesn't have initializehostdefinedrealm
    // return the realm okay?
    let exec_ctx_stack = b.record_get_slot(
        threaded_global,
        isa::InternalSlot::JSSATExecutionContextStack,
    );
    let first = b.make_number_decimal(0);
    let context = b.list_get(exec_ctx_stack, first);
    let realm = b.record_get_slot(context, isa::InternalSlot::Realm);

    // we don't care about the parameters we pass null to for ParseScript
    let null = b.make_null();
    let script_context = b.call_with_result(
        methods.ParseScript,
        [threaded_global, null, realm, null, entry_parse_node],
    );

    b.call(methods.ScriptEvaluation, [threaded_global, script_context]);

    f.end_block(b.ret(None));
    builder.end_function(f);
    let ir = builder.finish();

    println!("{}", crate::frontend::display_jssatir::display(&ir));

    println!("lifting program");
    let program = time(move || lifted::lift(ir));

    println!("executing program");
    // let program = time(|| symbolic_execution::execute(&program));
    let interpreter_result = time(|| {
        let builder = crate::interpreter::InterpreterBuilder::new(&program);
        let interpreter = builder.build();

        interpreter.execute_fn_id(program.entrypoint, vec![])
    });

    println!(
        "executed: {:?}",
        match interpreter_result {
            Ok(_) => "success".to_string(),
            Err(err) => format!("error: {}", err),
        }
    );

    panic!("done");
    let program = time(|| symbolic_execution::execute(&program));

    println!("typing program");
    let program = time(move || codegen::type_program(program));

    println!("optimizing system run");
    let program = time(move || opt::opt(program));

    println!("{}", codegen::display_typed(&program));

    println!("lowering run");
    let program = time(move || codegen::lower(program));

    println!("{}", codegen::display_program(&program));

    println!("compiling");
    let build = time(move || backend::compile(program));

    eprintln!("OUTPUT LLVM IR (use unix pipes to redirect this into a file):");
    println!("{}", build.llvm_ir);

    link_binary(build.obj.as_slice());
}

fn time<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    let start = Instant::now();
    let result = f();
    let end = Instant::now();
    println!("took {:?}", end - start);
    println!();
    result
}
