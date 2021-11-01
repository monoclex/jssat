#![feature(with_options)]

use std::{
    io::{BufWriter, Write},
    path::Path,
};

fn main() {
    compile_ecmascript_in_jssat_vm();
    link_jssatrt();
}

fn compile_ecmascript_in_jssat_vm() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("ecma262_irfile.rs");
    let mut output = BufWriter::new(
        std::fs::File::with_options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(dest_path)
            .unwrap(),
    );

    let ecmascript_src =
        std::fs::read_to_string("./src/frontend/js/ecmascript/ecma262_irfile.lisp").unwrap();
    println!("cargo:rerun-if-changed=src/frontend/js/ecmascript/ecma262_irfile.lisp");

    let code = ir_file::generate(&ecmascript_src);

    output.write_all(code.as_bytes()).unwrap();
}

fn link_jssatrt() {
    // hack:
    // we want the `./target/release/` folder that contains the runtime
    // we might be in `./target/x86-64-some-target-triplet/debug` or etc,
    // so we get the directoy relative to the `OUT_DIR`.
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let out_dir = Path::new(out_dir.as_str());
    let build_artifact_dir = out_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap();

    #[cfg(target_os = "windows")]
    let lib_name = "jssatrt.lib";
    #[cfg(target_os = "linux")]
    let lib_name = "libjssatrt.a";

    let artifact = build_artifact_dir.join(lib_name);

    if !artifact.exists() {
        panic!(
            "build artifact `{}` does not exist at `{}`. did you invoke `cargo make build`?",
            lib_name,
            artifact.display()
        );
    }

    eprintln!("runtime artifact: {}", artifact.display());
    println!("cargo:rustc-env=JSSATRT_PATH={}", artifact.display());
}
