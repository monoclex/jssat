#![feature(with_options)]

use std::{
    io::{BufWriter, Write},
    path::Path,
};

use walkdir::WalkDir;

fn main() {
    compile_irfiles();
    link_jssatrt();
}

enum GenKind {
    ParseNodes,
    IrFile,
}

fn compile_irfiles() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    for entry in WalkDir::new("./src/") {
        let entry = entry.unwrap();
        let name = entry.file_name().to_str().unwrap();

        let mut dest = None;
        if name == "parse_nodes.json" {
            let dest_path = Path::new(&out_dir).join("parse_nodes.rs");
            dest = Some((dest_path, GenKind::ParseNodes, "parse_nodes".to_string()));
        }

        if name.ends_with(".lisp") {
            let name = name.split('.').next().unwrap().to_string();
            let dest_path = Path::new(&out_dir).join(format!("{}_irfile.rs", name));
            dest = Some((dest_path, GenKind::IrFile, name));
        }

        if let Some((dest_path, kind, name)) = dest {
            let mut output = BufWriter::new(
                std::fs::File::with_options()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(dest_path)
                    .unwrap(),
            );

            let src = std::fs::read_to_string(entry.path()).unwrap();
            println!("cargo:rerun-if-changed={}", entry.path().to_str().unwrap());

            let code = match kind {
                GenKind::IrFile => ir_file::generate(&name, &src),
                GenKind::ParseNodes => grammar_notation_helper::generate(&src),
            };

            output.write_all(code.as_bytes()).unwrap();
        }
    }
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
