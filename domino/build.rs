use std::{path::Path, process::Command};

fn main() {
    let status = Command::new("npm")
        .arg("run")
        .arg("build")
        .current_dir("./webapp")
        .status()
        .unwrap();

    if !status.success() {
        panic!("build unsuccessful");
    }

    let webapp = std::fs::canonicalize("./webapp/dist").unwrap();

    println!("cargo:rustc-env=INDEX_HTML={}/index.html", webapp.display());
    println!("cargo:rustc-env=INDEX_JS={}/index.js", webapp.display());
}
