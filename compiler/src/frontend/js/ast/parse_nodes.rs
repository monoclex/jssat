with_builtin_macros::with_builtin! {
    let $path = concat!(env!("OUT_DIR"), "/parse_nodes.rs") in {
        #[path = $path]
        mod generated_code;
        pub use generated_code::*;
    }
}
