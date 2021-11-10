//! A macro crate the parses ECMAScript Grammar Notation, and generates
//! structures.

#![feature(proc_macro_span)]
#![feature(extend_one)]
#![feature(proc_macro_diagnostic)]

use proc_macro::*;

#[proc_macro]
pub fn grammar_notation_helper(tokens: TokenStream) -> TokenStream {
    tokens
}
