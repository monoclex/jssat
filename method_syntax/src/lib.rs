//! A helper crate for ergonomically writing JSSAT IR functions as methods.
//!
//! # `method_syntax`
//!
//! When transcribing the ECMA262 specification to JSSAT, it would be
//! ergonomically pleasing to be able to write each individual ECMAScript method
//! as a function. However, there is a lot of boilerplate with such ergonomics.
//! This crate exists to automatically transform ergonomic, method-looking code
//! into code that generates every function acordingly.
//!
//! # Example
//!
//! ```
//! method_syntax! {
//!     fn CallX(value) {
//!         let result = e.call(self.X);
//!         e.finish(Some(result))
//!     }
//!
//!     fn X(value) {
//!         e.finish(Some(value))
//!     }
//! }
//! ```
//!
//! would get transformed into
//!
//! ```
//! struct Methods {
//!     CallX: FnSignature<1>,
//!     X: FnSignature<1>,
//! }
//!
//! impl Methods {
//!     fn new(program: &mut ProgramBuilder) -> Self {
//!         let CallX = program.start_function();
//!         let X = program.start_function();
//!
//!         let signature_CallX = CallX.0.signature();
//!         let signature_X = X.0.signature();
//!
//!         let methods = Methods {
//!             CallX: signature_CallX,
//!             X: signature_X,
//!         };
//!
//!         methods.ImplCallX(Emitter::new(program, CallX.0), CallX.1);
//!         methods.ImplX(Emitter::new(program, X.0), X.1);
//!
//!         methods
//!     }
//!
//!     fn CallX(&self, e: Emitter<1>, [value]: [RegisterId; 1]) -> FnSignature<1> {
//!         let result = e.call(self.X);
//!         e.finish(Some(result))
//!     }
//!
//!     fn X(&self, e: Emitter<1>, [value]: [RegisterId; 1]) -> FnSignature<1> {
//!         e.finish(Some(value))
//!     }
//! }
//! ```

#![feature(proc_macro_span)]
#![feature(extend_one)]
#![feature(proc_macro_diagnostic)]

use proc_macro::*;

trait TSExt {
    fn ident(self) -> Ident;
    fn group(self) -> Group;
    fn punct(self) -> Punct;
}

impl TSExt for TokenTree {
    #[track_caller]
    fn ident(self) -> Ident {
        match self {
            TokenTree::Ident(ident) => ident,
            other => panic!("expected identifier, got {:?}", other),
        }
    }

    #[track_caller]
    fn group(self) -> Group {
        match self {
            TokenTree::Group(group) => group,
            other => panic!("expected group, got {:?}", other),
        }
    }

    #[track_caller]
    fn punct(self) -> Punct {
        match self {
            TokenTree::Punct(punct) => punct,
            other => panic!("expected punctuation, got {:?}", other),
        }
    }
}

macro_rules! makero {
    ($l: expr) => {{
        let tt: TokenStream = $l.parse().unwrap();
        tt
    }};
}

#[proc_macro]
pub fn method_syntax(tokens: TokenStream) -> TokenStream {
    // TODO(refactor): i wrote this macro when i didn't have internet, i'm
    //     positive there are crates that i could use that would make this waaay
    //     easier and cleaner

    let mut tokens = tokens.into_iter();

    let mut methods = vec![];

    while let Some(_fn_keyword) = tokens.next() {
        let name = tokens.next().expect("next should be ident").ident();
        let args = tokens.next().expect("next should be arguments").group();
        let body = tokens.next().expect("next should be body").group();

        let mut arg_names = vec![];

        let mut args = args.stream().into_iter().collect::<Vec<_>>();

        match args.len() {
            0 => {}
            1 => arg_names.push(args.remove(0)),
            _ => {
                arg_names.push(args.remove(0));

                let i = 0;
                while i < args.len() {
                    let args_ip1 = args.remove(0);
                    let args_i = args.remove(i);

                    arg_names.push(args_i);

                    let comma = args_ip1.punct();

                    if comma.span().source_text() != Some(",".into()) {
                        let mut error = Diagnostic::new(Level::Error, "expected comma");
                        error.set_spans(comma.span());
                        error.emit();
                        return TokenStream::new();
                    }

                    // i += 2;
                }
            }
        }

        methods.push((name, arg_names, body));
    }

    let mut generated_code = TokenStream::new();

    generated_code.extend(makero!("struct Methods"));

    let mut inner_methods = TokenStream::new();
    for (name, args, _) in methods.iter() {
        inner_methods.extend(makero!("    "));
        inner_methods.extend_one(TokenTree::Ident(name.clone()));
        inner_methods.extend(makero!(format!(": FnSignature<{}>,\n", args.len())));
    }

    generated_code.extend_one(TokenTree::Group(Group::new(
        Delimiter::Brace,
        inner_methods,
    )));

    generated_code.extend(makero!("impl Methods"));
    let mut inner_methods = TokenStream::new();

    // Methods::new():
    let mut methods_new = TokenStream::new();
    for (name, _, _) in methods.iter() {
        let raw_name = name.span().source_text().unwrap();

        methods_new.extend_one(makero!("let "));
        methods_new.extend_one(TokenTree::Ident(name.clone()));
        methods_new.extend_one(makero!(" = program.start_function();\n"));

        methods_new.extend_one(makero!(format!("let signature_{} = ", raw_name)));
        methods_new.extend_one(TokenTree::Ident(name.clone()));
        methods_new.extend_one(makero!(".0.signature();\n"));
    }

    let mut methods_constructor = TokenStream::new();
    for (name, _, _) in methods.iter() {
        let raw_name = name.span().source_text().unwrap();

        methods_constructor.extend_one(TokenTree::Ident(name.clone()));
        methods_constructor.extend_one(makero!(format!(": signature_{},\n", raw_name)));
    }
    let methods_constructor = TokenTree::Group(Group::new(Delimiter::Brace, methods_constructor));

    methods_new.extend_one(makero!("let methods = Methods "));
    methods_new.extend_one(methods_constructor);
    methods_new.extend_one(makero!(";\n\n"));

    for (name, _, _) in methods.iter() {
        let raw_name = name.span().source_text().unwrap();

        methods_new.extend_one(makero!("methods."));
        methods_new.extend_one(TokenTree::Ident(name.clone()));
        methods_new.extend_one(makero!(format!(
            "(Emitter::new(program, {}.0), {}.1);\n",
            raw_name, raw_name
        )));
    }

    methods_new.extend_one(makero!("methods"));

    inner_methods.extend(makero!("pub fn new(program: &mut ProgramBuilder) -> Self "));
    inner_methods.extend_one(TokenTree::Group(Group::new(Delimiter::Brace, methods_new)));
    // done methods_new

    // every individual method
    for (name, args, body) in methods.iter() {
        let raw_name = name.span().source_text().unwrap();
        let num_args = args.len();

        inner_methods.extend_one(makero!("fn"));
        inner_methods.extend_one(TokenTree::Ident(name.clone()));

        let mut destructed_args = TokenStream::new();
        let mut i = args.len();
        for arg in args {
            destructed_args.extend_one(arg.clone());

            if i >= 1 {
                destructed_args.extend_one(makero!(","));
                i -= 1;
            }
        }
        let destructed_args = TokenTree::Group(Group::new(Delimiter::Bracket, destructed_args));

        let mut args = TokenStream::new();
        args.extend_one(makero!(format!("&self, mut e: Emitter<{}>, ", num_args)));
        args.extend_one(destructed_args);
        args.extend_one(makero!(format!(": [RegisterId; {}]", num_args)));

        inner_methods.extend_one(TokenTree::Group(Group::new(Delimiter::Parenthesis, args)));
        inner_methods.extend_one(makero!(format!("-> FnSignature<{}>", num_args)));
        inner_methods.extend_one(TokenTree::Group(body.clone()));
    }

    // done all methods
    generated_code.extend_one(TokenTree::Group(Group::new(
        Delimiter::Brace,
        inner_methods,
    )));

    generated_code
}
