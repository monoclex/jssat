#[cfg(feature = "link-swc")]
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
#[cfg(feature = "link-swc")]
use swc_ecmascript::{
    ast::Script,
    parser::{Parser, Syntax},
};

use crate::frontend::{builder::*, ir::*};

#[cfg(not(feature = "link-swc"))]
fn to_script(_source: String) {}

#[cfg(feature = "link-swc")]
fn to_script(source: String) -> Script {
    // https://github.com/Starlight-JS/starlight/blob/4c4ce5d0178fb28c3b2a044d572473baaf057b73/crates/starlight/src/vm.rs#L275-L300
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));
    let fm = cm.new_source_file(FileName::Anon, source);

    let mut parser = Parser::new(
        Syntax::Es(Default::default()),
        StringInput::from(fm.as_ref()),
        None,
    );

    for err in parser.take_errors() {
        err.into_diagnostic(&handler).emit();
    }

    let script = parser.parse_script().expect("script to parse");
    script
}

pub fn traverse(source: String) -> IR {
    let _script = to_script(source);

    #[cfg(feature = "link-swc")]
    panic!("at this time, can't parse JS -> IR. leave link-swc off");

    let mut builder = ProgramBuilder::new();

    let print_stub = {
        let (mut print_stub, [any]) = builder.start_function("print_stub");

        let print = builder.external_function(
            "jssatrt_print_any",
            [FFIValueType::Runtime, FFIValueType::Any],
            Returns::Void,
        );

        let mut block = print_stub.start_block_main();
        let runtime = block.get_runtime();
        block.call_external_function(print, [runtime, any]);
        print_stub.end_block(block.ret(None));

        builder.end_function(print_stub)
    };

    let _fib = {
        // let (mut fib, [n]) = builder.start_function("fib");

        /*
        def fib(n):
            if n == 0:
                ret 0
            if n == 1:
                ret 1
            ret fib(n - 2) + fib(n - 1)

        fib(n):
            Entry():
                is_zero = n < 1 // not implementing `==` rn lmfao
                if (is_zero)
                    jmp RetZero()
                else
                    jmp CheckOne()
            RetZero():
                ret 0
            CheckOne():
                is_one = n < 2
                if (is_one)
                    jmp RetOne()
                else
                    jmp ComputeFib()
            RetOne():
                ret 1
            ComputeFib():
                p0 = n - 2
                lhs = call fib(p0)
                p1 = n - 1
                rhs = call fib(p1)
                result = lhs + rhs
                ret result
        */
    };

    let sum = {
        let (mut sum, [n]) = builder.start_function("sum");

        /*
        def sum(n):
            total = 0
            for i in range(0, n):
                total += i
            return total

        sum(n):
            Entry():
                i = 0
                total = 0
                jmp Check(i, total)
            Check(ci, ctotal):
                should_iterate = ci < n
                if (should_iterate)
                    jmp Loop(ci, ctotal)
                else
                    jmp End(ctotal)
            Loop(li, ltotal):
                print(li) # add stateful effect so the optimizer doesn't optimize everything
                total2 = ltotal + li
                i2 = li + 1
                jmp Check(i2, total2)
            End(etotal):
                ret etotal
        */
        let mut entry = sum.start_block_main();
        let (mut check, [ci, ctotal]) = sum.start_block();
        let (mut bloop, [li, ltotal]) = sum.start_block();
        let (end, [etotal]) = sum.start_block();

        let check_sig = check.signature();

        {
            let i = entry.make_number_decimal(0);
            let total = entry.make_number_decimal(0);
            sum.end_block(entry.jmp(check.signature(), [i, total]));
        }
        {
            let should_iterate = check.compare_less_than(ci, n);
            sum.end_block(check.jmpif(
                should_iterate,
                bloop.signature(),
                [ci, ctotal],
                end.signature(),
                [ctotal],
            ));
        }
        {
            bloop.call(print_stub, [li]);
            let total2 = bloop.add(ltotal, li);
            let one = bloop.make_number_decimal(1);
            let i2 = bloop.add(li, one);
            sum.end_block(bloop.jmp(check_sig, [i2, total2]));
        }
        {
            sum.end_block(end.ret(Some(etotal)));
        }

        builder.end_function(sum)
    };

    let _main = {
        let mut main = builder.start_function_main();

        let hello_world = builder.constant_str_utf16("hello_world", "Hello, World!".into());

        let mut block = main.start_block_main();
        // let (mut cond, [condition]) = main.start_block();
        // let (mut end, []) = main.start_block();

        // let d0 = block.make_number_decimal(0);
        // let d1 = block.make_number_decimal(1);
        // let b_true = block.compare_less_than(d0, d1);
        // let b_false = block.compare_less_than(d1, d0);

        // let cond_sig = cond.signature();
        // main.end_block(block.jmp(cond_sig, [b_false]));

        // cond.call(print_stub, [condition]);
        // main.end_block(cond.jmpif(condition, end.signature(), [], cond_sig, [b_true]));

        // main.end_block(end.ret(None));

        let hello_world = block.make_string(hello_world);
        block.call(print_stub, [hello_world]);
        let max = block.make_number_decimal(3);
        block.call(sum, [max]);
        main.end_block(block.ret(None));

        builder.end_function(main)
    };

    builder.finish()
}
