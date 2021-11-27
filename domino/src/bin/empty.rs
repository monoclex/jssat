//! Runs an instance of Domino the Debugger, with no data.

use std::collections::HashSet;

use domino::moment::MomentApi;

fn main() {
    println!("launching on http://localhost:8000");

    let mut builder = jssat_ir::frontend::builder::ProgramBuilder::new();

    let (mut secondary, [a, b]) = builder.start_function();
    secondary.with_name("Secondary".to_string());

    let mut e = secondary.start_block_main();
    e.comment("ok");
    let value = e.make_number_decimal(5);
    e.record_set_prop(a, b, value);
    secondary.end_block(e.ret(Some(a)));

    let secondary = builder.end_function(secondary);

    let mut main = builder.start_function_main();
    main.with_name("Primary".to_string());

    let mut e = main.start_block_main();
    let a = e.record_new();
    let b = e.record_new();
    let f = e.make_fnptr(secondary.id);
    let c = e.call_virt_with_result(f, [a, b]);
    e.comment("lol");
    main.end_block(e.ret(Some(c)));

    builder.end_function(main);

    let ir = builder.finish();
    let dealer = ir.dealer.clone();
    let lifted = jssat_ir::lifted::lift(ir);

    let mut moment = MomentApi::new(&lifted);

    let child_fn = **lifted
        .functions
        .keys()
        .collect::<HashSet<_>>()
        .difference(&[lifted.entrypoint].iter().collect::<HashSet<_>>())
        .next()
        .unwrap();

    moment.enter(lifted.entrypoint);
    moment.snapshot(0, None, Default::default());
    moment.snapshot(1, None, Default::default());
    moment.snapshot(2, None, Default::default());
    moment.snapshot(3, None, Default::default());
    moment.enter(child_fn);
    moment.snapshot(0, None, Default::default());
    moment.snapshot(1, None, Default::default());
    moment.snapshot(2, None, Default::default());
    moment.snapshot(3, None, Default::default());
    moment.exit();
    moment.snapshot(4, None, Default::default());
    moment.snapshot(5, None, Default::default());
    moment.exit();

    domino::launch("127.0.0.1:8000", &moment.into_data(dealer)).unwrap();
}
