use crate::frontend::{builder::*, ir::*};

use crate::isa::InternalSlot;

// might be vaguely useful in the future as tutorial code or something
#[allow(dead_code)]
pub fn traverse(_source: String) -> IR {
    let mut builder = ProgramBuilder::new();

    let print_stub = {
        let (mut print_stub, [any]) = builder.start_function();

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

    let sum = {
        let (mut sum, [n]) = builder.start_function();

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
                print(li)
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

    let print_is_cool_and_key = {
        let (mut func, [record]) = builder.start_function();

        let mut block = func.start_block_main();

        let key = block.make_string(builder.constant_str_utf16("key"));
        let prop_at_key = block.record_get_prop(record, key);
        let prop_at_slot = block.record_get_slot(record, InternalSlot::HostDefined);
        let call_it = block.record_get_slot(record, InternalSlot::Call);
        block.call(print_stub, [prop_at_key]);
        block.call(print_stub, [prop_at_slot]);
        block.call_virt(call_it, [key]);

        func.end_block(block.ret(None));
        builder.end_function(func)
    };

    let _main = {
        let mut main = builder.start_function_main();

        let mut block = main.start_block_main();

        // TODO: make compiling `sum` and hello world a test
        let hello_world = builder.constant_str_utf16("Hello, World!");
        let hello_world = block.make_string(hello_world);
        block.call(print_stub, [hello_world]);
        let max = block.make_number_decimal(3);
        block.call(sum, [max]);

        let obj = block.record_new();
        let key = block.make_string(builder.constant_str_utf16("key"));
        let value = block.make_number_decimal(69);
        block.record_set_prop(obj, key, value);
        let value = block.make_number_decimal(1);
        block.record_set_slot(obj, InternalSlot::HostDefined, value);
        let fnptr = block.make_fnptr(print_stub.id);
        block.record_set_slot(obj, InternalSlot::Call, fnptr);

        block.call(print_is_cool_and_key, [obj]);

        main.end_block(block.ret(None));

        builder.end_function(main)
    };

    builder.finish()
}
