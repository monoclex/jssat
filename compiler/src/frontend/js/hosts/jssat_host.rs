use super::HostEnvironment;

/// JSSAT host environment - this hooks functions useful
pub struct JSSATHostEnvironment {}

impl JSSATHostEnvironment {
    pub fn new() -> Self {
        Self {}
    }
}

impl HostEnvironment for JSSATHostEnvironment {
    fn inject<'s>(&mut self, hook: super::HostHookState<'s>) {
        let super::HostHookState {
            ecma_methods,
            program,
            block,
            threaded_global,
            global_object,
            ..
        } = hook;

        let print = program.constant_str_utf16("print");
        let print = block.make_string(print);

        // TODO: right now we can't capture any state if we need to
        //       this is a function pointer put straight into [[Call]]
        let print_fn = {
            let (mut print_fn, [_threaded_global, _function_object, _this_value, _arguments_list]) =
                program.start_function();

            let mut main = print_fn.start_block_main();

            let fail_assertion = main.make_bool(false);
            main.assert(fail_assertion, "Called the `print_fn`!");

            print_fn.end_block(main.ret(None));

            program.end_function(print_fn)
        };

        let print_fn_ptr = block.make_fnptr(print_fn.id);

        let length = block.make_number_decimal(1);
        let name = print;
        let additional_slots = block.list_new();
        let undef = block.make_undefined();
        let print_fn = block.call_with_result(
            ecma_methods.CreateBuiltinFunction,
            [
                threaded_global,
                print_fn_ptr,
                length,
                name,
                additional_slots,
                undef,
                undef,
                undef,
            ],
        );

        block.call(
            ecma_methods.CreateDataProperty,
            [threaded_global, global_object, print, print_fn],
        );
    }
}
