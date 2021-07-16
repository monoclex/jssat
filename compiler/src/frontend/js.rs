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

    parser.parse_script().expect("script to parse")
}

pub fn traverse(source: String) -> IR {
    let script = to_script(source);

    let mut builder = ProgramBuilder::new();

    let mut main = builder.start_function_main();
    let block = main.start_block_main().into_dynamic();
    let mut frontend = JsWriter {
        bld: &mut builder,
        bld_fn: &mut main,
        block,
    };

    frontend.InitializeHostDefinedRealm();

    let block = frontend.block.ret(None);
    main.end_block_dyn(block);

    builder.end_function(main);
    builder.finish()
}

struct JsWriter<'builder, 'name, const PARAMETERS: usize> {
    bld: &'builder mut ProgramBuilder,
    bld_fn: &'builder mut FunctionBuilder<'name, PARAMETERS>,
    block: DynBlockBuilder,
}

#[allow(non_snake_case)]
impl<'b, 'n, const PARAMS: usize> JsWriter<'b, 'n, PARAMS> {
    /// https://tc39.es/ecma262/#sec-returnifabrupt
    pub fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId {
        // 1. If argument is an abrupt completion, return argument.
        let completion_type = self.block.record_get_slot(argument, "Type");
        let normal_completion = self
            .block
            .make_string(self.bld.constant_str("", "normal".into()));
        let is_normal_completion = self.block.compare_equal(completion_type, normal_completion);
        let is_abrupt_completion = self.block.negate(is_normal_completion);

        let on_abrupt_completion = {
            let (on_abrupt_completion, []) = self.bld_fn.start_block();
            self.bld_fn
                .end_block(on_abrupt_completion.ret(Some(argument)))
        };

        // 2. Else if argument is a Completion Record, set argument to argument.[[Value]].

        let (mut on_normal_completion, []) = self.bld_fn.start_block();
        let argument_inner_value = on_normal_completion.record_get_slot(argument, "Value");

        // <== PATCH CONTROL FLOW ==>

        // `self.block`: control flow we're on
        // `on_abrupt_completion`: the path we need to go to if `is_abrupt_completion`
        // `on_normal_completion`: the new path we need to be on if `!is_abrupt_completion`
        //
        // 1. replace `self.block` with `on_normal_completion` so that we are on the new path
        let mut on_normal_completion = on_normal_completion.into_dynamic();
        std::mem::swap(&mut self.block, &mut on_normal_completion);
        let old_self_block = on_normal_completion;
        let on_normal_completion = &mut self.block;
        // 2. end the old `self.block` to go to `on_abrupt_completion`
        self.bld_fn.end_block_dyn(old_self_block.jmpif_dynargs(
            is_abrupt_completion,
            on_abrupt_completion.id,
            vec![],
            on_normal_completion.id,
            vec![],
        ));

        argument_inner_value
    }

    /// https://tc39.es/ecma262/#sec-normalcompletion
    pub fn NormalCompletion(&mut self, argument: RegisterId) -> RegisterId {
        // 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]: empty }.
        let completion_record = self.block.record_new();
        let normal = self
            .block
            .make_string(self.bld.constant_str("", "normal".into()));
        self.block
            .record_set_slot(completion_record, "Type", normal);
        self.block
            .record_set_slot(completion_record, "Value", argument);
        let empty = self
            .block
            .make_string(self.bld.constant_str("", "empty".into()));
        self.block
            .record_set_slot(completion_record, "Target", empty);
        completion_record
    }

    /// https://tc39.es/ecma262/#sec-initializehostdefinedrealm
    pub fn InitializeHostDefinedRealm(&mut self) -> RegisterId {
        // 1. Let realm be CreateRealm().
        let realm = self.CreateRealm();
        // 2. Let newContext be a new execution context.
        let newContext = self.block.record_new();
        // 3. Set the Function of newContext to null.
        let null = self.block.make_null();
        self.block.record_set_slot(newContext, "Function", null);
        // 4. Set the Realm of newContext to realm.
        self.block.record_set_slot(newContext, "Realm", realm);
        // 5. Set the ScriptOrModule of newContext to null.
        self.block
            .record_set_slot(newContext, "ScriptOrModule", null);
        // 6. Push newContext onto the execution context stack; newContext is
        //    now the running execution context.
        // TODO: implement
        // 7. If the host requires use of an exotic object to serve as realm's
        //    global object, let global be such an object created in a
        //    host-defined manner. Otherwise, let global be undefined,
        //    indicating that an ordinary object should be created as the
        //    global object.
        let global = self.block.make_undefined();
        // 8. If the host requires that the this binding in realm's global
        //    scope return an object other than the global object, let
        //    thisValue be such an object created in a host-defined manner.
        //    Otherwise, let thisValue be undefined, indicating that realm's
        //    global this binding should be the global object.
        let thisValue = self.block.make_undefined();
        // 9. Perform SetRealmGlobalObject(realm, global, thisValue).
        self.SetRealmGlobalObject(realm, global, thisValue);
        // 10. Let globalObj be ? SetDefaultGlobalBindings(realm).
        let try_globalObj = self.SetDefaultGlobalBindings(realm);
        let globalObj = self.ReturnIfAbrupt(try_globalObj);
        // 11. Create any host-defined global object properties on globalObj.
        // TODO: implement `print`
        // 12. Return NormalCompletion(empty).
        // TODO: what is `empty`? a string? a zero sized type? idk bro
        let empty = self
            .block
            .make_string(self.bld.constant_str("", "empty".into()));
        self.NormalCompletion(empty)
    }

    /// https://tc39.es/ecma262/#sec-createrealm
    pub fn CreateRealm(&mut self) -> RegisterId {
        // 1. Let realmRec be a new Realm Record.
        let realmRec = self.block.record_new();
        // 2. Perform CreateIntrinsics(realmRec).
        self.CreateIntrinsics(realmRec);
        // 3. Set realmRec.[[GlobalObject]] to undefined.
        let undefined = self.block.make_undefined();
        self.block
            .record_set_slot(realmRec, "GlobalObject", undefined);
        // 4. Set realmRec.[[GlobalEnv]] to undefined.
        self.block.record_set_slot(realmRec, "GlobalEnv", undefined);
        // 5. Set realmRec.[[TemplateMap]] to a new empty List.
        // TODO: add intrinsics for empty lists
        self.block
            .record_set_slot(realmRec, "TemplateMap", undefined);
        // 6. Return realmRec.
        realmRec
    }

    /// https://tc39.es/ecma262/#sec-createintrinsics
    pub fn CreateIntrinsics(&mut self, realmRec: RegisterId) -> RegisterId {
        // 1. Let intrinsics be a new Record.
        let intrinsics = self.block.record_new();
        // 2. Set realmRec.[[Intrinsics]] to intrinsics.
        self.block
            .record_set_slot(realmRec, "Intrinsics", intrinsics);
        // 3. Set fields of intrinsics with the values listed in Table 8.
        //    The field names are the names listed in column one of the table.
        //    The value of each field is a new object value fully and
        //    recursively populated with property values as defined by the
        //    specification of each object in clauses 19 through 28.
        //    All object property values are newly created object values.
        //    All values that are built-in function objects are created by
        //    performing CreateBuiltinFunction(steps, length, name, slots,
        //        realmRec, prototype) where steps is the definition of that
        //    function provided by this specification, name is the initial
        //    value of the function's name property, length is the initial
        //    value of the function's length property, slots is a list of the
        //    names, if any, of the function's specified internal slots, and
        //    prototype is the specified value of the function's [[Prototype]]
        //    internal slot.
        //    The creation of the intrinsics and their properties must be
        //    ordered to avoid any dependencies upon objects that have not yet
        //    been created.
        // TODO: implement this
        // 4. Perform AddRestrictedFunctionProperties(intrinsics.[[%Function.prototype%]], realmRec).
        // TODO: support getting properties that don't exist without failing
        // let F = block.record_get_slot(intrinsics, "%Function.prototype%");
        // self.AddRestrictedFunctionProperties(block, F, realmRec);
        // 5. Return intrinsics.
        intrinsics
    }

    // https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
    pub fn AddRestrictedFunctionProperties(&mut self, F: RegisterId, realm: RegisterId) {
        // TODO: implement this
    }

    // https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
    pub fn SetRealmGlobalObject(
        &mut self,
        realmRec: RegisterId,
        globalObj: RegisterId,
        thisValue: RegisterId,
    ) {
        // TODO: implement this
    }

    // https://tc39.es/ecma262/#sec-setdefaultglobalbindings
    pub fn SetDefaultGlobalBindings(&mut self, realmRec: RegisterId) -> RegisterId {
        // 1. Let global be realmRec.[[GlobalObject]].
        let global = self.block.record_get_slot(realmRec, "GlobalObject");
        // 2. For each property of the Global Object specified in clause 19, do
        // TODO: implement global object stuff
        // a. Let name be the String value of the property name.
        // b. Let desc be the fully populated data Property Descriptor for the property, containing the specified attributes for the property. For properties listed in 19.2, 19.3, or 19.4 the value of the [[Value]] attribute is the corresponding intrinsic object from realmRec.
        // c. Perform ? DefinePropertyOrThrow(global, name, desc).
        // 3. Return global.
        self.NormalCompletion(global)
    }
}

// let print_stub = {
//     let (mut print_stub, [any]) = builder.start_function("print_stub");

//     let print = builder.external_function(
//         "jssatrt_print_any",
//         [FFIValueType::Runtime, FFIValueType::Any],
//         Returns::Void,
//     );

//     let mut block = print_stub.start_block_main();
//     let runtime = block.get_runtime();
//     block.call_external_function(print, [runtime, any]);
//     print_stub.end_block(block.ret(None));

//     builder.end_function(print_stub)
// };

// let _fib = {
//     // let (mut fib, [n]) = builder.start_function("fib");

//     /*
//     def fib(n):
//         if n == 0:
//             ret 0
//         if n == 1:
//             ret 1
//         ret fib(n - 2) + fib(n - 1)

//     fib(n):
//         Entry():
//             is_zero = n < 1 // not implementing `==` rn lmfao
//             if (is_zero)
//                 jmp RetZero()
//             else
//                 jmp CheckOne()
//         RetZero():
//             ret 0
//         CheckOne():
//             is_one = n < 2
//             if (is_one)
//                 jmp RetOne()
//             else
//                 jmp ComputeFib()
//         RetOne():
//             ret 1
//         ComputeFib():
//             p0 = n - 2
//             lhs = call fib(p0)
//             p1 = n - 1
//             rhs = call fib(p1)
//             result = lhs + rhs
//             ret result
//     */
// };

// // let sum = {
// //     let (mut sum, [n]) = builder.start_function("sum");

// //     /*
// //     def sum(n):
// //         total = 0
// //         for i in range(0, n):
// //             total += i
// //         return total

// //     sum(n):
// //         Entry():
// //             i = 0
// //             total = 0
// //             jmp Check(i, total)
// //         Check(ci, ctotal):
// //             should_iterate = ci < n
// //             if (should_iterate)
// //                 jmp Loop(ci, ctotal)
// //             else
// //                 jmp End(ctotal)
// //         Loop(li, ltotal):
// //             print(li) # add stateful effect so the optimizer doesn't optimize everything
// //             total2 = ltotal + li
// //             i2 = li + 1
// //             jmp Check(i2, total2)
// //         End(etotal):
// //             ret etotal
// //     */
// //     let mut entry = sum.start_block_main();
// //     let (mut check, [ci, ctotal]) = sum.start_block();
// //     let (mut bloop, [li, ltotal]) = sum.start_block();
// //     let (end, [etotal]) = sum.start_block();

// //     let check_sig = check.signature();

// //     {
// //         let i = entry.make_number_decimal(0);
// //         let total = entry.make_number_decimal(0);
// //         sum.end_block(entry.jmp(check.signature(), [i, total]));
// //     }
// //     {
// //         let should_iterate = check.compare_less_than(ci, n);
// //         sum.end_block(check.jmpif(
// //             should_iterate,
// //             bloop.signature(),
// //             [ci, ctotal],
// //             end.signature(),
// //             [ctotal],
// //         ));
// //     }
// //     {
// //         bloop.call(print_stub, [li]);
// //         let total2 = bloop.add(ltotal, li);
// //         let one = bloop.make_number_decimal(1);
// //         let i2 = bloop.add(li, one);
// //         sum.end_block(bloop.jmp(check_sig, [i2, total2]));
// //     }
// //     {
// //         sum.end_block(end.ret(Some(etotal)));
// //     }

// //     builder.end_function(sum)
// // };

// let print_is_cool_and_key = {
//     let (mut func, [record]) = builder.start_function("print_is_cool_and_key");

//     let mut block = func.start_block_main();

//     let key = block.make_string(builder.constant_str_utf16("", "key".into()));
//     let prop_at_key = block.record_get_prop(record, key);
//     let prop_at_slot = block.record_get_slot(record, "is_cool");
//     let call_it = block.record_get_slot(record, "Call");
//     block.call(print_stub, [prop_at_key]);
//     block.call(print_stub, [prop_at_slot]);
//     block.call_virt(call_it, [key]);

//     func.end_block(block.ret(None));
//     builder.end_function(func)
// };

// let _main = {
//     let mut main = builder.start_function_main();

//     // let hello_world = builder.constant_str_utf16("hello_world", "Hello, World!".into());

//     let mut block = main.start_block_main();
//     // let (mut cond, [condition]) = main.start_block();
//     // let (mut end, []) = main.start_block();

//     // let d0 = block.make_number_decimal(0);
//     // let d1 = block.make_number_decimal(1);
//     // let b_true = block.compare_less_than(d0, d1);
//     // let b_false = block.compare_less_than(d1, d0);

//     // let cond_sig = cond.signature();
//     // main.end_block(block.jmp(cond_sig, [b_false]));

//     // cond.call(print_stub, [condition]);
//     // main.end_block(cond.jmpif(condition, end.signature(), [], cond_sig, [b_true]));

//     // main.end_block(end.ret(None));

//     // TODO: make compiling `sum` and hello world a test
//     // let hello_world = block.make_string(hello_world);
//     // block.call(print_stub, [hello_world]);
//     // let max = block.make_number_decimal(3);
//     // block.call(sum, [max]);
//     let obj = block.record_new();
//     let key = block.make_string(builder.constant_str_utf16("", "key".into()));
//     let value = block.make_number_decimal(69);
//     block.record_set_prop(obj, key, value);
//     let value = block.make_number_decimal(1);
//     block.record_set_slot(obj, "is_cool", value);
//     let fnptr = block.make_fnptr(print_stub.id);
//     block.record_set_slot(obj, "Call", fnptr);

//     block.call(print_is_cool_and_key, [obj]);

//     main.end_block(block.ret(None));

//     builder.end_function(main)
// };
