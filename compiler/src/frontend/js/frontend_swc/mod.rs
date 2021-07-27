use std::ops::{Deref, DerefMut};

use rustc_hash::FxHashSet;
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    pass::define,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::ast::{
    BindingIdent, BlockStmt, CallExpr, ClassDecl, Decl, Expr, ExprOrSpread, FnDecl, Function,
    Ident, LabeledStmt, ObjectPatProp, Pat, PatOrExpr, Stmt, VarDecl, VarDeclKind, VarDeclOrExpr,
    VarDeclOrPat, VarDeclarator,
};
use swc_ecmascript::{
    ast::Script,
    parser::{Parser, Syntax},
};

use std::iter::FromIterator;

use self::{
    environment_record::{EnvironmentRecord, EnvironmentRecordFactory, GlobalEnvironmentRecord},
    ordinary_object_behaviors::{create_ordinary_internal_methods, OrdinaryInternalMethods},
};
use crate::frontend::{builder::*, ir::*};
use crate::isa::InternalSlot;

pub mod abstract_operations;
pub mod environment_record;
pub mod ordinary_object_behaviors;

pub struct Emitter<'program, const P: usize> {
    pub program: &'program mut ProgramBuilder,
    pub function: FunctionBuilder<P>,
    pub block: DynBlockBuilder,
}

impl<const P: usize> Deref for Emitter<'_, P> {
    type Target = DynBlockBuilder;

    fn deref(&self) -> &Self::Target {
        &self.block
    }
}

impl<const P: usize> DerefMut for Emitter<'_, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.block
    }
}

impl<'p, const P: usize> Emitter<'p, P> {
    pub fn new(program: &'p mut ProgramBuilder, mut function: FunctionBuilder<P>) -> Self {
        let block = function.start_block_main().into_dynamic();
        Self {
            program,
            function,
            block,
        }
    }

    pub fn finish<F>(mut self, finish: F) -> FnSignature<P>
    where
        F: FnOnce(DynBlockBuilder) -> DynFinalizedBlockBuilder,
    {
        self.function.end_block_dyn(finish(self.block));
        self.program.end_function(self.function)
    }
}

#[test]
fn doesnt_panic() {
    let mut builder = ProgramBuilder::new();
    let ordinary = create_ordinary_internal_methods(&mut builder);

    let env_factory = EnvironmentRecordFactory::new(&mut builder);

    let mut main = builder.start_function_main();
    let block = main.start_block_main().into_dynamic();
    let mut frontend = JsWriter {
        bld: &mut builder,
        bld_fn: &mut main,
        block,
        // the value doesn't matter, it gets set on ScriptEvalution
        running_execution_context_lexical_environment:
            EnvironmentRecord::new_with_register_unchecked(RegisterId::default()),
        env_factory,
        ordinary,
    };

    let s = frontend.bld.constant_str("a string");
    let s = frontend.make_string(s);
    let condition = frontend.compare_equal(s, s);
    let r = frontend.perform_if_else_w_value(
        condition,
        |me| {
            let constant = me.bld.constant_str("yes");
            me.make_string(constant)
        },
        |me| {
            let constant = me.bld.constant_str("no");
            me.make_string(constant)
        },
    );

    let cmp_const = frontend.bld.constant_str("yes");
    let cmp_const = frontend.make_string(cmp_const);
    frontend.compare_equal(r, cmp_const);

    let block = frontend.block.ret(None);
    main.end_block_dyn(block);

    builder.end_function(main);
    let ir = builder.finish();

    let program = crate::lifted::lift(ir);
    let lifted = Box::leak(Box::new(program));
    crate::symbolic_execution::execute(lifted);
}

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
    let ordinary = create_ordinary_internal_methods(&mut builder);

    let env_factory = EnvironmentRecordFactory::new(&mut builder);

    let js_machinery = {
        let (mut main, []) = builder.start_function();
        let block = main.start_block_main().into_dynamic();
        let mut frontend = JsWriter {
            bld: &mut builder,
            bld_fn: &mut main,
            block,
            // the value doesn't matter, it gets set on ScriptEvalution
            running_execution_context_lexical_environment:
                EnvironmentRecord::new_with_register_unchecked(RegisterId::default()),
            env_factory,
            ordinary,
        };

        let (_, realm) = frontend.InitializeHostDefinedRealm();
        let undefined = frontend.block.make_undefined();
        let script_record = frontend.ParseScript((), realm, undefined);
        frontend.ScriptEvaluation(script_record, &script);

        let empty = frontend.block.make_undefined();
        let completion = frontend.NormalCompletion(empty);
        let block = frontend.block.ret(Some(completion));
        main.end_block_dyn(block);

        builder.end_function(main)
    };

    let mut main = builder.start_function_main();
    let mut block = main.start_block_main();

    block.call(js_machinery, []);

    // TODO: make forgetting to call `end_block` a panic
    main.end_block(block.ret(None));
    builder.end_function(main);
    builder.finish()
}

struct JsWriter<'builder, const PARAMETERS: usize> {
    bld: &'builder mut ProgramBuilder,
    bld_fn: &'builder mut FunctionBuilder<PARAMETERS>,
    block: DynBlockBuilder,
    /// this is a hack, shouldn't be here... probably
    running_execution_context_lexical_environment: EnvironmentRecord,
    env_factory: EnvironmentRecordFactory,
    ordinary: OrdinaryInternalMethods,
}

impl<const P: usize> Deref for JsWriter<'_, P> {
    type Target = DynBlockBuilder;

    fn deref(&self) -> &Self::Target {
        &self.block
    }
}

impl<const P: usize> DerefMut for JsWriter<'_, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.block
    }
}

#[allow(non_snake_case)]
impl<'b, const PARAMS: usize> JsWriter<'b, PARAMS> {
    /// <https://tc39.es/ecma262/#sec-initializehostdefinedrealm>
    pub fn InitializeHostDefinedRealm(&mut self) -> (RegisterId, RegisterId) {
        self.comment("InitializeHostDefinedRealm");

        //# 1. Let realm be CreateRealm().
        let realm = self.CreateRealm();

        //# 2. Let newContext be a new execution context.
        let newContext = self.record_new();

        //# 3. Set the Function of newContext to null.
        let null = self.make_null();
        self.record_set_slot(newContext, InternalSlot::Function, null);

        //# 4. Set the Realm of newContext to realm.
        self.record_set_slot(newContext, InternalSlot::Realm, realm);

        //# 5. Set the ScriptOrModule of newContext to null.
        self.record_set_slot(newContext, InternalSlot::ScriptOrModule, null);

        //# 6. Push newContext onto the execution context stack; newContext is
        //#    now the running execution context.
        // TODO: implement

        //# 7. If the host requires use of an exotic object to serve as realm's
        //#    global object, let global be such an object created in a
        //#    host-defined manner. Otherwise, let global be undefined,
        //#    indicating that an ordinary object should be created as the
        //#    global object.
        // TODO: be in accordance with spec later
        // let global = self.make_undefined();
        let global = self.record_new();

        //# 8. If the host requires that the this binding in realm's global
        //#    scope return an object other than the global object, let
        //#    thisValue be such an object created in a host-defined manner.
        //#    Otherwise, let thisValue be undefined, indicating that realm's
        //#    global this binding should be the global object.
        let thisValue = self.make_undefined();

        //# 9. Perform SetRealmGlobalObject(realm, global, thisValue).
        self.SetRealmGlobalObject(realm, global, thisValue);

        //# 10. Let globalObj be ? SetDefaultGlobalBindings(realm).
        let try_globalObj = self.SetDefaultGlobalBindings(realm);
        let globalObj = self.ReturnIfAbrupt(try_globalObj);

        //# 11. Create any host-defined global object properties on globalObj.
        self.create_host_defined_global_object_property_print(globalObj);

        //# 12. Return NormalCompletion(empty).
        // TODO: use TrivialItem::Empty
        let empty = self.block.make_string(self.bld.constant_str("empty"));
        (self.NormalCompletion(empty), realm)
    }

    /// <https://tc39.es/ecma262/#sec-createrealm>
    pub fn CreateRealm(&mut self) -> RegisterId {
        self.comment("CreateRealm");

        //# 1. Let realmRec be a new Realm Record.
        let realmRec = self.record_new();

        //# 2. Perform CreateIntrinsics(realmRec).
        self.CreateIntrinsics(realmRec);

        //# 3. Set realmRec.[[GlobalObject]] to undefined.
        let undefined = self.make_undefined();
        self.record_set_slot(realmRec, InternalSlot::GlobalObject, undefined);

        //# 4. Set realmRec.[[GlobalEnv]] to undefined.
        self.record_set_slot(realmRec, InternalSlot::GlobalEnv, undefined);

        //# 5. Set realmRec.[[TemplateMap]] to a new empty List.
        // TODO: add intrinsics for empty lists
        self.record_set_slot(realmRec, InternalSlot::TemplateMap, undefined);

        //# 6. Return realmRec.
        realmRec
    }

    /// <https://tc39.es/ecma262/#sec-createintrinsics>
    pub fn CreateIntrinsics(&mut self, realmRec: RegisterId) -> RegisterId {
        self.comment("CreateIntrinsics");

        //# 1. Let intrinsics be a new Record.
        let intrinsics = self.record_new();

        //# 2. Set realmRec.[[Intrinsics]] to intrinsics.
        self.record_set_slot(realmRec, InternalSlot::Intrinsics, intrinsics);

        //# 3. Set fields of intrinsics with the values listed in Table 8.
        //#    The field names are the names listed in column one of the table.
        //#    The value of each field is a new object value fully and
        //#    recursively populated with property values as defined by the
        //#    specification of each object in clauses 19 through 28.
        //#    All object property values are newly created object values.
        //#    All values that are built-in function objects are created by
        //#    performing CreateBuiltinFunction(steps, length, name, slots,
        //#        realmRec, prototype) where steps is the definition of that
        //#    function provided by this specification, name is the initial
        //#    value of the function's name property, length is the initial
        //#    value of the function's length property, slots is a list of the
        //#    names, if any, of the function's specified internal slots, and
        //#    prototype is the specified value of the function's [[Prototype]]
        //#    internal slot.
        //#    The creation of the intrinsics and their properties must be
        //#    ordered to avoid any dependencies upon objects that have not yet
        //#    been created.
        // TODO: implement this

        //# 4. Perform AddRestrictedFunctionProperties(intrinsics.[[%Function.prototype%]], realmRec).
        // insert a value into the record to not trip up the symbolic execution engine
        // this will stay un until we properly populate intrinsics
        let undefined = self.make_undefined();
        self.record_set_slot(intrinsics, InternalSlot::FunctionPrototype, undefined);

        let F = self
            .block
            .record_get_slot(intrinsics, InternalSlot::FunctionPrototype);
        self.AddRestrictedFunctionProperties(F, realmRec);

        //# 5. Return intrinsics.
        intrinsics
    }

    /// <https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties>
    pub fn AddRestrictedFunctionProperties(&mut self, F: RegisterId, realm: RegisterId) {
        self.comment("AddRestrictedFunctionProperties");
        // TODO: implement this
    }

    /// <https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties>
    pub fn SetRealmGlobalObject(
        &mut self,
        realmRec: RegisterId,
        globalObj: RegisterId,
        thisValue: RegisterId,
    ) -> RegisterId {
        self.comment("SetRealmGlobalObject");

        // TODO: implement this
        //# 1. If globalObj is undefined, then
        let undefined = self.make_undefined();
        let is_undefined = self.compare_equal(globalObj, undefined);

        let globalObj = self.perform_if_else_w_value(
            is_undefined,
            |me| {
                //# a. Let intrinsics be realmRec.[[Intrinsics]].
                let intrinsics = me.record_get_slot(realmRec, InternalSlot::Intrinsics);

                //# b. Set globalObj to ! OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]]).
                let proto = me.record_get_slot(intrinsics, InternalSlot::ObjectPrototype);
                me.OrdinaryObjectCreate(proto, Vec::new())
            },
            |me| globalObj,
        );

        //# 2. Assert: Type(globalObj) is Object.
        //# 3. If thisValue is undefined, set thisValue to globalObj.
        //# 4. Set realmRec.[[GlobalObject]] to globalObj.
        self.record_set_slot(realmRec, InternalSlot::GlobalObject, globalObj);

        //# 5. Let newGlobalEnv be NewGlobalEnvironment(globalObj, thisValue).
        let newGlobalEnv = self.NewGlobalEnvironment(globalObj, thisValue);

        //# 6. Set realmRec.[[GlobalEnv]] to newGlobalEnv.
        self.record_set_slot(realmRec, InternalSlot::GlobalEnv, newGlobalEnv.register);

        //# 7. Return realmRec.
        realmRec
    }

    /// <https://tc39.es/ecma262/#sec-ordinaryobjectcreate>
    pub fn OrdinaryObjectCreate(
        &mut self,
        proto: RegisterId,
        additionalInternalSlotsList: Vec<InternalSlot>,
    ) -> RegisterId {
        //# 1. Let internalSlotsList be « [[Prototype]], [[Extensible]] ».
        let mut internalSlotsList = vec![InternalSlot::Prototype, InternalSlot::Extensible];

        //# 2. If additionalInternalSlotsList is present, append each of its elements to internalSlotsList.
        internalSlotsList.extend(additionalInternalSlotsList);

        //# 3. Let O be ! MakeBasicObject(internalSlotsList).
        let O = self.MakeBasicObject(internalSlotsList);

        //# 4. Set O.[[Prototype]] to proto.
        self.record_set_slot(O, InternalSlot::Prototype, proto);

        //# 5. Return O.
        O
    }

    /// <https://tc39.es/ecma262/#sec-makebasicobject>
    pub fn MakeBasicObject(&mut self, internalSlotsList: Vec<InternalSlot>) -> RegisterId {
        //# 1. Assert: internalSlotsList is a List of internal slot names.

        //# 2. Let obj be a newly created object with an internal slot for each name in internalSlotsList.
        let obj = self.record_new();
        // we don't need to set the fields here, because the caller will do so

        //# 3. Set obj's essential internal methods to the default ordinary object definitions specified in 10.1.
        let id = self.ordinary.OrdinaryDefineOwnProperty.id;
        let ordinary_define_own_property = self.make_fnptr(id);
        self.record_set_slot(
            obj,
            InternalSlot::DefineOwnProperty,
            ordinary_define_own_property,
        );

        //# 4. Assert: If the caller will not be overriding both obj's
        //#    [[GetPrototypeOf]] and [[SetPrototypeOf]] essential internal
        //#    methods, then internalSlotsList contains [[Prototype]].
        //# 5. Assert: If the caller will not be overriding all of obj's
        //#    [[SetPrototypeOf]], [[IsExtensible]], and [[PreventExtensions]]
        //#    essential internal methods, then internalSlotsList contains
        //#    [[Extensible]].

        //# 6. If internalSlotsList contains [[Extensible]], set obj.[[Extensible]] to true.
        if internalSlotsList.contains(&InternalSlot::Extensible) {
            let r#true = self.make_bool(true);
            self.record_set_slot(obj, InternalSlot::Extensible, r#true);
        }

        //# 7. Return obj.
        obj
    }

    /// <https://tc39.es/ecma262/#sec-newglobalenvironment>
    pub fn NewGlobalEnvironment(
        &mut self,
        G: RegisterId,
        thisValue: RegisterId,
    ) -> GlobalEnvironmentRecord {
        self.comment("NewGlobalEnvironment");

        //# 1. Let objRec be NewObjectEnvironment(G, false, null).
        let r#false = self.make_bool(false);
        let null = self.make_null();
        let objRec = self.NewObjectEnvironment(G, r#false, null);

        //# 2. Let dclRec be a new declarative Environment Record containing no bindings.
        let dclRec = self.env_factory.make_decl_env_rec(&mut self.block);

        //# 3. Let env be a new global Environment Record.
        let env = self.env_factory.make_global_env_rec(&mut self.block);

        //# 4. Set env.[[ObjectRecord]] to objRec.
        self.record_set_slot(env.register, InternalSlot::ObjectRecord, objRec.register);

        //# 5. Set env.[[GlobalThisValue]] to thisValue.
        self.record_set_slot(env.register, InternalSlot::GlobalThisValue, thisValue);

        //# 6. Set env.[[DeclarativeRecord]] to dclRec.
        self.record_set_slot(
            env.register,
            InternalSlot::DeclarativeRecord,
            dclRec.register,
        );

        //# 7. Set env.[[VarNames]] to a new empty List.
        //# 8. Set env.[[OuterEnv]] to null.
        let null = self.make_null();
        self.record_set_slot(env.register, InternalSlot::OuterEnv, null);

        //# 9. Return env.
        env
    }

    /// <https://tc39.es/ecma262/#sec-newobjectenvironment>
    pub fn NewObjectEnvironment(
        &mut self,
        O: RegisterId,
        W: RegisterId,
        E: RegisterId,
    ) -> EnvironmentRecord {
        self.comment("NewObjectEnvironment");

        //# 1. Let env be a new object Environment Record.
        let env = self.env_factory.make_obj_env_rec(&mut self.block);

        //# 2. Set env.[[BindingObject]] to O.
        self.record_set_slot(env.register, InternalSlot::BindingObject, O);

        //# 3. Set env.[[IsWithEnvironment]] to W.
        self.record_set_slot(env.register, InternalSlot::IsWithEnvironment, W);

        //# 4. Set env.[[OuterEnv]] to E.
        self.record_set_slot(env.register, InternalSlot::OuterEnv, E);

        //# 5. Return env.
        env
    }

    /// <https://tc39.es/ecma262/#sec-setdefaultglobalbindings>
    pub fn SetDefaultGlobalBindings(&mut self, realmRec: RegisterId) -> RegisterId {
        self.comment("SetDefaultGlobalBindings");

        //# 1. Let global be realmRec.[[GlobalObject]].
        let global = self
            .block
            .record_get_slot(realmRec, InternalSlot::GlobalObject);
        //# 2. For each property of the Global Object specified in clause 19, do
        // // TODO: do this for all properties and not just `print`

        // //# a. Let name be the String value of the property name.
        // let print = self.bld.constant_str_utf16("print");
        // let name = self.make_string(print);

        // //# b. Let desc be the fully populated data Property Descriptor for the
        // //# property, containing the specified attributes for the property. For
        // //# properties listed in 19.2, 19.3, or 19.4 the value of the [[Value]]
        // //# attribute is the corresponding intrinsic object from realmRec.
        // // let r#true = self.make_bool(true);
        // let r#false = self.make_bool(false);

        // let value =

        // let desc = self.record_new();
        // self.record_set_slot(desc, InternalSlot::Writable, r#false);
        // self.record_set_slot(desc, InternalSlot::Enumerable, r#false);
        // self.record_set_slot(desc, InternalSlot::Configurable, r#false);

        // //# c. Perform ? DefinePropertyOrThrow(global, name, desc).
        // let result = self.DefinePropertyOrThrow(global, name, desc);
        // self.ReturnIfAbrupt(result);

        //# 3. Return global.
        self.NormalCompletion(global)
    }

    /// <https://tc39.es/ecma262/#sec-definepropertyorthrow>
    pub fn DefinePropertyOrThrow(
        &mut self,
        O: RegisterId,
        P: RegisterId,
        desc: RegisterId,
    ) -> RegisterId {
        self.comment("DefinePropertyOrThrow");

        //# 1. Assert: Type(O) is Object.
        //# 2. Assert: IsPropertyKey(P) is true.

        //# 3. Let success be ? O.[[DefineOwnProperty]](P, desc).
        let define_own_property = self.record_get_slot(O, InternalSlot::DefineOwnProperty);
        let success_try = self.call_virt_with_result(define_own_property, [O, P, desc]);
        let success = self.ReturnIfAbrupt(success_try);

        //# 4. If success is false, throw a TypeError exception.
        let success_false = self.negate(success);
        self.perform_if(success_false, |me| {
            // TODO: throw a type error properly
            let argument = me.make_undefined();
            let completion = me.ThrowCompletion(argument);

            // TODO: dont have this hack
            let (mut hack, []) = me.bld_fn.start_block();
            std::mem::swap(&mut me.block, &mut hack);
            me.bld_fn.end_block(hack.ret(Some(completion)));
        });

        //# 5. Return success.
        self.NormalCompletion(success)
    }

    /// <https://tc39.es/ecma262/#sec-parse-script>
    ///
    /// The way this method handles scripts, is by pretending like it parsed the script.
    /// Really, we just emit the corresponding code as we comb over the script ourselves.
    pub fn ParseScript(
        &mut self,
        sourceText: (),
        realm: RegisterId,
        hostDefined: RegisterId,
    ) -> RegisterId {
        self.comment("ParseScript");

        //# 1. Assert: sourceText is an ECMAScript source text (see clause 11).

        //# 2. Let body be ParseText(sourceText, Script).

        //# 3. If body is a List of errors, return body.

        //# 4. Return Script Record { [[Realm]]: realm, [[ECMAScriptCode]]: body, [[HostDefined]]: hostDefined }.
        let script_record = self.record_new();
        self.record_set_slot(script_record, InternalSlot::Realm, realm);
        let undefined = self.make_undefined();
        self.record_set_slot(script_record, InternalSlot::ECMAScriptCode, undefined);
        self.record_set_slot(script_record, InternalSlot::HostDefined, hostDefined);
        script_record
    }

    /// <https://tc39.es/ecma262/#sec-runtime-semantics-scriptevaluation>
    pub fn ScriptEvaluation(&mut self, scriptRecord: RegisterId, script: &Script) -> RegisterId {
        self.comment("ScriptEvaluation");

        //# 1. Let globalEnv be scriptRecord.[[Realm]].[[GlobalEnv]].
        let scriptRecordRealm = self
            .block
            .record_get_slot(scriptRecord, InternalSlot::Realm);
        let globalEnv = self
            .block
            .record_get_slot(scriptRecordRealm, InternalSlot::GlobalEnv);
        let globalEnv = EnvironmentRecord::new_with_register_unchecked(globalEnv);

        //# 2. Let scriptContext be a new ECMAScript code execution context.
        let scriptContext = self.record_new();

        //# 3. Set the Function of scriptContext to null.
        let null = self.make_null();
        self.record_set_slot(scriptContext, InternalSlot::Function, null);

        //# 4. Set the Realm of scriptContext to scriptRecord.[[Realm]].
        self.record_set_slot(scriptContext, InternalSlot::Realm, scriptRecordRealm);

        //# 5. Set the ScriptOrModule of scriptContext to scriptRecord.
        self.record_set_slot(
            scriptContext,
            InternalSlot::ScriptOrModule,
            scriptRecordRealm,
        );

        //# 6. Set the VariableEnvironment of scriptContext to globalEnv.
        self.record_set_slot(
            scriptContext,
            InternalSlot::VariableEnvironment,
            globalEnv.register,
        );

        //# 7. Set the LexicalEnvironment of scriptContext to globalEnv.
        self.record_set_slot(
            scriptContext,
            InternalSlot::LexicalEnvironment,
            globalEnv.register,
        );

        //# 8. Set the PrivateEnvironment of scriptContext to null.
        self.record_set_slot(scriptContext, InternalSlot::PrivateEnvironment, null);

        //# 9. Suspend the currently running execution context.
        // TODO: ?

        //# 10. Push scriptContext onto the execution context stack; scriptContext is now the running execution context.
        // TODO: this is a hack, should be using proepr execution context stack things
        self.running_execution_context_lexical_environment = globalEnv;

        //# 11. Let scriptBody be scriptRecord.[[ECMAScriptCode]].
        let scriptBody = script;

        //# 12. Let result be GlobalDeclarationInstantiation(scriptBody, globalEnv).
        // TODO: at this time, we don't have a good way to mutably change local vars
        // will have to figure out some sort of workaround. thus, in the meantime, this is broken.
        // /shrug
        let result = self.GlobalDeclarationInstantiation(scriptBody, globalEnv);

        //# 13. If result.[[Type]] is normal, then
        let is_normal = self.is_normal_completion(result);

        let result = self.perform_if_else_w_value(
            is_normal,
            |me| {
                //# a. Set result to the result of evaluating scriptBody.
                me.evaluate_script(script)
            },
            |_| {
                // in the event that we don't evaluate the script, we should use
                // the old value for `result`
                result
            },
        );

        //# 14. If result.[[Type]] is normal and result.[[Value]] is empty, then
        let is_normal = self.is_normal_completion(result);
        // TODO: check `[[Value]]` and stuff
        //# a. Set result to NormalCompletion(undefined).

        //# 15. Suspend scriptContext and remove it from the execution context stack.
        //# 16. Assert: The execution context stack is not empty.
        //# 17. Resume the context that is now on the top of the execution context stack as the running execution context.
        //# 18. Return Completion(result).

        self.NormalCompletion(result)
    }

    /// <https://tc39.es/ecma262/#sec-globaldeclarationinstantiation>
    pub fn GlobalDeclarationInstantiation(
        &mut self,
        script: &Script,
        env: EnvironmentRecord,
    ) -> RegisterId {
        self.comment("GlobalDeclarationInstantiation");

        //# 1. Assert: env is a global Environment Record.
        let env = GlobalEnvironmentRecord::new_with_register_unchecked(env.register);

        //# 2. Let lexNames be the LexicallyDeclaredNames of script.
        let lexNames = LexicallyDeclaredNames::compute(script);
        let lex_names_set = FxHashSet::from_iter(&lexNames);

        //# It is a Syntax Error if the LexicallyDeclaredNames of <x> contains
        //# any duplicate entries.
        if lex_names_set.len() != lexNames.len() {
            panic!("duplicate LexicallyDeclaredNames, error in JS");
        }

        //# 3. Let varNames be the VarDeclaredNames of script.
        let varNames = VarDeclaredNames::compute(script);
        let var_names_set = FxHashSet::from_iter(&varNames);

        //# It is a Syntax Error if any element of the LexicallyDeclaredNames of
        //# StatementList also occurs in the VarDeclaredNames of StatementList.
        if lex_names_set.intersection(&var_names_set).any(|_| true) {
            panic!("duplicate LexicallyDeclaredNames, error in JS");
        }

        //# 4. For each element name of lexNames, do
        for elem in lexNames {
            let name = self.bld.constant_str_utf16(elem);
            let name = self.make_string(name);

            //# a. If env.HasVarDeclaration(name) is true, throw a SyntaxError exception.
            let cond = env.HasVarDeclaration(&mut self.block, name);
            self.perform_if(cond, |me| {
                // TODO: throw a SyntaxError properly
                let constant = me.bld.constant_str_utf16("SyntaxError occurred");
                let syntax_error = me.make_string(constant);
                let completion = me.ThrowCompletion(syntax_error);

                let (mut hack, []) = me.bld_fn.start_block();
                std::mem::swap(&mut me.block, &mut hack);
                me.bld_fn.end_block(hack.ret(Some(completion)));
            });

            //# b. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
            let cond = env.HasLexicalDeclaration(&mut self.block, name);
            self.perform_if(cond, |me| {
                // TODO: throw a SyntaxError properly
                let constant = me.bld.constant_str_utf16("SyntaxError occurred");
                let syntax_error = me.make_string(constant);
                let completion = me.ThrowCompletion(syntax_error);

                let (mut hack, []) = me.bld_fn.start_block();
                std::mem::swap(&mut me.block, &mut hack);
                me.bld_fn.end_block(hack.ret(Some(completion)));
            });

            //# c. Let hasRestrictedGlobal be ? env.HasRestrictedGlobalProperty(name).
            let hasRestrictedGlobal_try = env.HasRestrictedGlobalProperty(&mut self.block, name);
            let hasRestrictedGlobal = self.ReturnIfAbrupt(hasRestrictedGlobal_try);

            //# d. If hasRestrictedGlobal is true, throw a SyntaxError exception.
            self.perform_if(hasRestrictedGlobal, |me| {
                // TODO: throw a SyntaxError properly
                let constant = me.bld.constant_str_utf16("SyntaxError occurred");
                let syntax_error = me.make_string(constant);
                let completion = me.ThrowCompletion(syntax_error);

                let (mut hack, []) = me.bld_fn.start_block();
                std::mem::swap(&mut me.block, &mut hack);
                me.bld_fn.end_block(hack.ret(Some(completion)));
            });
        }

        //# 5. For each element name of varNames, do
        for elem in varNames {
            //# a. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
            let name = self.bld.constant_str_utf16(elem);
            let name = self.make_string(name);
        }

        //# 6. Let varDeclarations be the VarScopedDeclarations of script.
        let varDeclarations = VarScopedDeclarations::compute(script);

        //# 7. Let functionsToInitialize be a new empty List.
        let mut functionsToInitialize = Vec::new();

        //# 8. Let declaredFunctionNames be a new empty List.
        let mut declaredFunctionNames = Vec::new();

        //# 9. For each element d of varDeclarations, in reverse List order, do
        for d in varDeclarations.iter().rev() {
            //# a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
            if let VarDeclaratorKind::DeclaredFunctionName(d) = d {
                //# i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
                //# ii. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
                //# iii. Let fn be the sole element of the BoundNames of d.
                let r#fn = BoundNames::compute_fn(d).remove(0);

                //# iv. If fn is not an element of declaredFunctionNames, then
                if !declaredFunctionNames.contains(&r#fn) {
                    //# 1. Let fnDefinable be ? env.CanDeclareGlobalFunction(fn).
                    //# 2. If fnDefinable is false, throw a TypeError exception.
                    //# 3. Append fn to declaredFunctionNames.
                    declaredFunctionNames.push(r#fn);

                    //# 4. Insert d as the first element of functionsToInitialize.
                    functionsToInitialize.insert(0, d);
                }
            }
        }

        //# 10. Let declaredVarNames be a new empty List.
        let mut declaredVarNames = Vec::new();

        //# 11. For each element d of varDeclarations, do
        for d in varDeclarations.iter() {
            //# a. If d is a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
            if let VarDeclaratorKind::DeclaredVariableName(d) = d {
                //# i. For each String vn of the BoundNames of d, do
                for vn in BoundNames::compute(d) {
                    //# 1. If vn is not an element of declaredFunctionNames, then
                    if !declaredFunctionNames.contains(&vn) {
                        //# a. Let vnDefinable be ? env.CanDeclareGlobalVar(vn).
                        //# b. If vnDefinable is false, throw a TypeError exception.
                        //# c. If vn is not an element of declaredVarNames, then
                        if !declaredVarNames.contains(&vn) {
                            //# i. Append vn to declaredVarNames.
                            declaredVarNames.push(vn);
                        }
                    }
                }
            }
        }

        //# 12. NOTE: No abnormal terminations occur after this algorithm step
        //#     if the global object is an ordinary object. However, if the
        //#     global object is a Proxy exotic object it may exhibit behaviours
        //#     that cause abnormal terminations in some of the following steps.

        //# 13. NOTE: Annex B.3.3.2 adds additional steps at this point.

        //# 14. Let lexDeclarations be the LexicallyScopedDeclarations of script.
        let lexDeclarations = LexicallyScopedDeclarations::compute(script);

        //# 15. Let privateEnv be null.
        let privateEnv = self.make_null();

        //# 16. For each element d of lexDeclarations, do
        for d in lexDeclarations {
            //# a. NOTE: Lexically declared names are only instantiated here but not initialized.

            //# b. For each element dn of the BoundNames of d, do
            let (IsConstantDeclaration, bound_names) = d.bound_names();
            for dn in bound_names {
                //# i. If IsConstantDeclaration of d is true, then
                if IsConstantDeclaration {
                    //# 1. Perform ? env.CreateImmutableBinding(dn, true).
                    todo!("const");
                } else {
                    //# ii. Else,
                    //# 1. Perform ? env.CreateMutableBinding(dn, false).
                    todo!("non const");
                }
            }
        }

        //# 17. For each Parse Node f of functionsToInitialize, do
        for f in functionsToInitialize {
            //# a. Let fn be the sole element of the BoundNames of f.
            let r#fn = BoundNames::compute_fn(f).remove(0);

            //# b. Let fo be InstantiateFunctionObject of f with arguments env and privateEnv.
            let fo_try = self.InstantiateFunctionObject(f, env, privateEnv);
            let fo = self.ReturnIfAbrupt(fo_try);

            //# c. Perform ? env.CreateGlobalFunctionBinding(fn, fo, false).
            let n_const = self.bld.constant_str_utf16(r#fn);
            let N = self.make_string(n_const);
            let D = self.make_bool(false);
            let completion = env.CreateGlobalFunctionBinding(&mut self.block, N, fo, D);
            self.comment("Returning if Abrupt from completion");
            self.ReturnIfAbrupt(completion);
            self.comment("Done Returning if Abrupt from completion");
        }

        //# 18. For each String vn of declaredVarNames, do
        for vn in declaredVarNames {
            //# a. Perform ? env.CreateGlobalVarBinding(vn, false).
            todo!();
        }

        //# 19. Return NormalCompletion(empty).
        // TODO: implement ZST `empty`
        let empty = self.make_undefined();
        self.NormalCompletion(empty)
    }

    /// <https://tc39.es/ecma262/#sec-getvalue>
    pub fn GetValue(&mut self, V: RegisterId) -> RegisterId {
        self.comment("GetValue");

        // TODO: this is botched lmfao

        //# 1. ReturnIfAbrupt(V).
        let value = self.ReturnIfAbrupt(V);

        //# 2. If V is not a Reference Record, return V.
        value

        //# 3. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
        //# 4. If IsPropertyReference(V) is true, then
        //# a. Let baseObj be ! ToObject(V.[[Base]]).
        //# b. If IsPrivateReference(V) is true, then
        //# i. Return ? PrivateGet(V.[[ReferencedName]], baseObj).
        //# c. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
        //# 5. Else,
        //# a. Let base be V.[[Base]].
        //# b. Assert: base is an Environment Record.
        //# c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
    }

    /// <https://tc39.es/ecma262/#sec-runtime-semantics-instantiatefunctionobject>
    pub fn InstantiateFunctionObject(
        &mut self,
        f: &FnDecl,
        scope: GlobalEnvironmentRecord,
        privateScope: RegisterId,
    ) -> RegisterId {
        let f = &f.function;
        match (f.is_async, f.is_generator) {
            (false, false) => {
                let instance = self.InstantiateOrdinaryFunctionObject(f, scope, privateScope);
                self.NormalCompletion(instance)
            }
            _ => todo!(),
        }
    }

    /// <https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionobject>
    pub fn InstantiateOrdinaryFunctionObject(
        &mut self,
        f: &Function,
        scope: GlobalEnvironmentRecord,
        privateScope: RegisterId,
    ) -> RegisterId {
        // TODO: implement lmao

        //# FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
        //# 1. Let name be StringValue of BindingIdentifier.
        //# 2. Let sourceText be the source text matched by FunctionDeclaration.
        //# 3. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters, FunctionBody, non-lexical-this, scope, privateScope).
        //# 4. Perform SetFunctionName(F, name).
        //# 5. Perform MakeConstructor(F).
        //# 6. Return F.
        //# FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
        //# 1. Let sourceText be the source text matched by FunctionDeclaration.
        //# 2. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters, FunctionBody, non-lexical-this, scope, privateScope).
        //# 3. Perform SetFunctionName(F, "default").
        //# 4. Perform MakeConstructor(F).
        //# 5. Return F.

        self.record_new()
    }

    /// <https://tc39.es/ecma262/#sec-resolvebinding>
    pub fn ResolveBinding(&mut self, name: RegisterId, env: Option<RegisterId>) -> RegisterId {
        self.comment("ResolveBinding");

        let env_value;
        //# 1. If env is not present or if env is undefined, then
        if env.is_none() {
            //# a. Set env to the running execution context's LexicalEnvironment.
            env_value = self.running_execution_context_lexical_environment;
        } else {
            panic!("what");
        }
        let env = env_value;

        //# 2. Assert: env is an Environment Record.
        //# 3. If the code matching the syntactic production that is being
        //#    evaluated is contained in strict mode code, let strict be true;
        //#    else let strict be false.
        // TODO: handle non strict
        let strict = true;

        //# 4. Return ? GetIdentifierReference(env, name, strict).
        self.GetIdentifierReference(env, name, strict)
    }

    /// <https://tc39.es/ecma262/#sec-getidentifierreference>
    pub fn GetIdentifierReference(
        &mut self,
        env: EnvironmentRecord,
        name: RegisterId,
        strict: bool,
    ) -> RegisterId {
        let k = self.running_execution_context_lexical_environment;
        self.comment("GetIdentifierReference");

        //# 1. If env is the value null, then
        let null = self.make_null();
        let condition = self.compare_equal(env.register, null);
        self.perform_if(condition, |me| {
            //# a. Return the Reference Record { [[Base]]: unresolvable, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
            let completion = me.record_new();

            // TODO: trivial item 'unresolvable"
            let unresolvable = me.bld.constant_str("unresolvable");
            let unresolvable = me.make_string(unresolvable);

            let strict = me.make_bool(strict);

            // TODO: trivial item 'empty'
            let empty = me.bld.constant_str("empty");
            let empty = me.make_string(empty);

            me.record_set_slot(completion, InternalSlot::Base, unresolvable);
            me.record_set_slot(completion, InternalSlot::ReferencedName, name);
            me.record_set_slot(completion, InternalSlot::Strict, strict);
            me.record_set_slot(completion, InternalSlot::ThisValue, empty);

            // TODO: cleaner way to return from an if
            let (mut hack, []) = me.bld_fn.start_block();
            std::mem::swap(&mut me.block, &mut hack);
            me.bld_fn.end_block(hack.ret(Some(completion)));
        });

        //# 2. Let exists be ? env.HasBinding(name).
        let exists_try = env.HasBinding(&mut self.block, name);
        let exists = self.ReturnIfAbrupt(exists_try);

        //# 3. If exists is true, then
        self.perform_if(exists, |me| {
            //# a. Return the Reference Record { [[Base]]: env, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
            // TODO: we assume that we'll always have it
            let record = me.record_new();

            let strict = me.make_bool(strict);

            // TODO: trivial item 'empty'
            let empty = me.bld.constant_str("empty");
            let empty = me.make_string(empty);

            me.record_set_slot(record, InternalSlot::Base, env.register);
            me.record_set_slot(record, InternalSlot::ReferencedName, name);
            me.record_set_slot(record, InternalSlot::Strict, strict);
            me.record_set_slot(record, InternalSlot::ThisValue, empty);

            let completion = me.NormalCompletion(record);

            // TODO: cleaner way to return from an if
            let (mut hack, []) = me.bld_fn.start_block();
            std::mem::swap(&mut me.block, &mut hack);
            me.bld_fn.end_block(hack.ret(Some(completion)));
        });

        //# 4. Else,

        //# a. Let outer be env.[[OuterEnv]].
        let outer = self.record_get_slot(env.register, InternalSlot::OuterEnv);
        let outer = EnvironmentRecord::new_with_register_unchecked(outer);

        //# b. Return ? GetIdentifierReference(outer, name, strict).
        // TODO: this causes a recursion exception
        // we should make this a seperate function in the future
        // self.GetIdentifierReference(outer, name, strict)
        self.ThrowCompletion(outer.register)
    }

    /// <https://tc39.es/ecma262/#sec-evaluatecall>
    pub fn EvaluateCall(
        &mut self,
        func: RegisterId,
        r#ref: RegisterId,
        arguments: &Vec<ExprOrSpread>,
        tailPosition: (),
    ) -> RegisterId {
        self.comment("EvaluateCall");

        // TODO: actually call the function
        // help this is so much effort just to get `print("hello world")`
        let virtual_func = self.record_get_slot(func, InternalSlot::Call);
        self.call_virt(virtual_func, [r#ref]);
        let result = self.make_null();
        self.NormalCompletion(result)
    }

    // host-defined
    pub fn create_host_defined_global_object_property_print(&mut self, global: RegisterId) {
        let print_fn = {
            let (mut print, [arg]) = self.bld.start_function();

            let jssatrt_print = self.bld.external_function(
                "jssatrt_print_any",
                [FFIValueType::Runtime, FFIValueType::Any],
                Returns::Void,
            );

            let mut block = print.start_block_main();
            let runtime = block.get_runtime();
            block.call_external_function(jssatrt_print, [runtime, arg]);
            print.end_block(block.ret(None));

            self.bld.end_function(print)
        };

        // TODO: do this for all properties and not just `print`

        //# a. Let name be the String value of the property name.
        let print = self.bld.constant_str_utf16("print");
        let name = self.make_string(print);

        //# b. Let desc be the fully populated data Property Descriptor for the
        //# property, containing the specified attributes for the property. For
        //# properties listed in 19.2, 19.3, or 19.4 the value of the [[Value]]
        //# attribute is the corresponding intrinsic object from realmRec.
        // let r#true = self.make_bool(true);
        let r#false = self.make_bool(false);

        // TODO: properly make a function
        let func_obj = self.record_new();
        let fnptr = self.make_fnptr(print_fn.id);
        self.record_set_slot(func_obj, InternalSlot::Call, fnptr);
        let value = func_obj;

        let desc = self.record_new();
        self.record_set_slot(desc, InternalSlot::Writable, r#false);
        self.record_set_slot(desc, InternalSlot::Enumerable, r#false);
        self.record_set_slot(desc, InternalSlot::Configurable, r#false);

        //# c. Perform ? DefinePropertyOrThrow(global, name, desc).
        let result = self.DefinePropertyOrThrow(global, name, desc);
        self.ReturnIfAbrupt(result);
    }

    // RUNTIME SEMANTICS OF SCRIPT
    pub fn evaluate_script(&mut self, script: &Script) -> RegisterId {
        self.comment("evaluate_script");

        // StatementList : StatementList StatementListItem
        let undefined = self.make_undefined();
        let mut s = self.NormalCompletion(undefined);

        // TODO: evaluate statements as specified
        // 1. Let sl be the result of evaluating StatementList.
        // 2. ReturnIfAbrupt(sl).
        // 3. Let s be the result of evaluating StatementListItem.
        // 4. Return Completion(UpdateEmpty(s, sl)).
        for stmt in script.body.iter() {
            let s2 = self.evaluate_stmt(stmt);
        }

        s
    }

    pub fn evaluate_stmt(&mut self, stmt: &Stmt) -> RegisterId {
        self.comment("evaluate_stmt");

        match stmt {
            Stmt::Block(_) => todo!(),
            Stmt::Empty(_) => todo!(),
            Stmt::Debugger(_) => todo!(),
            Stmt::With(_) => todo!(),
            Stmt::Return(_) => todo!(),
            Stmt::Labeled(_) => todo!(),
            Stmt::Break(_) => todo!(),
            Stmt::Continue(_) => todo!(),
            Stmt::If(_) => todo!(),
            Stmt::Switch(_) => todo!(),
            Stmt::Throw(_) => todo!(),
            Stmt::Try(_) => todo!(),
            Stmt::While(_) => todo!(),
            Stmt::DoWhile(_) => todo!(),
            Stmt::For(_) => todo!(),
            Stmt::ForIn(_) => todo!(),
            Stmt::ForOf(_) => todo!(),
            Stmt::Decl(Decl::Fn(f)) => {
                let f = &f.function;
                match (f.is_async, f.is_generator) {
                    (false, false) => {
                        //# HoistableDeclaration : FunctionDeclaration
                        //# 1. Return the result of evaluating FunctionDeclaration.

                        //# 15.2.6 Runtime Semantics: Evaluation
                        //# FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
                        //# 1. Return NormalCompletion(empty).
                        //# FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
                        //# 1. Return NormalCompletion(empty).
                        let empty = self.make_null();
                        self.NormalCompletion(empty)
                    }
                    _ => {
                        //# HoistableDeclaration :
                        //#     GeneratorDeclaration
                        //#     AsyncFunctionDeclaration
                        //#     AsyncGeneratorDeclaration
                        //# 1. Return NormalCompletion(empty).
                        let empty = self.make_null();
                        self.NormalCompletion(empty)
                    }
                }
            }
            Stmt::Decl(_) => todo!(),
            Stmt::Expr(e) => self.evaluate_expr(&*e.expr),
        }
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> RegisterId {
        self.comment("evaluate_expr");

        match expr {
            swc_ecmascript::ast::Expr::This(_) => todo!(),
            swc_ecmascript::ast::Expr::Array(_) => todo!(),
            swc_ecmascript::ast::Expr::Object(_) => todo!(),
            swc_ecmascript::ast::Expr::Fn(_) => todo!(),
            swc_ecmascript::ast::Expr::Unary(_) => todo!(),
            swc_ecmascript::ast::Expr::Update(_) => todo!(),
            swc_ecmascript::ast::Expr::Bin(_) => todo!(),
            swc_ecmascript::ast::Expr::Assign(_) => todo!(),
            swc_ecmascript::ast::Expr::Member(_) => todo!(),
            swc_ecmascript::ast::Expr::Cond(_) => todo!(),
            swc_ecmascript::ast::Expr::Call(CallExpr {
                span,
                callee,
                args,
                type_args,
            }) => {
                //# 1. Let ref be the result of evaluating CallExpression.
                let r#ref = match callee {
                    swc_ecmascript::ast::ExprOrSuper::Super(s) => todo!(),
                    swc_ecmascript::ast::ExprOrSuper::Expr(e) => self.evaluate_expr(&*e),
                };

                //# 2. Let func be ? GetValue(ref).
                let func = self.GetValue(r#ref);
                self.comment("after gettr val");

                // TODO: do arguments
                let hello_world = self.bld.constant_str_utf16("Hello, World!");
                let hello_world = self.make_string(hello_world);

                //# 3. Let thisCall be this CallExpression.
                //# 4. Let tailCall be IsInTailPosition(thisCall).
                //# 5. Return ? EvaluateCall(func, ref, Arguments, tailCall).
                let completion_record = self.EvaluateCall(func, hello_world, args, ());
                self.comment("after eval call");
                let result = self.ReturnIfAbrupt(completion_record);
                self.NormalCompletion(result)
                /*
                let mut invoc = Vec::new();

                for ExprOrSpread { spread, expr } in args {
                    assert!(matches!(spread, None));
                    let expr = self.evaluate_expr(&*expr);
                    invoc.push(expr);
                } */
            }
            swc_ecmascript::ast::Expr::New(_) => todo!(),
            swc_ecmascript::ast::Expr::Seq(_) => todo!(),
            swc_ecmascript::ast::Expr::Ident(ident) => {
                //# 1. Return ? ResolveBinding(StringValue of Identifier).
                let string_value = ident.sym.to_string();
                let string_value = self.bld.constant_str_utf16(string_value);
                let name = self.make_string(string_value);
                let resolve_binding = self.ResolveBinding(name, None);
                self.comment("after resolve binding");
                let inner_value = self.ReturnIfAbrupt(resolve_binding);
                self.NormalCompletion(inner_value)
            }
            swc_ecmascript::ast::Expr::Lit(_) => todo!(),
            swc_ecmascript::ast::Expr::Tpl(_) => todo!(),
            swc_ecmascript::ast::Expr::TaggedTpl(_) => todo!(),
            swc_ecmascript::ast::Expr::Arrow(_) => todo!(),
            swc_ecmascript::ast::Expr::Class(_) => todo!(),
            swc_ecmascript::ast::Expr::Yield(_) => todo!(),
            swc_ecmascript::ast::Expr::MetaProp(_) => todo!(),
            swc_ecmascript::ast::Expr::Await(_) => todo!(),
            swc_ecmascript::ast::Expr::Paren(_) => todo!(),
            swc_ecmascript::ast::Expr::JSXMember(_) => todo!(),
            swc_ecmascript::ast::Expr::JSXNamespacedName(_) => todo!(),
            swc_ecmascript::ast::Expr::JSXEmpty(_) => todo!(),
            swc_ecmascript::ast::Expr::JSXElement(_) => todo!(),
            swc_ecmascript::ast::Expr::JSXFragment(_) => todo!(),
            swc_ecmascript::ast::Expr::TsTypeAssertion(_) => todo!(),
            swc_ecmascript::ast::Expr::TsConstAssertion(_) => todo!(),
            swc_ecmascript::ast::Expr::TsNonNull(_) => todo!(),
            swc_ecmascript::ast::Expr::TsAs(_) => todo!(),
            swc_ecmascript::ast::Expr::PrivateName(_) => todo!(),
            swc_ecmascript::ast::Expr::OptChain(_) => todo!(),
            swc_ecmascript::ast::Expr::Invalid(_) => todo!(),
        }
    }

    // ecmascript helpers

    /// <https://tc39.es/ecma262/#sec-returnifabrupt>
    pub fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("ReturnIfAbrupt");

        //# 1. If argument is an abrupt completion, return argument.
        let is_abrupt_completion = self.is_abrupt_completion(argument);

        let on_abrupt_completion = {
            let (on_abrupt_completion, []) = self.bld_fn.start_block();
            self.bld_fn
                .end_block(on_abrupt_completion.ret(Some(argument)))
        };

        //# 2. Else if argument is a Completion Record, set argument to argument.[[Value]].

        let (mut on_normal_completion, []) = self.bld_fn.start_block();
        let argument_inner_value =
            on_normal_completion.record_get_slot(argument, InternalSlot::Value);

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

    /// <https://tc39.es/ecma262/#sec-normalcompletion>
    pub fn NormalCompletion(&mut self, argument: RegisterId) -> RegisterId {
        self.block.NormalCompletion(self.bld, argument)
    }

    /// <https://tc39.es/ecma262/#sec-throwcompletion>
    pub fn ThrowCompletion(&mut self, argument: RegisterId) -> RegisterId {
        self.comment("ThrowCompletion");

        //# 1. Return Completion { [[Type]]: throw, [[Value]]: argument, [[Target]]: empty }.
        let completion_record = self.record_new();
        let normal = self.block.make_string(self.bld.constant_str("throw"));
        self.record_set_slot(completion_record, InternalSlot::Type, normal);
        self.record_set_slot(completion_record, InternalSlot::Value, argument);
        let empty = self.block.make_string(self.bld.constant_str("empty"));
        self.record_set_slot(completion_record, InternalSlot::Target, empty);
        completion_record
    }

    // actual helpers

    fn perform_if<F: FnMut(&'_ mut JsWriter<'b, PARAMS>)>(
        &mut self,
        condition: RegisterId,
        mut in_if: F,
    ) {
        let (mut if_true, []) = self.bld_fn.start_block();
        let (mut return_to, []) = self.bld_fn.start_block();

        // `self.block`: control path we're on
        // `if_true`: path to execute if true
        // `return_to`: path to return on once we're done
        //
        // 1. replace `self.block` with `if_true` so we can modify and end `self.block`,
        //    and pretend like all execution after that point (which will be in `in_if`)
        //    is part of self
        std::mem::swap(&mut self.block, &mut if_true);
        let self_block = if_true;
        let if_true = &mut self.block;

        // 2. self_block is our old path, fork it to "if_true else return_to" and kill it
        self.bld_fn.end_block(self_block.jmpif_dynargs(
            condition,
            if_true.id,
            vec![],
            return_to.id,
            vec![],
        ));

        // 3. now execute `in_if`
        //
        //    self.block will be `if_true`, so all modification will be performed in our if
        in_if(self);

        // 4. kill `self.block`, which is `if_true`,
        //    to make it go to `return_to`
        std::mem::swap(&mut self.block, &mut return_to);
        let self_block = return_to;
        let return_to = &mut self.block;

        self.bld_fn
            .end_block(self_block.jmp_dynargs(return_to.id, vec![]));
    }

    fn perform_if_else_w_value<F1, F2>(
        &mut self,
        condition: RegisterId,
        mut if_so: F1,
        mut other: F2,
    ) -> RegisterId
    where
        F1: FnMut(&'_ mut JsWriter<'b, PARAMS>) -> RegisterId,
        F2: FnMut(&'_ mut JsWriter<'b, PARAMS>) -> RegisterId,
    {
        self.comment("perform_if_else_w_value - fork");
        let (mut if_true, []) = self.bld_fn.start_block();
        let (mut if_not, []) = self.bld_fn.start_block();
        let (mut return_to, [two_paths_results]) = self.bld_fn.start_block();
        let return_to_sig = return_to.signature();

        // make `self.block` jump to `if_true`/`if_not`
        // sets self to the `return_to` block, which will have the `two_paths_results`
        // register that the user wants
        let mut return_to = return_to.into_dynamic();
        std::mem::swap(&mut self.block, &mut return_to);
        let old_self = return_to;

        self.bld_fn.end_block_dyn(old_self.jmpif_dynargs(
            condition,
            if_true.id,
            vec![],
            if_not.id,
            vec![],
        ));

        // perform if_true
        std::mem::swap(&mut self.block, &mut if_true);
        self.comment("perform_if_else_w_value - if_so");
        let reg = if_so(self);
        std::mem::swap(&mut self.block, &mut if_true);
        self.bld_fn.end_block(if_true.jmp(return_to_sig, [reg]));

        // perform if_not
        std::mem::swap(&mut self.block, &mut if_not);
        self.comment("perform_if_else_w_value - other");
        let reg = other(self);
        std::mem::swap(&mut self.block, &mut if_not);
        self.bld_fn.end_block(if_not.jmp(return_to_sig, [reg]));

        self.comment("perform_if_else_w_value - merge");
        two_paths_results
    }

    fn is_normal_completion(&mut self, record: RegisterId) -> RegisterId {
        self.comment("is_normal_completion");

        let completion_type = self.record_get_slot(record, InternalSlot::Type);
        let normal_completion = self.block.make_string(self.bld.constant_str("normal"));
        self.compare_equal(completion_type, normal_completion)
    }

    fn is_abrupt_completion(&mut self, record: RegisterId) -> RegisterId {
        self.comment("is_abrupt_completion");

        let is_normal_completion = self.is_normal_completion(record);
        self.negate(is_normal_completion)
    }
}

impl DynBlockBuilder {
    /// <https://tc39.es/ecma262/#sec-normalcompletion>
    pub fn NormalCompletion(
        &mut self,
        // TODO: remove this when `normal` and `empty` get turned into trivial items
        bld: &mut ProgramBuilder,
        argument: RegisterId,
    ) -> RegisterId {
        self.comment("NormalCompletion");

        //# 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]: empty }.
        let completion_record = self.record_new();
        let normal = self.make_string(bld.constant_str("normal"));
        self.record_set_slot(completion_record, InternalSlot::Type, normal);
        self.record_set_slot(completion_record, InternalSlot::Value, argument);
        let empty = self.make_string(bld.constant_str("empty"));
        self.record_set_slot(completion_record, InternalSlot::Target, empty);
        completion_record
    }
}

struct LexicallyDeclaredNames<'names>(&'names mut Vec<String>);

/// <https://tc39.es/ecma262/#sec-static-semantics-lexicallydeclarednames>
///
/// reading the spec is far too hard. most of this is inspired by:
/// <https://github.com/engine262/engine262/blob/main/src/static-semantics/LexicallyDeclaredNames.mjs>
#[allow(non_snake_case)]
impl LexicallyDeclaredNames<'_> {
    pub fn compute(script: &Script) -> Vec<String> {
        let mut names = Vec::new();
        LexicallyDeclaredNames(&mut names).Script(script);
        names
    }

    fn Script(&mut self, script: &Script) {
        //# ScriptBody : StatementList
        //# 1. Return TopLevelLexicallyDeclaredNames of StatementList.
        TopLevelLexicallyDeclaredNames(self.0).StatementList(&script.body);
    }
}

struct TopLevelLexicallyDeclaredNames<'names>(&'names mut Vec<String>);

impl TopLevelLexicallyDeclaredNames<'_> {
    fn StatementList(&mut self, stmts: &[Stmt]) {
        // for loop:
        //# StatementList : StatementList StatementListItem
        //# 1. Let names1 be TopLevelLexicallyDeclaredNames of StatementList.
        //# 2. Let names2 be TopLevelLexicallyDeclaredNames of StatementListItem.
        //# 3. Return the list-concatenation of names1 and names2.
        for stmt in stmts {
            // match:
            //# StatementListItem : Declaration
            //# 1. If Declaration is Declaration : HoistableDeclaration , then
            //#     a. Return « ».
            //# 2. Return the BoundNames of Declaration.
            match stmt {
                Stmt::Decl(Decl::Class(class)) => BoundNames(self.0).Class(class),
                Stmt::Decl(Decl::Var(lex)) => BoundNames(self.0).VariableDeclarationList(lex),
                _ => {}
            }
        }
    }
}

struct VarDeclaredNames<'names>(&'names mut Vec<String>);

impl VarDeclaredNames<'_> {
    pub fn compute(script: &Script) -> Vec<String> {
        let mut names = Vec::new();
        VarDeclaredNames(&mut names).Script(script);
        names
    }

    fn Script(&mut self, script: &Script) {
        //# ScriptBody : StatementList
        //# 1. Return TopLevelVarDeclaredNames of StatementList.
        TopLevelVarDeclaredNames(self.0).StatementList(&script.body);
    }

    fn StatementList(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.Statement(stmt);
        }
    }

    fn Statement(&mut self, stmt: &Stmt) {
        // for loop:
        //# StatementList : StatementList StatementListItem
        //# 1. Let names1 be VarDeclaredNames of StatementList.
        //# 2. Let names2 be VarDeclaredNames of StatementListItem.
        //# 3. Return the list-concatenation of names1 and names2.
        match stmt {
            Stmt::Decl(Decl::Fn(func)) => {
                BoundNames(self.0).HoistableDeclaration(func);
            }
            Stmt::Decl(Decl::Var(v)) => {
                //# VariableStatement : var VariableDeclarationList ;
                //# 1. Return BoundNames of VariableDeclarationList.
                BoundNames(self.0).VariableDeclarationList(v)
            }
            Stmt::DoWhile(s) => {
                //# DoWhileStatement : do Statement while ( Expression ) ;
                //# 1. Return the VarDeclaredNames of Statement.
                self.Statement(&*s.body);
            }
            Stmt::While(s) => {
                //# WhileStatement : while ( Expression ) Statement
                //# 1. Return the VarDeclaredNames of Statement.
                self.Statement(&*s.body);
            }
            Stmt::For(s) => {
                match &s.init {
                    Some(VarDeclOrExpr::VarDecl(v)) => {
                        //# ForStatement : for ( var VariableDeclarationList ; Expressionopt ; Expressionopt ) Statement
                        //# 1. Let names1 be BoundNames of VariableDeclarationList.
                        //# 2. Let names2 be VarDeclaredNames of Statement.
                        //# 3. Return the list-concatenation of names1 and names2.
                        BoundNames(self.0).VariableDeclarationList(v);
                        self.Statement(&*s.body);
                    }
                    _ => {
                        //# ForStatement : for ( Expressionopt ; Expressionopt ; Expressionopt ) Statement
                        //# 1. Return the VarDeclaredNames of Statement.
                        self.Statement(&*s.body)
                    }
                }
            }
            Stmt::ForIn(s) => {
                //# ForInOfStatement :
                //  < all variants >
                //# 1. Return the VarDeclaredNames of Statement.
                self.Statement(&*s.body)
            }
            Stmt::ForOf(s) => {
                //# ForInOfStatement :
                //  < all variants >
                //# 1. Let names1 be the BoundNames of ForBinding.
                //# 2. Let names2 be the VarDeclaredNames of Statement.
                //# 3. Return the list-concatenation of names1 and names2.
                match &s.left {
                    VarDeclOrPat::VarDecl(v) => BoundNames(self.0).VariableDeclarationList(v),
                    VarDeclOrPat::Pat(p) => BoundNames(self.0).LexicalBinding(p),
                };

                self.Statement(&*s.body);
            }
            Stmt::If(if_stmt) => {
                //# IfStatement : if ( Expression ) Statement else Statement
                //# 1. Let names1 be VarDeclaredNames of the first Statement.
                //# 2. Let names2 be VarDeclaredNames of the second Statement.
                //# 3. Return the list-concatenation of names1 and names2.
                //# IfStatement : if ( Expression ) Statement
                //# 1. Return the VarDeclaredNames of Statement.
                VarDeclaredNames(self.0).Statement(&*if_stmt.cons);

                if let Some(alt) = &if_stmt.alt {
                    VarDeclaredNames(self.0).Statement(&**alt)
                }
            }
            Stmt::With(s) => {
                //# WithStatement : with ( Expression ) Statement
                //# 1. Return the VarDeclaredNames of Statement.
                self.Statement(&*s.body);
            }
            Stmt::Switch(s) => {
                //# SwitchStatement : switch ( Expression ) CaseBlock
                //# 1. Return the VarDeclaredNames of CaseBlock.
                for case in s.cases.iter() {
                    // what the spec here says in many words is:
                    // run VarDeclardNames on all case bodies
                    self.StatementList(&case.cons);
                }
            }
            Stmt::Try(s) => {
                // what the spec here says in many words is:
                // run VarDeclardNames on all blocks of try
                self.StatementList(&s.block.stmts);

                if let Some(catch) = &s.handler {
                    self.StatementList(&catch.body.stmts)
                }

                if let Some(finally) = &s.finalizer {
                    self.StatementList(&finally.stmts)
                }
            }
            Stmt::Expr(_) => {}
            s => todo!("{:?}", s),
        }
    }
}

struct TopLevelVarDeclaredNames<'names>(&'names mut Vec<String>);

impl TopLevelVarDeclaredNames<'_> {
    fn StatementList(&mut self, stmts: &[Stmt]) {
        // for loop
        //# StatementList : StatementList StatementListItem
        //# 1. Let names1 be TopLevelVarDeclaredNames of StatementList.
        //# 2. Let names2 be TopLevelVarDeclaredNames of StatementListItem.
        //# 3. Return the list-concatenation of names1 and names2.
        for stmt in stmts {
            // match:
            //# StatementListItem : Declaration
            //# 1. If Declaration is Declaration : HoistableDeclaration , then
            //#     a. Return the BoundNames of HoistableDeclaration.
            //# 2. Return a new empty List.
            match stmt {
                Stmt::Decl(Decl::Fn(func)) => {
                    BoundNames(self.0).HoistableDeclaration(func);
                }
                //# LabelledItem : Statement
                //# 1. If Statement is Statement : LabelledStatement , return TopLevelVarDeclaredNames of Statement.
                //# 2. Return VarDeclaredNames of Statement.
                s => VarDeclaredNames(self.0).Statement(s),
            }
        }
    }
}

struct BoundNames<'names>(&'names mut Vec<String>);

/// <https://tc39.es/ecma262/#sec-static-semantics-boundnames>
impl BoundNames<'_> {
    pub fn compute(var_declarator: &VarDeclarator) -> Vec<String> {
        let mut names = Vec::new();
        BoundNames(&mut names).LexicalBinding(&var_declarator.name);
        names
    }

    pub fn compute_fn(func: &FnDecl) -> Vec<String> {
        let mut names = Vec::new();
        BoundNames(&mut names).HoistableDeclaration(&func);
        names
    }

    pub fn compute_cls(cls: &ClassDecl) -> Vec<String> {
        let mut names = Vec::new();
        BoundNames(&mut names).Class(&cls);
        names
    }

    fn VariableDeclarationList(&mut self, v: &VarDecl) {
        //# LexicalDeclaration : LetOrConst BindingList ;
        //# 1. Return the BoundNames of BindingList.

        // for loop
        //# BindingList : BindingList , LexicalBinding
        //# 1. Let names1 be the BoundNames of BindingList.
        //# 2. Let names2 be the BoundNames of LexicalBinding.
        //# 3. Return the list-concatenation of names1 and names2.
        for lex in v.decls.iter() {
            self.LexicalBinding(&lex.name);
        }
    }

    fn Class(&mut self, class: &ClassDecl) {
        self.swc_identifier(&class.ident);

        // TODO: what to do about if there is no identifier for the class?
        // should return
    }

    fn HoistableDeclaration(&mut self, func: &FnDecl) {
        //# FunctionDeclaration[?Yield, ?Await, ?Default]
        //# GeneratorDeclaration[?Yield, ?Await, ?Default]
        //# AsyncFunctionDeclaration[?Yield, ?Await, ?Default]
        //# AsyncGeneratorDeclaration[?Yield, ?Await, ?Default]
        self.swc_identifier(&func.ident);
    }

    fn LexicalBinding(&mut self, pat: &Pat) {
        //# LexicalBinding : BindingIdentifier Initializeropt
        //# 1. Return the BoundNames of BindingIdentifier.
        //# LexicalBinding : BindingPattern Initializer
        //# 1. Return the BoundNames of BindingPattern.
        match pat {
            Pat::Ident(ident) => self.BindingIdentifier(ident),
            Pat::Array(arr) => {
                // if a pat is missing, then it must be the `Elision` pattern
                // othewise, it has to be something we call BoundNames on
                for pat in arr.elems.iter().flatten() {
                    self.LexicalBinding(pat);
                }
            }
            Pat::Rest(rest) => self.LexicalBinding(&*rest.arg),
            Pat::Object(obj) => {
                for prop in obj.props.iter() {
                    //# BindingProperty : PropertyName : BindingElement
                    //# 1. Return the BoundNames of BindingElement.
                    match prop {
                        ObjectPatProp::KeyValue(pat) => self.LexicalBinding(&*pat.value),
                        ObjectPatProp::Assign(_) => todo!(),
                        ObjectPatProp::Rest(pat) => self.LexicalBinding(&*pat.arg),
                    }
                }
            }
            Pat::Assign(_) => todo!(),
            Pat::Invalid(_) => todo!(),
            Pat::Expr(_) => todo!(),
        }
    }

    fn BindingIdentifier(&mut self, ident: &BindingIdent) {
        //# BindingIdentifier : Identifier
        //# 1. Return a List whose sole element is the StringValue of Identifier.
        //# BindingIdentifier : yield
        //# 1. Return a List whose sole element is "yield".
        //# BindingIdentifier : await
        //# 1. Return a List whose sole element is "await".
        self.swc_identifier(&ident.id);
    }

    fn swc_identifier(&mut self, ident: &Ident) {
        self.0.push(ident.sym.to_string());
    }
}

enum VarDeclaratorKind {
    /// If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
    DeclaredFunctionName(FnDecl),
    /// a. If d is a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
    DeclaredVariableName(VarDeclarator),
}

struct VarScopedDeclarations<'names>(&'names mut Vec<VarDeclaratorKind>);

#[allow(non_snake_case)]
impl VarScopedDeclarations<'_> {
    pub fn compute(script: &Script) -> Vec<VarDeclaratorKind> {
        let mut decls = Vec::new();
        VarScopedDeclarations(&mut decls).Script(script);
        decls
    }

    fn Script(&mut self, script: &Script) {
        //# ScriptBody : StatementList
        //# 1. Return TopLevelVarScopedDeclarations of StatementList.
        TopLevelVarScopedDeclarations(self.0).StatementList(&script.body)
    }

    fn StatementList(&mut self, stmts: &[Stmt]) {
        // for loop:
        //# StatementList : StatementList StatementListItem
        //# 1. Let declarations1 be VarScopedDeclarations of StatementList.
        //# 2. Let declarations2 be VarScopedDeclarations of StatementListItem.
        //# 3. Return the list-concatenation of declarations1 and declarations2.
        for stmt in stmts {
            self.Statement(stmt);
        }
    }

    fn Statement(&mut self, stmt: &Stmt) {
        // <https://github.com/engine262/engine262/blob/main/src/static-semantics/VarScopedDeclarations.mjs>
        match stmt {
            Stmt::Block(s) => self.StatementList(&s.stmts),
            Stmt::Empty(_) => {}
            Stmt::If(s) => {
                self.Statement(&*s.cons);

                if let Some(alt) = &s.alt {
                    self.Statement(&**alt);
                }
            }
            Stmt::While(s) => self.Statement(&*s.body),
            Stmt::DoWhile(s) => self.Statement(&*s.body),
            Stmt::For(s) => {
                if let Some(VarDeclOrExpr::VarDecl(v)) = &s.init {
                    self.VariableDeclarationList(v, VarDeclaratorKind::DeclaredVariableName);
                }

                self.Statement(&*s.body);
            }
            Stmt::ForIn(s) => {
                if let VarDeclOrPat::VarDecl(v) = &s.left {
                    self.VariableDeclarationList(v, VarDeclaratorKind::DeclaredVariableName);
                }

                self.Statement(&*s.body);
            }
            Stmt::ForOf(s) => {
                if let VarDeclOrPat::VarDecl(v) = &s.left {
                    self.VariableDeclarationList(&v, VarDeclaratorKind::DeclaredVariableName);
                }

                self.Statement(&*s.body);
            }
            Stmt::With(s) => self.Statement(&*s.body),
            Stmt::Switch(s) => {
                for case in s.cases.iter() {
                    self.StatementList(&case.cons);
                }
            }
            Stmt::Labeled(s) => self.Statement(&*s.body),
            Stmt::Try(s) => {
                self.StatementList(&s.block.stmts);

                if let Some(c) = &s.handler {
                    self.StatementList(&*c.body.stmts);
                }

                if let Some(c) = &s.finalizer {
                    self.StatementList(&c.stmts);
                }
            }
            Stmt::Decl(Decl::Fn(f)) => {
                if let Some(body) = &f.function.body {
                    TopLevelVarScopedDeclarations(self.0).StatementList(&body.stmts)
                }
            }
            _ => {}
        }
    }

    fn VariableDeclarationList<F>(&mut self, v: &VarDecl, f: F)
    where
        F: Fn(VarDeclarator) -> VarDeclaratorKind,
    {
        //# VariableDeclarationList : VariableDeclarationList , VariableDeclaration
        //# 1. Let declarations1 be VarScopedDeclarations of VariableDeclarationList.
        //# 2. Return the list-concatenation of declarations1 and « VariableDeclaration ».
        for d in v.decls.iter() {
            self.0.push(f(d.clone()));
        }
    }
}

struct TopLevelVarScopedDeclarations<'names>(&'names mut Vec<VarDeclaratorKind>);

#[allow(non_snake_case)]
impl TopLevelVarScopedDeclarations<'_> {
    fn StatementList(&mut self, stmts: &[Stmt]) {
        // for loop:
        //# StatementList : StatementList StatementListItem
        //# 1. Let declarations1 be TopLevelVarScopedDeclarations of StatementList.
        //# 2. Let declarations2 be TopLevelVarScopedDeclarations of StatementListItem.
        //# 3. Return the list-concatenation of declarations1 and declarations2.
        for stmt in stmts {
            self.Statement(stmt);
        }
    }

    fn Statement(&mut self, stmt: &Stmt) {
        //# StatementListItem : Statement
        //# 1. If Statement is Statement : LabelledStatement , return TopLevelVarScopedDeclarations of Statement.
        //# 2. Return VarScopedDeclarations of Statement.
        match stmt {
            // this is a hack to get the data in a way that VarScopedDecls understands
            Stmt::Decl(Decl::Fn(f)) => {
                (self.0).push(VarDeclaratorKind::DeclaredFunctionName(f.clone()))
            }
            // this is a hack to get the data in a way that VarScopedDecls understands
            Stmt::Decl(Decl::Class(c)) => VarScopedDeclarations(self.0).VariableDeclarationList(
                &VarDecl {
                    span: c.ident.span,
                    kind: VarDeclKind::Let,
                    declare: false,
                    decls: vec![VarDeclarator {
                        span: c.ident.span,
                        name: Pat::Ident(BindingIdent {
                            id: c.ident.clone(),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                },
                VarDeclaratorKind::DeclaredVariableName,
            ),
            Stmt::Decl(Decl::Var(v)) => {
                VarScopedDeclarations(self.0)
                    .VariableDeclarationList(v, VarDeclaratorKind::DeclaredVariableName);
            }
            _ => VarScopedDeclarations(self.0).Statement(stmt),
        }
    }
}

enum LexicalScopedDecl {
    Function(FnDecl),
    Class(ClassDecl),
    Variable(VarDecl),
}

impl LexicalScopedDecl {
    fn bound_names(&self) -> (bool, Vec<String>) {
        match self {
            LexicalScopedDecl::Function(f) => (false, BoundNames::compute_fn(f)),
            LexicalScopedDecl::Class(f) => (false, BoundNames::compute_cls(f)),
            LexicalScopedDecl::Variable(v) => {
                let mut bound_names = Vec::new();
                for v in v.decls.iter() {
                    bound_names.extend(BoundNames::compute(v));
                }

                (matches!(v.kind, VarDeclKind::Const), bound_names)
            }
        }
    }
}

struct LexicallyScopedDeclarations<'names>(&'names mut Vec<LexicalScopedDecl>);

#[allow(non_snake_case)]
impl LexicallyScopedDeclarations<'_> {
    fn compute(script: &Script) -> Vec<LexicalScopedDecl> {
        let mut names = Vec::new();
        LexicallyScopedDeclarations(&mut names).Script(script);
        names
    }

    fn Script(&mut self, script: &Script) {
        self.StatementList(&script.body);
    }

    fn StatementList(&mut self, stmts: &[Stmt]) {
        // for loop:
        //# StatementList : StatementList StatementListItem
        //# 1. Let declarations1 be LexicallyScopedDeclarations of StatementList.
        //# 2. Let declarations2 be LexicallyScopedDeclarations of StatementListItem.
        //# 3. Return the list-concatenation of declarations1 and declarations2.
        for stmt in stmts {
            self.Statement(stmt);
        }
    }

    fn Statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Decl(Decl::Fn(f)) => {}
            _ => {}
        }
    }
}

struct TopLevelLexicallyScopedDeclarations<'names>(&'names mut Vec<LexicalScopedDecl>);

#[allow(non_snake_case)]
impl TopLevelLexicallyScopedDeclarations<'_> {
    fn StatementList(&mut self, stmts: &[Stmt]) {
        // for loop:
        //# StatementList : StatementList StatementListItem
        //# 1. Let declarations1 be LexicallyScopedDeclarations of StatementList.
        //# 2. Let declarations2 be LexicallyScopedDeclarations of StatementListItem.
        //# 3. Return the list-concatenation of declarations1 and declarations2.
        for stmt in stmts {
            self.Statement(stmt);
        }
    }

    fn Statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Decl(Decl::Class(c)) => {
                //
            }
            Stmt::Decl(Decl::Var(v)) => {

                //
            }
            _ => {}
        }
    }
}
