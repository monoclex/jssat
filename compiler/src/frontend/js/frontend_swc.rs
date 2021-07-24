use std::ops::{Deref, DerefMut};

use rustc_hash::FxHashSet;
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::ast::{
    BindingIdent, BlockStmt, CallExpr, ClassDecl, Decl, Expr, ExprOrSpread, FnDecl, Ident,
    LabeledStmt, ObjectPatProp, Pat, PatOrExpr, Stmt, VarDecl, VarDeclOrExpr, VarDeclOrPat,
};
use swc_ecmascript::{
    ast::Script,
    parser::{Parser, Syntax},
};

use std::iter::FromIterator;

use self::environment_record::{
    EnvironmentRecord, EnvironmentRecordFactory, GlobalEnvironmentRecord,
};
use crate::frontend::{builder::*, ir::*};
use crate::isa::InternalSlot;

pub mod environment_record;

#[test]
fn doesnt_panic() {
    let mut builder = ProgramBuilder::new();
    let env_factory = EnvironmentRecordFactory::new(&mut builder);

    let mut main = builder.start_function_main();
    let block = main.start_block_main().into_dynamic();
    let mut frontend = JsWriter {
        bld: &mut builder,
        bld_fn: &mut main,
        block,
        // the value doesn't matter, it gets set on ScriptEvalution
        running_execution_context_lexical_environment: RegisterId::default(),
        env_factory,
    };

    let s = frontend.bld.constant_str("a string".into());
    let s = frontend.make_string(s);
    let condition = frontend.compare_equal(s, s);
    let r = frontend.perform_if_else_w_value(
        condition,
        |me| {
            let constant = me.bld.constant_str("yes".into());
            me.make_string(constant)
        },
        |me| {
            let constant = me.bld.constant_str("no".into());
            me.make_string(constant)
        },
    );

    let cmp_const = frontend.bld.constant_str("yes".into());
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
    let env_factory = EnvironmentRecordFactory::new(&mut builder);

    let mut main = builder.start_function_main();
    let block = main.start_block_main().into_dynamic();
    let mut frontend = JsWriter {
        bld: &mut builder,
        bld_fn: &mut main,
        block,
        // the value doesn't matter, it gets set on ScriptEvalution
        running_execution_context_lexical_environment: RegisterId::default(),
        env_factory,
    };

    let (_, realm) = frontend.InitializeHostDefinedRealm();
    let undefined = frontend.block.make_undefined();
    let script_record = frontend.ParseScript((), realm, undefined);
    frontend.ScriptEvaluation(script_record, &script);

    let block = frontend.block.ret(None);
    main.end_block_dyn(block);

    builder.end_function(main);
    builder.finish()
}

struct JsWriter<'builder, const PARAMETERS: usize> {
    bld: &'builder mut ProgramBuilder,
    bld_fn: &'builder mut FunctionBuilder<PARAMETERS>,
    block: DynBlockBuilder,
    /// this is a hack, shouldn't be here... probably
    running_execution_context_lexical_environment: RegisterId,
    env_factory: EnvironmentRecordFactory,
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
        let empty = self
            .block
            .make_string(self.bld.constant_str("empty".into()));
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
        //# a. Let intrinsics be realmRec.[[Intrinsics]].
        //# b. Set globalObj to ! OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]]).
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

    /// <https://tc39.es/ecma262/#sec-newglobalenvironment>
    pub fn NewGlobalEnvironment(
        &mut self,
        G: RegisterId,
        thisValue: RegisterId,
    ) -> EnvironmentRecord {
        //# 1. Let objRec be NewObjectEnvironment(G, false, null).
        //# 2. Let dclRec be a new declarative Environment Record containing no bindings.
        let dclRec = self.env_factory.make_decl_env_rec(&mut self.block);

        //# 3. Let env be a new global Environment Record.
        let env = self.env_factory.make_global_env_rec(&mut self.block);

        //# 4. Set env.[[ObjectRecord]] to objRec.
        //# 5. Set env.[[GlobalThisValue]] to thisValue.
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

    /// <https://tc39.es/ecma262/#sec-setdefaultglobalbindings>
    pub fn SetDefaultGlobalBindings(&mut self, realmRec: RegisterId) -> RegisterId {
        self.comment("SetDefaultGlobalBindings");

        //# 1. Let global be realmRec.[[GlobalObject]].
        let global = self
            .block
            .record_get_slot(realmRec, InternalSlot::GlobalObject);
        //# 2. For each property of the Global Object specified in clause 19, do
        //# a. Let name be the String value of the property name.
        //# b. Let desc be the fully populated data Property Descriptor for the property, containing the specified attributes for the property. For properties listed in 19.2, 19.3, or 19.4 the value of the [[Value]] attribute is the corresponding intrinsic object from realmRec.
        //# c. Perform ? DefinePropertyOrThrow(global, name, desc).
        //# 3. Return global.
        self.NormalCompletion(global)
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
        // NOTE: `globalEnv` is NOT THE GLOBAL OBJECT.
        let theActualGlobalObject = self
            .block
            .record_get_slot(scriptRecordRealm, InternalSlot::GlobalObject);
        self.running_execution_context_lexical_environment = theActualGlobalObject;

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
            //# b. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
            //# c. Let hasRestrictedGlobal be ? env.HasRestrictedGlobalProperty(name).
            //# d. If hasRestrictedGlobal is true, throw a SyntaxError exception.
        }

        //# 5. For each element name of varNames, do
        for elem in varNames {
            let name = self.bld.constant_str_utf16(elem);
            let name = self.make_string(name);
            //# a. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
        }

        //# 6. Let varDeclarations be the VarScopedDeclarations of script.
        //# 7. Let functionsToInitialize be a new empty List.
        //# 8. Let declaredFunctionNames be a new empty List.
        //# 9. For each element d of varDeclarations, in reverse List order, do
        //# a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
        //# i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
        //# ii. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
        //# iii. Let fn be the sole element of the BoundNames of d.
        //# iv. If fn is not an element of declaredFunctionNames, then
        //# 1. Let fnDefinable be ? env.CanDeclareGlobalFunction(fn).
        //# 2. If fnDefinable is false, throw a TypeError exception.
        //# 3. Append fn to declaredFunctionNames.
        //# 4. Insert d as the first element of functionsToInitialize.
        //# 10. Let declaredVarNames be a new empty List.
        //# 11. For each element d of varDeclarations, do
        //# a. If d is a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
        //# i. For each String vn of the BoundNames of d, do
        //# 1. If vn is not an element of declaredFunctionNames, then
        //# a. Let vnDefinable be ? env.CanDeclareGlobalVar(vn).
        //# b. If vnDefinable is false, throw a TypeError exception.
        //# c. If vn is not an element of declaredVarNames, then
        //# i. Append vn to declaredVarNames.
        //# 12. NOTE: No abnormal terminations occur after this algorithm step if the global object is an ordinary object. However, if the global object is a Proxy exotic object it may exhibit behaviours that cause abnormal terminations in some of the following steps.
        //# 13. NOTE: Annex B.3.3.2 adds additional steps at this point.
        //# 14. Let lexDeclarations be the LexicallyScopedDeclarations of script.
        //# 15. Let privateEnv be null.
        //# 16. For each element d of lexDeclarations, do
        //# a. NOTE: Lexically declared names are only instantiated here but not initialized.
        //# b. For each element dn of the BoundNames of d, do
        //# i. If IsConstantDeclaration of d is true, then
        //# 1. Perform ? env.CreateImmutableBinding(dn, true).
        //# ii. Else,
        //# 1. Perform ? env.CreateMutableBinding(dn, false).
        //# 17. For each Parse Node f of functionsToInitialize, do
        //# a. Let fn be the sole element of the BoundNames of f.
        //# b. Let fo be InstantiateFunctionObject of f with arguments env and privateEnv.
        //# c. Perform ? env.CreateGlobalFunctionBinding(fn, fo, false).
        //# 18. For each String vn of declaredVarNames, do
        //# a. Perform ? env.CreateGlobalVarBinding(vn, false).
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
            // unidiomatic, idc
            env_value = match env {
                Some(it) => it,
                _ => unreachable!(),
            };
        }
        let env = EnvironmentRecord::new_with_register_unchecked(env_value);

        //# 2. Assert: env is an Environment Record.
        //# 3. If the code matching the syntactic production that is being
        //#    evaluated is contained in strict mode code, let strict be true;
        //#    else let strict be false.
        // TODO: handle non strict
        let strict = true;

        //# 4. Return ? GetIdentifierReference(env, name, strict).
        let completion_record = self.GetIdentifierReference(env, name, strict);
        let result = self.ReturnIfAbrupt(completion_record);
        // TODO: is this the right thing to do?
        self.NormalCompletion(result)
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
        self
            .comment(Box::leak(Box::new(format!("ok we are calling GetIdentifierRef with {:?}, {:?}, {:?} and globalEnv should be {:?}", env, name, strict, k))).as_str());

        // TODO: properly do stuff
        // for now, just always return `env[name]`
        let func = self.record_get_prop(env.register, name);
        return self.NormalCompletion(func);

        //# 1. If env is the value null, then
        let null = self.make_null();
        let condition = self.compare_equal(env.register, null);
        self.perform_if(condition, |me| {
            //# a. Return the Reference Record { [[Base]]: unresolvable, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
            // TODO: return that reference record
            // this involves some weird control flow, idk how to handle it rn
        });
        //# 2. Let exists be ? env.HasBinding(name).
        // TODO: figure this out
        //# 3. If exists is true, then
        //# a. Return the Reference Record { [[Base]]: env, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
        // TODO: we assume that we'll always have it
        let record = self.record_new();
        self.record_set_slot(record, InternalSlot::Base, env.register);
        self.record_set_slot(record, InternalSlot::ReferenceName, name);
        // TODO: make boolean
        let strict = self.make_undefined();
        self.record_set_slot(record, InternalSlot::Strict, strict);
        // TODO: make `empty`
        let empty = self.make_undefined();
        self.record_set_slot(record, InternalSlot::ThisValue, empty);
        // TODO: the `NormalCompletion` was added becuase code was wrong
        // are we wrong or is the spec wrong idk, maybe ill find out l8r
        self.NormalCompletion(record)
        //# 4. Else,
        //# a. Let outer be env.[[OuterEnv]].
        //# b. Return ? GetIdentifierReference(outer, name, strict).
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
        let print = {
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

        // TODO: properly make a function
        let func_obj = self.record_new();
        let fnptr = self.make_fnptr(print.id);
        self.record_set_slot(func_obj, InternalSlot::Call, fnptr);

        let print_text = self.bld.constant_str_utf16("print".into());
        let key = self.make_string(print_text);
        self.record_set_prop(global, key, func_obj);
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

                // TODO: do arguments
                let hello_world = self.bld.constant_str_utf16("Hello, World!".into());
                let hello_world = self.make_string(hello_world);

                //# 3. Let thisCall be this CallExpression.
                //# 4. Let tailCall be IsInTailPosition(thisCall).
                //# 5. Return ? EvaluateCall(func, ref, Arguments, tailCall).
                let completion_record = self.EvaluateCall(func, hello_world, args, ());
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
        self.comment("NormalCompletion");

        //# 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]: empty }.
        let completion_record = self.record_new();
        let normal = self
            .block
            .make_string(self.bld.constant_str("normal".into()));
        self.record_set_slot(completion_record, InternalSlot::Type, normal);
        self.record_set_slot(completion_record, InternalSlot::Value, argument);
        let empty = self
            .block
            .make_string(self.bld.constant_str("empty".into()));
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
        let normal_completion = self
            .block
            .make_string(self.bld.constant_str("normal".into()));
        self.compare_equal(completion_type, normal_completion)
    }

    fn is_abrupt_completion(&mut self, record: RegisterId) -> RegisterId {
        self.comment("is_abrupt_completion");

        let is_normal_completion = self.is_normal_completion(record);
        self.negate(is_normal_completion)
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
                Stmt::Decl(Decl::Var(lex)) => BoundNames(self.0).Lexical(lex),
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
    fn VariableDeclarationList(&mut self, v: &VarDecl) {
        for lex in v.decls.iter() {
            self.LexicalBinding(&lex.name);
        }
    }

    fn Class(&mut self, class: &ClassDecl) {
        self.swc_identifier(&class.ident);

        // TODO: what to do about if there is no identifier for the class?
        // should return
    }

    fn Lexical(&mut self, lex: &VarDecl) {
        //# LexicalDeclaration : LetOrConst BindingList ;
        //# 1. Return the BoundNames of BindingList.

        // for loop
        //# BindingList : BindingList , LexicalBinding
        //# 1. Let names1 be the BoundNames of BindingList.
        //# 2. Let names2 be the BoundNames of LexicalBinding.
        //# 3. Return the list-concatenation of names1 and names2.
        for decl in lex.decls.iter() {
            self.LexicalBinding(&decl.name);
        }
    }

    fn HoistableDeclaration(&mut self, func: &FnDecl) {
        //# FunctionDeclaration[?Yield, ?Await, ?Default]
        //# GeneratorDeclaration[?Yield, ?Await, ?Default]
        //# AsyncFunctionDeclaration[?Yield, ?Await, ?Default]
        //# AsyncGeneratorDeclaration[?Yield, ?Await, ?Default]
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
