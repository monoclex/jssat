#[cfg(feature = "link-swc")]
use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::ast::{
    BlockStmt, CallExpr, Decl, Expr, ExprOrSpread, ExprStmt, LabeledStmt, Stmt,
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
        // the value doesn't matter, it gets set on ScriptEvalution
        running_execution_context_lexical_environment: RegisterId::default(),
    };

    frontend.InitializeHostDefinedRealm();
    let undefined = frontend.block.make_undefined();
    let script_record = frontend.ParseScript((), undefined, undefined);
    frontend.ScriptEvaluation(script_record, &script);

    let block = frontend.block.ret(None);
    main.end_block_dyn(block);

    builder.end_function(main);
    builder.finish()
}

struct JsWriter<'builder, 'name, const PARAMETERS: usize> {
    bld: &'builder mut ProgramBuilder,
    bld_fn: &'builder mut FunctionBuilder<'name, PARAMETERS>,
    block: DynBlockBuilder,
    /// this is a hack, shouldn't be here... probably
    running_execution_context_lexical_environment: RegisterId,
}

#[allow(non_snake_case)]
impl<'b, 'n, const PARAMS: usize> JsWriter<'b, 'n, PARAMS> {
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
        // TODO: be in accordance with spec later
        // let global = self.block.make_undefined();
        let global = self.block.record_new();
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

    /// https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
    pub fn AddRestrictedFunctionProperties(&mut self, F: RegisterId, realm: RegisterId) {
        // TODO: implement this
    }

    /// https://tc39.es/ecma262/#sec-addrestrictedfunctionproperties
    pub fn SetRealmGlobalObject(
        &mut self,
        realmRec: RegisterId,
        globalObj: RegisterId,
        thisValue: RegisterId,
    ) {
        // TODO: implement this
        // 1. If globalObj is undefined, then
        // a. Let intrinsics be realmRec.[[Intrinsics]].
        // b. Set globalObj to ! OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]]).
        // 2. Assert: Type(globalObj) is Object.
        // 3. If thisValue is undefined, set thisValue to globalObj.
        // 4. Set realmRec.[[GlobalObject]] to globalObj.
        self.block
            .record_set_slot(realmRec, "GlobalObject", globalObj);
        // 5. Let newGlobalEnv be NewGlobalEnvironment(globalObj, thisValue).
        // 6. Set realmRec.[[GlobalEnv]] to newGlobalEnv.
        // 7. Return realmRec.
    }

    /// https://tc39.es/ecma262/#sec-setdefaultglobalbindings
    pub fn SetDefaultGlobalBindings(&mut self, realmRec: RegisterId) -> RegisterId {
        // 1. Let global be realmRec.[[GlobalObject]].
        let global = self.block.record_get_slot(realmRec, "GlobalObject");
        // 2. For each property of the Global Object specified in clause 19, do
        // TODO: implement global object stuff
        let print_stub = {
            let (mut print_stub, [any]) = self.bld.start_function("print_stub");

            let print = self.bld.external_function(
                "jssatrt_print_any",
                [FFIValueType::Runtime, FFIValueType::Any],
                Returns::Void,
            );

            let mut block = print_stub.start_block_main();
            let runtime = block.get_runtime();
            block.call_external_function(print, [runtime, any]);
            print_stub.end_block(block.ret(None));

            self.bld.end_function(print_stub)
        };

        let func_obj = self.block.record_new();
        let fnptr = self.block.make_fnptr(print_stub.id);
        self.block.record_set_slot(func_obj, "Call", fnptr);

        let print_text = self.bld.constant_str_utf16("", "print".into());
        let key = self.block.make_string(print_text);
        self.block.record_set_slot(global, "slot", key);
        // a. Let name be the String value of the property name.
        // b. Let desc be the fully populated data Property Descriptor for the property, containing the specified attributes for the property. For properties listed in 19.2, 19.3, or 19.4 the value of the [[Value]] attribute is the corresponding intrinsic object from realmRec.
        // c. Perform ? DefinePropertyOrThrow(global, name, desc).
        // 3. Return global.
        self.NormalCompletion(global)
    }

    /// https://tc39.es/ecma262/#sec-parse-script
    ///
    /// The way this method handles scripts, is by pretending like it parsed the script.
    /// Really, we just emit the corresponding code as we comb over the script ourselves.
    pub fn ParseScript(
        &mut self,
        sourceText: (),
        realm: RegisterId,
        hostDefined: RegisterId,
    ) -> RegisterId {
        // 1. Assert: sourceText is an ECMAScript source text (see clause 11).
        // 2. Let body be ParseText(sourceText, Script).
        // 3. If body is a List of errors, return body.
        // 4. Return Script Record { [[Realm]]: realm, [[ECMAScriptCode]]: body, [[HostDefined]]: hostDefined }.
        let script_record = self.block.record_new();
        self.block.record_set_slot(script_record, "Realm", realm);
        let undefined = self.block.make_undefined();
        self.block
            .record_set_slot(script_record, "ECMAScriptCode", undefined);
        self.block
            .record_set_slot(script_record, "HostDefined", undefined);
        script_record
    }

    /// https://tc39.es/ecma262/#sec-runtime-semantics-scriptevaluation
    pub fn ScriptEvaluation(&mut self, scriptRecord: RegisterId, script: &Script) -> RegisterId {
        // 1. Let globalEnv be scriptRecord.[[Realm]].[[GlobalEnv]].
        let scriptRecordRealm = self.block.record_get_slot(scriptRecord, "Realm");
        let globalEnv = self.block.record_get_slot(scriptRecordRealm, "GlobalEnv");
        // 2. Let scriptContext be a new ECMAScript code execution context.
        let scriptContext = self.block.record_new();
        // 3. Set the Function of scriptContext to null.
        let null = self.block.make_null();
        self.block.record_set_slot(scriptContext, "Function", null);
        // 4. Set the Realm of scriptContext to scriptRecord.[[Realm]].
        self.block
            .record_set_slot(scriptContext, "Realm", scriptRecordRealm);
        // 5. Set the ScriptOrModule of scriptContext to scriptRecord.
        self.block
            .record_set_slot(scriptContext, "ScriptOrModule", scriptRecordRealm);
        // 6. Set the VariableEnvironment of scriptContext to globalEnv.
        self.block
            .record_set_slot(scriptContext, "VariableEnvironment", globalEnv);
        // 7. Set the LexicalEnvironment of scriptContext to globalEnv.
        self.block
            .record_set_slot(scriptContext, "LexicalEnvironment", globalEnv);
        // 8. Set the PrivateEnvironment of scriptContext to null.
        self.block
            .record_set_slot(scriptContext, "PrivateEnvironment", null);
        // 9. Suspend the currently running execution context.
        // TODO: ?
        // 10. Push scriptContext onto the execution context stack; scriptContext is now the running execution context.
        // TODO: this is a hack, should be using proepr execution context stack things
        self.running_execution_context_lexical_environment = globalEnv;
        println!("!!! RUNNING EXEC CTX: {}", globalEnv);
        // 11. Let scriptBody be scriptRecord.[[ECMAScriptCode]].
        let scriptBody = script;
        // 12. Let result be GlobalDeclarationInstantiation(scriptBody, globalEnv).
        let mut result = self.GlobalDeclarationInstantiation(scriptBody, globalEnv);
        // 13. If result.[[Type]] is normal, then
        let is_normal = self.is_normal_completion(result);
        self.perform_if(is_normal, |me| {
            // a. Set result to the result of evaluating scriptBody.
            result = me.evaluate_script(script);
        });
        // 14. If result.[[Type]] is normal and result.[[Value]] is empty, then
        let is_normal = self.is_normal_completion(result);
        // TODO: figure out if we can implement this part
        // a. Set result to NormalCompletion(undefined).
        // 15. Suspend scriptContext and remove it from the execution context stack.
        // TODO: ?
        // 16. Assert: The execution context stack is not empty.
        // TODO: ?
        // 17. Resume the context that is now on the top of the execution context stack as the running execution context.
        // TODO: ?
        // 18. Return Completion(result).

        let undefined = self.block.make_undefined();
        self.NormalCompletion(undefined)
    }

    /// https://tc39.es/ecma262/#sec-globaldeclarationinstantiation
    pub fn GlobalDeclarationInstantiation(
        &mut self,
        script: &Script,
        env: RegisterId,
    ) -> RegisterId {
        // 1. Assert: env is a global Environment Record.
        // 2. Let lexNames be the LexicallyDeclaredNames of script.
        let lexNames = ECMA262Script(script).LexicallyDeclaredNames();
        // 3. Let varNames be the VarDeclaredNames of script.
        // TODO: figure out this crud lololololololololololOLOLOLOLOLOLO IM NOT CRYING YOU'RE CRYING
        // 4. For each element name of lexNames, do
        // a. If env.HasVarDeclaration(name) is true, throw a SyntaxError exception.
        // b. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
        // c. Let hasRestrictedGlobal be ? env.HasRestrictedGlobalProperty(name).
        // d. If hasRestrictedGlobal is true, throw a SyntaxError exception.
        // 5. For each element name of varNames, do
        // a. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
        // 6. Let varDeclarations be the VarScopedDeclarations of script.
        // 7. Let functionsToInitialize be a new empty List.
        // 8. Let declaredFunctionNames be a new empty List.
        // 9. For each element d of varDeclarations, in reverse List order, do
        // a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
        // i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
        // ii. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
        // iii. Let fn be the sole element of the BoundNames of d.
        // iv. If fn is not an element of declaredFunctionNames, then
        // 1. Let fnDefinable be ? env.CanDeclareGlobalFunction(fn).
        // 2. If fnDefinable is false, throw a TypeError exception.
        // 3. Append fn to declaredFunctionNames.
        // 4. Insert d as the first element of functionsToInitialize.
        // 10. Let declaredVarNames be a new empty List.
        // 11. For each element d of varDeclarations, do
        // a. If d is a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
        // i. For each String vn of the BoundNames of d, do
        // 1. If vn is not an element of declaredFunctionNames, then
        // a. Let vnDefinable be ? env.CanDeclareGlobalVar(vn).
        // b. If vnDefinable is false, throw a TypeError exception.
        // c. If vn is not an element of declaredVarNames, then
        // i. Append vn to declaredVarNames.
        // 12. NOTE: No abnormal terminations occur after this algorithm step if the global object is an ordinary object. However, if the global object is a Proxy exotic object it may exhibit behaviours that cause abnormal terminations in some of the following steps.
        // 13. NOTE: Annex B.3.3.2 adds additional steps at this point.
        // 14. Let lexDeclarations be the LexicallyScopedDeclarations of script.
        // 15. Let privateEnv be null.
        // 16. For each element d of lexDeclarations, do
        // a. NOTE: Lexically declared names are only instantiated here but not initialized.
        // b. For each element dn of the BoundNames of d, do
        // i. If IsConstantDeclaration of d is true, then
        // 1. Perform ? env.CreateImmutableBinding(dn, true).
        // ii. Else,
        // 1. Perform ? env.CreateMutableBinding(dn, false).
        // 17. For each Parse Node f of functionsToInitialize, do
        // a. Let fn be the sole element of the BoundNames of f.
        // b. Let fo be InstantiateFunctionObject of f with arguments env and privateEnv.
        // c. Perform ? env.CreateGlobalFunctionBinding(fn, fo, false).
        // 18. For each String vn of declaredVarNames, do
        // a. Perform ? env.CreateGlobalVarBinding(vn, false).
        // 19. Return NormalCompletion(empty).
        // TODO: implement ZST `empty`
        let empty = self.block.make_undefined();
        self.NormalCompletion(empty)
    }

    pub fn GetValue(&mut self, V: RegisterId) -> RegisterId {
        // 1. ReturnIfAbrupt(V).
        let value = self.ReturnIfAbrupt(V);
        // 2. If V is not a Reference Record, return V.
        value
        // TODO: handle this
        // 3. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
        // TODO: handle this
        // 4. If IsPropertyReference(V) is true, then
        // a. Let baseObj be ! ToObject(V.[[Base]]).
        // b. If IsPrivateReference(V) is true, then
        // i. Return ? PrivateGet(V.[[ReferencedName]], baseObj).
        // c. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
        // 5. Else,
        // a. Let base be V.[[Base]].
        // b. Assert: base is an Environment Record.
        // c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
    }

    pub fn ResolveBinding(&mut self, name: RegisterId, env: Option<RegisterId>) -> RegisterId {
        let mut env_value;
        // 1. If env is not present or if env is undefined, then
        if let None = env {
            // a. Set env to the running execution context's LexicalEnvironment.
            env_value = self.running_execution_context_lexical_environment;
        } else {
            // unidiomatic, idc
            env_value = env.unwrap();
        }
        let env = env_value;

        // 2. Assert: env is an Environment Record.
        // 3. If the code matching the syntactic production that is being
        //    evaluated is contained in strict mode code, let strict be true;
        //    else let strict be false.
        // TODO: handle non strict
        let strict = true;
        // 4. Return ? GetIdentifierReference(env, name, strict).
        let completion_record = self.GetIdentifierReference(env, name, strict);
        self.ReturnIfAbrupt(completion_record)
    }

    pub fn GetIdentifierReference(
        &mut self,
        env: RegisterId,
        name: RegisterId,
        strict: bool,
    ) -> RegisterId {
        // 1. If env is the value null, then
        let null = self.block.make_null();
        let condition = self.block.compare_equal(env, null);
        self.perform_if(condition, |me| {
            // a. Return the Reference Record { [[Base]]: unresolvable, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
            // TODO: return that reference record
            // this involves some weird control flow, idk how to handle it rn
        });
        // 2. Let exists be ? env.HasBinding(name).
        // TODO: figure this out
        // 3. If exists is true, then
        // a. Return the Reference Record { [[Base]]: env, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
        // TODO: we assume that we'll always have it
        let record = self.block.record_new();
        self.block.record_set_slot(record, "Base", env);
        self.block.record_set_slot(record, "ReferenceName", name);
        // TODO: make boolean
        let strict = self.block.make_undefined();
        self.block.record_set_slot(record, "Strict", strict);
        // TODO: make `empty`
        let empty = self.block.make_undefined();
        self.block.record_set_slot(record, "ThisValue", empty);
        record
        // 4. Else,
        // a. Let outer be env.[[OuterEnv]].
        // b. Return ? GetIdentifierReference(outer, name, strict).
    }

    /// https://tc39.es/ecma262/#sec-evaluatecall
    pub fn EvaluateCall(
        &mut self,
        func: RegisterId,
        r#ref: RegisterId,
        arguments: &Vec<ExprOrSpread>,
        tailPosition: (),
    ) -> RegisterId {
        // TODO: actually call the function
        // help this is so much effort just to get `print("hello world")`
        let virtual_func = self.block.record_get_slot(func, "Call");
        self.block.call_virt(virtual_func, [r#ref]);
        self.block.make_null()
    }

    // RUNTIME SEMANTICS OF SCRIPT
    pub fn evaluate_script(&mut self, script: &Script) -> RegisterId {
        // StatementList : StatementList StatementListItem
        let undefined = self.block.make_undefined();
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
                // 1. Let ref be the result of evaluating CallExpression.
                let r#ref = match callee {
                    swc_ecmascript::ast::ExprOrSuper::Super(s) => todo!(),
                    swc_ecmascript::ast::ExprOrSuper::Expr(e) => self.evaluate_expr(&*e),
                };

                // 2. Let func be ? GetValue(ref).
                let func = self.GetValue(r#ref);

                // 3. Let thisCall be this CallExpression.
                // 4. Let tailCall be IsInTailPosition(thisCall).
                // 5. Return ? EvaluateCall(func, ref, Arguments, tailCall).
                let completion_record = self.EvaluateCall(func, r#ref, args, ());
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
                // 1. Return ? ResolveBinding(StringValue of Identifier).
                let string_value = ident.sym.to_string();
                let string_value = self.bld.constant_str_utf16("", string_value);
                let name = self.block.make_string(string_value);
                let resolve_binding = self.ResolveBinding(name, None);
                self.ReturnIfAbrupt(resolve_binding)
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

    /// https://tc39.es/ecma262/#sec-returnifabrupt
    pub fn ReturnIfAbrupt(&mut self, argument: RegisterId) -> RegisterId {
        // 1. If argument is an abrupt completion, return argument.
        let is_abrupt_completion = self.is_abrupt_completion(argument);

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

    // actual helpers

    fn perform_if<F: FnMut(&'_ mut JsWriter<'b, 'n, PARAMS>)>(
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

    fn is_normal_completion(&mut self, record: RegisterId) -> RegisterId {
        let completion_type = self.block.record_get_slot(record, "Type");
        let normal_completion = self
            .block
            .make_string(self.bld.constant_str("", "normal".into()));
        self.block.compare_equal(completion_type, normal_completion)
    }

    fn is_abrupt_completion(&mut self, record: RegisterId) -> RegisterId {
        let is_normal_completion = self.is_normal_completion(record);
        self.block.negate(is_normal_completion)
    }
}

pub struct ECMA262Script<'script>(&'script Script);

#[allow(non_snake_case)]
impl ECMA262Script<'_> {
    pub fn LexicallyDeclaredNames(&self) -> Vec<String> {
        ECMA262Script::LexicallyDeclaredNames_StatementList(&self.0.body)
    }

    pub fn LexicallyDeclaredNames_Block(block: &BlockStmt) -> Vec<String> {
        // Block : { }

        // 1. Return a new empty List.
        vec![]
    }

    pub fn LexicallyDeclaredNames_StatementList(statement_list: &[Stmt]) -> Vec<String> {
        // this isn't specified in the specification, probably my fault
        if statement_list.len() == 0 {
            panic!();
        }

        // this case isn't specified in the specification, specification's fault
        if statement_list.len() == 1 {
            return ECMA262Script::LexicallyDeclaredNames_StatementListItem(&statement_list[0]);
        }

        // StatementList : StatementList StatementListItem

        // 1. Let names1 be LexicallyDeclaredNames of StatementList.
        let names1 = ECMA262Script::LexicallyDeclaredNames_StatementList(
            &statement_list[..statement_list.len() - 1],
        );
        // 2. Let names2 be LexicallyDeclaredNames of StatementListItem.
        let names2 = ECMA262Script::LexicallyDeclaredNames_StatementListItem(
            &statement_list[statement_list.len() - 1],
        );
        // 3. Return the list-concatenation of names1 and names2.
        let mut concatenation = names1;
        concatenation.extend(names2);
        concatenation
    }

    pub fn LexicallyDeclaredNames_StatementListItem(statement: &Stmt) -> Vec<String> {
        // this weirdness is just because we're using swc and not rolling our own ecmascript parser
        if let Stmt::Decl(declaration) = statement {
            // StatementListItem : Declaration

            // 1. Return the BoundNames of Declaration.
            return ECMA262Script::BoundNames_Declaration(declaration);
        }

        // StatementListItem : Statement

        // 1. If Statement is Statement : LabelledStatement , return LexicallyDeclaredNames of LabelledStatement.
        if let Stmt::Labeled(labelled_statement) = statement {
            return ECMA262Script::LexicallyDeclaredNames_LabelledStatement(labelled_statement);
        }
        // 2. Return a new empty List.
        vec![]
    }

    pub fn BoundNames_Declaration(declaration: &Decl) -> Vec<String> {
        match declaration {
            Decl::Class(_) => todo!(),
            Decl::Fn(_) => todo!(),
            Decl::Var(_) => todo!(),
            Decl::TsInterface(_) => todo!(),
            Decl::TsTypeAlias(_) => todo!(),
            Decl::TsEnum(_) => todo!(),
            Decl::TsModule(_) => todo!(),
        }
    }

    pub fn LexicallyDeclaredNames_LabelledStatement(
        labelled_statement: &LabeledStmt,
    ) -> Vec<String> {
        // LabelledStatement : LabelIdentifier : LabelledItem

        // 1. Return the LexicallyDeclaredNames of LabelledItem.
        match &*labelled_statement.body {
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
            Stmt::Expr(_) => todo!(),
        }
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
