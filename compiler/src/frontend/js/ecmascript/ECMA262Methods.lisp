;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ECMA262 IR FILE IMPLEMENTATION ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file is an implementation of ECMA262 https://tc39.es/ecma262/
; as a JSSAT IR File.
;
; A JSSAT IR File can be briefly described as follows: a list of S-expressions,
; where:
;
; - a `(def ...)` S-expression represents a rule rewrite
; - a `(section ...)` S-expression represents an ECMAScript function
;
; Once a JSSAT IR File is parsed, it produces the list of S-expressions, with all
; rule rewrites applied. It is then used to generate JSSAT IR, which is compiled
; into the program.
;
; For more information on JSSAT IR Files, see the `ir_file` crate.
;

(def (and :a :b) (:a and :b))
(def (or :a :b) (:a or :b))
(def (and3 :a :b :c) (and (and :a :b) :c))
(def (and4 :a :b :c :d) (and3 :a :b (and :c :d)))
(def (and6 :1 :2 :3 :4 :5 :6) (and (and3 :1 :2 :3) (and3 :4 :5 :6)))
(def (or3 :1 :2 :3) (or (or :1 :2) :3))
(def (or7 :1 :2 :3 :4 :5 :6 :7) (or3 (or3 :1 :2 :3) (or3 :4 :5 :6) :7))
(def (both :a :b (:x :y)) (and (:a :x :y) (:b :x :y)))
(def (both :1 :2 :f) (and (:f :1) (:f :2)))
(def (either :a :b (:x :y)) (or (:a :x :y) (:b :x :y)))
(def (todo) (assert false "TODO"))
(def (:a - :b) (:a + (not :b)))
(def (throw :x) (return (ThrowCompletion :x)))

(def
  (elif :condition :then-expr :end-expr)
  ((if :condition :then-expr :end-expr)))

(def
  (if-elif3-else
   (:cond :then)
   (:cond1 :then1)
   (:cond2 :then2)
   :else)
  (if :cond
      :then
      (elif :cond1 :then1
            (elif :cond2 :then2 :else))))

(def (exec-ctx-stack-push :x) (list-push (get-global -> JSSATExecutionContextStack) :x))
(def (exec-ctx-stack-size) (list-len (get-global -> JSSATExecutionContextStack)))

(def for-item (list-get :jssat_list :jssat_i))
(def for-item-rev (list-get :jssat_list (:jssat_len - (:jssat_i + 1))))

(def
  (for :list :body)
  (loop
        ((jssat_list = :list) (jssat_i = 0) (jssat_len = (list-len :list)))
        (:jssat_i < :jssat_len)
        ((jssat_list = :jssat_list) (jssat_i = (:jssat_i + 1)) (jssat_len = :jssat_len))
        :body))

(def
  (list-push :list :x)
  (list-set :list (math-max ((list-len :list) - 1) 0) :x))

(def
  (list-new-1 :1)
  (expr-block
   ((jssat_list_temp = list-new)
    (list-push :jssat_list_temp :1)
    (:jssat_list_temp))))

(def
  (list-new-2 :1 :2)
  (expr-block
   ((jssat_list_temp = list-new)
    (list-push :jssat_list_temp :1)
    (list-push :jssat_list_temp :2)
    (:jssat_list_temp))))

(def
  (list-concat :a :b)
  (expr-block
   ((for :b ((list-push :a for-item)))
    (:a))))

; i'm too lazy to change let exprs to expr blocks atm
(def (expr-block :x) (let
                       _discard
                       =
                       0
                       in
                       :x))

; only `not x`, `x == y`, and `x < y` are implemented. create the other operators here
(def (:x != :y) (not (:x == :y)))
(def (:x <= :y) ((:x == :y) or (:x < :y)))
(def (:x > :y) (:y < :x))
(def (:x >= :y) (:y <= :x))

(def String Bytes)
(def BigInt BigNumber)

(def null (trivial Null))
(def undefined (trivial Undefined))
(def normal (trivial Normal))
(def empty (trivial Empty))
(def (trivial throw) (trivial Throw))

(def (is-undef :x) (:x == undefined))
(def (isnt-undef :x) (:x != undefined))
(def (is-null :x) (:x == null))
(def (isnt-null :x) (:x != null))
(def (is-true :x) (:x == true))
(def (is-false :x) (:x == false))
(def (is-normal :x) (:x == normal))
(def (is-empty :x) (:x == empty))
(def (is-string :x) (is-type-of String :x))
(def (is-symbol :x) (is-type-of Symbol :x))
(def (is-number :x) (is-type-of Number :x))
(def (is-bigint :x) (is-type-of BigInt :x))
(def (is-bool :x) (is-type-of Boolean :x))
(def (is-object :x) (is-type-of Record :x))

(def
  (match-pn :parseNode :kind :variant_idx)
  (and (:parseNode -> JSSATParseNodeKind == :kind) (:parseNode -> JSSATParseNodeVariant == :variant_idx)))

(def
  (is-pn :kind :variant_idx)
  (match-pn :parseNode (trivial-node :kind) :variant_idx))

(def (isnt-type-as :x :y) (not (is-type-as :x :y)))

(def
  (math-max :x :y)
  (expr-block
   ((if (:x > :y)
        ((:x))
        ((:y))))))

(def (:1 -> :2) (record-get-slot :1 :2))
(def (:1 -> :2 == :3) ((:1 -> :2) == :3))
(def (:1 -> :2 -> :3) ((:1 -> :2) -> :3))
(def (:1 => :2) (record-get-prop :1 :2))
(def (:record :slot <- :expr) (record-set-slot :record :slot :expr))
(def (:record :slot <-) (record-del-slot :record :slot))
(def (record-absent-slot :record :slot) (not (record-has-slot :record :slot)))
(def (record-absent-prop :record :prop) (not (record-has-prop :record :prop)))

(def
  (record-do-slot :bind :record :slot :action)
  (if (record-has-slot :record :slot)
      ((:bind = (record-get-slot :record :slot))
       :action)))

(def
  (record-copy-slot-or-default :src :dest :slot :default)
  (if (record-absent-slot :src :slot)
      ((record-set-slot :dest :slot :default))
      ((record-set-slot :dest :slot (record-get-slot :src :slot)))))

(def
  (record-copy-slot-if-present :src :dest :slot)
  (if (record-has-slot :src :slot)
      ((record-set-slot :dest :slot (:src -> :slot)))))

(def (record-absent-slot2 :r :1 :2)
  (and (record-absent-slot :r :1) (record-absent-slot :r :2)))

(def (record-absent-slot6 :r :s1 :s2 :s3 :s4 :s5 :s6)
  (and6 (record-absent-slot :r :s1) (record-absent-slot :r :s2)
        (record-absent-slot :r :s3) (record-absent-slot :r :s4)
        (record-absent-slot :r :s5) (record-absent-slot :r :s6)))

(def (isnt-abrupt-completion :x) (not (is-abrupt-completion :x)))
(def
  (is-abrupt-completion :x)
  (if (record-has-slot :x Type)
      (((record-get-slot :x Type) != normal))
      (false)))

(def
  (is-completion-record :x)
  (and3
   (record-has-slot :x Type)
   (record-has-slot :x Value)
   (record-has-slot :x Target)))

; TODO: somehow use `env` to load the `SyntaxError` object and construct it
(def (SyntaxError :env :msg)
  (:msg))

; "Let <thing> be the sole element of <list>"
(def
  (sole-element :x)
  ((let
     jssat_list
     =
     :x
     in
     (; assert that the list is a list with a singular element
      (assert ((list-len :jssat_list) == 1) "to get the 'sole element' of a list, it must be a singleton list")
      (assert (list-has :jssat_list 1) "sanity check")
      (list-get :jssat_list 0)))))

; 5.2.3.4 ReturnIfAbrupt Shorthands
(def
  (! :OperationName)
  (expr-block
   (;;; 1. Let val be OperationName().
    (val = :OperationName)
    ;;; 2. Assert: val is never an abrupt completion.
    (assert (isnt-abrupt-completion :val) "val is never an abrupt completion")
    ;;; 3. If val is a Completion Record, set val to val.[[Value]].
    (if (is-completion-record :val)
        ((record-get-slot :val Value))
        (:val)))))

; 5.2.3.4 ReturnIfAbrupt Shorthands
(def
  (? :x)
  (expr-block
   ((jssat_arg = :x)
    (;;; 1. If argument is an abrupt completion, return argument.
     (if (is-abrupt-completion :jssat_arg)
         ((return :jssat_arg)
          (unreachable))
         ;;; 2. Else if argument is a Completion Record, set argument to argument.[[Value]].
         (elif (is-completion-record :jssat_arg)
               ((record-get-slot :jssat_arg Value))
               (:jssat_arg)))))))

; 6.2.3.2  NormalCompletion
(def
  (NormalCompletion :x)
  (expr-block
   ((jssat_normal_completion = record-new)
    (:jssat_normal_completion Type <- normal)
    (:jssat_normal_completion Value <- :x)
    (:jssat_normal_completion Target <- empty)
    (:jssat_normal_completion))))

; 6.2.3.3  ThrowCompletion
(def
  (ThrowCompletion :x)
  (expr-block
   ((jssat_throw_completion = record-new)
    (:jssat_throw_completion Type <- (trivial throw))
    (:jssat_throw_completion Value <- :x)
    (:jssat_throw_completion Target <- empty)
    (:jssat_throw_completion))))

;;;;;;;
; virt calls
;;;;;;;

; we use `..` instead of `.` because `.` is a cons cell :v
(def (:env .. HasVarDeclaration :N) (call-virt (:env -> JSSATHasVarDeclaration) :env :N))
(def (:env .. HasLexicalDeclaration :N) (call-virt (:env -> JSSATHasLexicalDeclaration) :env :N))
(def (:env .. HasRestrictedGlobalProperty :N) (call-virt (:env -> JSSATHasRestrictedGlobalProperty) :env :N))

(def (evaluating :x) (NormalCompletion empty))

;;;;;;;;;;;;;;;;;;
; something ; (STATIC SEMANTICS AND RUNTIME SEMANTICS WIP SECTION)
;;;;;;;;;;;;;;;;;;
; well not really jssat behavior, more like implementation of static semantics
; and the way static semantics are is that there's a jssat record for each ast
; node with associated virtual methods that are implemented here
; wait but we have runtime semantics too h m mm

; TODO: these are all ParseNode related
; we need to declare the algorithm steps here, then link to them in the code that
; generates js objects from ecmascript spec

; helpers
(section
  (:0.0.0.0 InitializeJSSATThreadedGlobal ())
  ((get-global JSSATExecutionContextStack <- list-new)
   (return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; METHOD IMPLEMENTATIONS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(section
  (:6.1.6.1.14 Number::sameValue (x, y))
  ((return (:x == :y))))

(section
  (:6.1.6.2.14 BigInt::sameValue (x, y))
  ((return (:x == :y))))

(section
  (:6.2.5.1 IsAccessorDescriptor (Desc))
  (;;; 1. If Desc is undefined, return false.
   (if (is-undef :Desc)
       ((return false)))
   ;;; 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
   (if (record-absent-slot2 :Desc Get Set)
       ((return false)))
   ;;; 3. Return true.
   (return true)))

(section
  (:6.2.5.2 IsDataDescriptor (Desc))
  (;;; 1. If Desc is undefined, return false.
   (if (is-undef :Desc)
       ((return false)))
   ;;; 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
   (if (record-absent-slot2 :Desc Value Writable)
       ((return false)))
   ;;; 3. Return true.
   (return true)))

(section
  (:6.2.5.3 IsGenericDescriptor (Desc))
  (;;; 1. If Desc is undefined, return false.
   (if (is-undef :Desc)
       ((return false)))
   ;;; 2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, return true.
   (if (both (call IsAccessorDescriptor :Desc) (call IsDataDescriptor :Desc) (== false))
       ((return true)))
   ;;; 3. Return false.
   (return false)))

(section
  (:7.2.7 IsPropertyKey (argument))
  (;;; 1. If Type(argument) is String, return true.
   (if (is-string :argument)
       ((return true)))
   ;;; 2. If Type(argument) is Symbol, return true.
   (if (is-symbol :argument)
       ((return true)))
   ;;; 3. Return false.
   (return false)))

(section
  (:7.2.10 SameValue (x, y))
  (;;; 1. If Type(x) is different from Type(y), return false.
   (if (isnt-type-as :x :y)
       ((return false)))
   ;;; 2. If Type(x) is Number, then
   (if (is-number :x)
       (;;; a. Return ! Number::sameValue(x, y).
        (return (! (call Number::sameValue :x :y)))))
   ;;; 3. If Type(x) is BigInt, then
   (if (is-bigint :x)
       (;;; a. Return ! BigInt::sameValue(x, y).
        (return (! (call BigInt::sameValue :x :y)))))
   ;;; 4. Return ! SameValueNonNumeric(x, y).
   (return (! (call SameValueNonNumeric :x :y)))))

(section
  (:7.2.12 SameValueNonNumeric (x, y))
  (;;; 1. Assert: Type(x) is the same as Type(y).
   (assert (is-type-as :x :y) "Type(x) is the same as Type(y)")
   ;;; 2. If Type(x) is Undefined, return true.
   (if (is-undef :x) ((return true)))
   ;;; 3. If Type(x) is Null, return true.
   (if (is-null :x) ((return true)))
   ;;; 4. If Type(x) is String, then
   (if (is-string :x)
       ;;; a. If x and y are exactly the same sequence of code units (same length and same code units at corresponding
       ;;;    indices), return true; otherwise, return false.
       ((return (:x == :y))))
   ;;; 5. If Type(x) is Boolean, then
   (if (is-bool :x)
       (;;; a. If x and y are both true or both false, return true; otherwise, return false.
        (return (:x == :y))))
   ;;; 6. If Type(x) is Symbol, then
   (if (is-symbol :x)
       (;;; a. If x and y are both the same Symbol value, return true; otherwise, return false.
        (return (:x == :y))))
   ;;; 7. If x and y are the same Object value, return true. Otherwise, return false.
   (return (:x == :y))))

(section
  (:7.3.1 MakeBasicObject (internalSlotsList))
  (;;; 1. Let obj be a newly created object with an internal slot for each name in internalSlotsList.
   (obj = record-new)
   ;;; 2. Set obj's essential internal methods to the default ordinary object definitions specified in 10.1.
   ;;; 3. Assert: If the caller will not be overriding both obj's [[GetPrototypeOf]] and [[SetPrototypeOf]] essential internal
   ;;;    methods, then internalSlotsList contains [[Prototype]].
   ;;; 4. Assert: If the caller will not be overriding all of obj's [[SetPrototypeOf]], [[IsExtensible]], and [[PreventExtensions]]
   ;;;    essential internal methods, then internalSlotsList contains [[Extensible]].
   ;;; 5. If internalSlotsList contains [[Extensible]], set obj.[[Extensible]] to true.
   ;;; 6. Return obj.
   (return :obj)))

(section
  (:8.1.1 BoundNames (parseNode))
  (; BindingIdentifier : Identifier
   (if (match-pn :parseNode (trivial-node BindingIdentifier) 0)
       (;;; 1. Return a List whose sole element is the StringValue of Identifier.
        (return (list-new-1 (:parseNode -> JSSATParseNodeSlot1 -> JSSATParseNode_Identifier_StringValue)))))
   ; BindingIdentifier : yield
   (if (match-pn :parseNode (trivial-node BindingIdentifier) 1)
       (;;; 1. Return a List whose sole element "yield".
        (return (list-new-1 "yield"))))
   ; BindingIdentifier : await
   (if (match-pn :parseNode (trivial-node BindingIdentifier) 2)
       (;;; 1. Return a List whose sole element "await".
        (return (list-new-1 "await"))))
   (return list-new)))

(section
  (:8.1.4 LexicallyDeclaredNames (parseNode))
  ((return list-new)))

(section
  (:8.1.5 LexicallyScopedDeclarations (parseNode))
  ((return list-new)))

(section
  (:8.1.6 VarDeclaredNames (parseNode))
  ((return list-new)))

(section
  (:8.1.7 VarScopedDeclarations (parseNode))
  (; VarScopedDeclarations is defined to "Return a new empty List" for so many
   ; productions, that productions that do so are omitted.
   ;
   ; StatementList : StatementList StatementListItem
   (if (match-pn :parseNode (trivial-node StatementList) 1)
       (;;; 1. Let declarations1 be VarScopedDeclarations of StatementList.
        (declarations1 = (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let declarations2 be VarScopedDeclarations of StatementListItem.
        (declarations2 = (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot2)))
        ;;; 3. Return the list-concatenation of declarations1 and declarations2.
        (return (list-concat :declarations1 :declarations2))))
   ; VariableDeclarationList : VariableDeclaration
   (if (match-pn :parseNode (trivial-node VariableDeclarationList) 0)
       ((return (list-new-1 (:parseNode -> JSSATParseNodeSlot1)))))
   ; VariableDeclarationList : VariableDeclarationList , VariableDeclaration
   (if (is-pn VariableDeclarationList 1)
       (;;; 1. Let declarations1 be VarScopedDeclarations of VariableDeclarationList.
        (declarations1 = (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Return the list-concatenation of declarations1 and « VariableDeclaration ».
        (return (list-concat :declarations1 (list-new-1 (:parseNode -> JSSATParseNodeSlot2))))))
   ; ScriptBody : StatementList
   (if (is-pn ScriptBody 0)
       (;;; 1. Return TopLevelVarScopedDeclarations of StatementList.
        (return (call TopLevelVarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))))
   (return list-new)))

(section
  (:8.1.8 TopLevelLexicallyDeclaredNames (parseNode))
  ((return list-new)))

(section
  (:8.1.11 TopLevelVarScopedDeclarations (parseNode))
  (; StatementList : StatementList StatementListItem
   (if (is-pn StatementList 1)
       (;;; 1. Let declarations1 be TopLevelVarScopedDeclarations of StatementList.
        (declarations1 = (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let declarations2 be TopLevelVarScopedDeclarations of StatementListItem.
        (declarations2 = (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot2)))
        ;;; 3. Return the list-concatenation of declarations1 and declarations2.
        (return (list-concat :declarations1 :declarations2))))
   ; StatementListItem : Statement
   (if (is-pn StatementListItem 0)
       (;;; 1. If Statement is Statement : LabelledStatement , return TopLevelVarScopedDeclarations of Statement.
        ;;; 2. Return VarScopedDeclarations of Statement.
        (return (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))))
   (return list-new)))

(section
  (:9.1.2.3 NewObjectEnvironment (O, W, E))
  (;;; 1. Let env be a new object Environment Record.
   (env = record-new)
   ;;; 2. Set env.[[BindingObject]] to O.
   (:env BindingObject <- :O)
   ;;; 3. Set env.[[IsWithEnvironment]] to W.
   (:env IsWithEnvironment <- :W)
   ;;; 4. Set env.[[OuterEnv]] to E.
   (:env OuterEnv <- :E)
   ;;; 5. Return env.
   (return :env)))

(section
  (:9.1.2.5 NewGlobalEnvironment (G, thisValue))
  (;;; 1. Let objRec be NewObjectEnvironment(G, false, null).
   (objRec = (call NewObjectEnvironment :G false null))
   ;;; 2. Let dclRec be a new declarative Environment Record containing no bindings.
   (dclRec = record-new)
   ;;; 3. Let env be a new global Environment Record.
   (env = record-new)
   ;;; 4. Set env.[[ObjectRecord]] to objRec.
   (:env ObjectRecord <- :objRec)
   ;;; 5. Set env.[[GlobalThisValue]] to thisValue.
   (:env GlobalThisValue <- :thisValue)
   ;;; 6. Set env.[[DeclarativeRecord]] to dclRec.
   (:env DeclarativeRecord <- :dclRec)
   ;;; 7. Set env.[[VarNames]] to a new empty List.
   (:env VarNames <- list-new)
   ;;; 8. Set env.[[OuterEnv]] to null.
   (:env OuterEnv <- null)
   ;;; 9. Return env.
   (return :env)))

(section
  (:9.3.1 CreateRealm ())
  (;;; 1. Let realmRec be a new Realm Record.
   (realmRec = record-new)
   ;;; 2. Perform CreateIntrinsics(realmRec).
   (call CreateIntrinsics :realmRec)
   ;;; 3. Set realmRec.[[GlobalObject]] to undefined.
   (:realmRec GlobalObject <- undefined)
   ;;; 4. Set realmRec.[[GlobalEnv]] to undefined.
   (:realmRec GlobalEnv <- undefined)
   ;;; 5. Set realmRec.[[TemplateMap]] to a new empty List.
   (:realmRec TemplateMap <- list-new)
   ;;; 6. Return realmRec.
   (return :realmRec)))

(section
  (:9.3.2 CreateIntrinsics (realmRec))
  (;;; 1. Let intrinsics be a new Record.
   (intrinsics = record-new)
   ;;; 2. Set realmRec.[[Intrinsics]] to intrinsics.
   (:realmRec Intrinsics <- :intrinsics)
   ;;; 3. Set fields of intrinsics with the values listed in Table 8. The field names are the names listed in column one
   ;;;    of the table. The value of each field is a new object value fully and recursively populated with property values
   ;;;    as defined by the specification of each object in clauses 19 through 28. All object property values are newly
   ;;;    created object values. All values that are built-in function objects are created by performing CreateBuiltinFunction(steps, length, name, slots, realmRec, prototype) where steps is the definition of that function provided by this specification, name is the initial value of the function's name property, length is the initial value of the function's length property, slots is a list of the names, if any, of the function's specified internal slots, and prototype is the specified value of the function's [[Prototype]] internal slot. The creation of the intrinsics and their properties must be ordered to avoid any dependencies upon objects that have not yet been created.
   ;;; 4. Perform AddRestrictedFunctionProperties(intrinsics.[[%Function.prototype%]], realmRec).
   ;;; 5. Return intrinsics.
   (return :intrinsics)))

(section
  (:9.3.3 SetRealmGlobalObject (realmRec, globalObj, thisValue))
  (;;; 1. If globalObj is undefined, then
   (globalObj =
              (expr-block
               ((if (is-undef :globalObj)
                    (;;; a. Let intrinsics be realmRec.[[Intrinsics]].
                     (intrinsics = (:realmRec -> Intrinsics))
                     ;;; b. Set globalObj to ! OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]]).
                     (! (call OrdinaryObjectCreate record-new list-new)))
                    ((:globalObj))))))
   ;;; 2. Assert: Type(globalObj) is Object.
   (assert (is-object :globalObj) "Type(globalObj) is Object")
   ;;; 3. If thisValue is undefined, set thisValue to globalObj.
   (thisValue =
              (expr-block
               ((if (is-undef :thisValue)
                    ((:globalObj))
                    ((:thisValue))))))
   ;;; 4. Set realmRec.[[GlobalObject]] to globalObj.
   (:realmRec GlobalObject <- :globalObj)
   ;;; 5. Let newGlobalEnv be NewGlobalEnvironment(globalObj, thisValue).
   (newGlobalEnv = (call NewGlobalEnvironment :globalObj :thisValue))
   ;;; 6. Set realmRec.[[GlobalEnv]] to newGlobalEnv.
   (:realmRec GlobalEnv <- :newGlobalEnv)
   ;;; 7. Return realmRec.
   (return :realmRec)))

(section
  (:9.3.4 SetDefaultGlobalBindings (realmRec))
  (;;; 1. Let global be realmRec.[[GlobalObject]].
   (global = (:realmRec -> GlobalObject))
   ;;; 2. For each property of the Global Object specified in clause 19, do
   ;;; a. Let name be the String value of the property name.
   ;;; b. Let desc be the fully populated data Property Descriptor for the property, containing the specified attributes for the property. For properties listed in 19.2, 19.3, or 19.4 the value of the [[Value]] attribute is the corresponding intrinsic object from realmRec.
   ;;; c. Perform ? DefinePropertyOrThrow(global, name, desc).
   ;;; 3. Return global.
   (return :global)))

(section
  (:9.5 InitializeHostDefinedRealm ())
  (;;; 1. Let realm be CreateRealm().
   (realm = (call CreateRealm))
   ;;; 2. Let newContext be a new execution context.
   (newContext = record-new)
   ;;; 3. Set the Function of newContext to null.
   (:newContext Function <- null)
   ;;; 4. Set the Realm of newContext to realm.
   (:newContext Realm <- :realm)
   ;;; 5. Set the ScriptOrModule of newContext to null.
   (:newContext ScriptOrModule <- null)
   ;;; 6. Push newContext onto the execution context stack; newContext is now the running execution context.
   (exec-ctx-stack-push :newContext)
   ;;; 7. If the host requires use of an exotic object to serve as realm's global object, let global be such an object created
   ;;;    in a host-defined manner. Otherwise, let global be undefined, indicating that an ordinary object should be created
   ;;;    as the global object.
   (global = undefined)
   ;;; 8. If the host requires that the this binding in realm's global scope return an object other than the global object,
   ;;;    let thisValue be such an object created in a host-defined manner. Otherwise, let thisValue be undefined, indicating
   ;;;    that realm's global this binding should be the global object.
   (thisValue = undefined)
   ;;; 9. Perform SetRealmGlobalObject(realm, global, thisValue).
   (call SetRealmGlobalObject :realm :global :thisValue)
   ;;; 10. Let globalObj be ? SetDefaultGlobalBindings(realm).
   (globalObj = (? (call SetDefaultGlobalBindings :realm)))
   ;;; 11. Create any host-defined global object properties on globalObj.
   ;;; 12. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))

(section
  (:10.1.6.3 ValidateAndApplyPropertyDescriptor (O, P, extensible, Desc, current))
  (;;; 1. Assert: If O is not undefined, then IsPropertyKey(P) is true
   (if (isnt-undef :O)
       ((assert ((call IsPropertyKey :P) == true) "If O is not undefined, then IsPropertyKey(P) is true")))
   ;;; 2. If current is undefined, then
   (if (is-undef :current)
       (;;; a. If extensible is false, return false
        (if (:extensible == false)
            ((return false)))
        ;;; b. Assert: extensible is true.
        (assert (:extensible == true) "extensible is true")
        ;;; c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
        (if (either (call IsGenericDescriptor :Desc) (call IsDataDescriptor :Desc) (== true))
            (;;; i. If O is not undefined, create an own data property named P of object O whose [[Value]],
             ;;;[[Writable]], [[Enumerable]], and [[Configurable]] attribute values are described by Desc. If the
             ;;;value of an attribute field of Desc is absent, the attribute of the newly created property is set to its
             ;;;default value.
             (if (isnt-undef :O)
                 ((p-desc = record-new)
                  (record-copy-slot-or-default :Desc :p-desc Value undefined)
                  (record-copy-slot-or-default :Desc :p-desc Writable undefined)
                  (record-copy-slot-or-default :Desc :p-desc Enumerable undefined)
                  (record-copy-slot-or-default :Desc :p-desc Configurable undefined)
                  (record-set-prop :O :P :p-desc))
                 ;;; d. Else,
                 (;; i. Assert: ! IsAccessorDescriptor(Desc) is true.
                  (assert (! (call IsAccessorDescriptor :Desc)) "! IsAccessorDescriptor(Desc) is true")
                  ;; ii. If O is not undefined, create an own accessor property named P of object O 
                  ;;     whose [[Get]], [[Set]], [[Enumerable]], and [[Configurable]] attribute values are described by
                  ;;     Desc. If the value of an attribute field of Desc is absent, the attribute of the newly created
                  ;;     property is set to its default value.
                  (if (isnt-undef :O)
                      ((p-desc = record-new)
                       (record-copy-slot-or-default :Desc :p-desc Get undefined)
                       (record-copy-slot-or-default :Desc :p-desc Set undefined)
                       (record-copy-slot-or-default :Desc :p-desc Enumerable undefined)
                       (record-copy-slot-or-default :Desc :p-desc Configurable undefined)
                       (record-set-prop :O :P :p-desc)))))
             ;;; e. Return true.
             (return true)))))
   ;;; 3. If every field in Desc is absent, return true.
   (if (record-absent-slot6 :Desc Value Writable Get Set Enumerable Configurable)
       ((return true)))
   ;;; 4. If current.[[Configurable]] is false, then
   (if (is-false (:current -> Configurable))
       (;;; a. If Desc.[[Configurable]] is present and its value is true, return false.
        (record-do-slot value :Desc Configurable
                        (if (is-true :value) ((return false))))
        ;;; b. If Desc.[[Enumerable]] is present and ! SameValue(Desc.[[Enumerable]], current.[[Enumerable]]) is false, return false.
        (if (record-has-slot :Desc Enumerable)
            ((if (is-false (! (call SameValue (:Desc -> Enumerable) (:current -> Enumerable))))
                 ((return false)))))))
   ;;; 5. If ! IsGenericDescriptor(Desc) is true, then
   (if-elif3-else
    ((is-true (! (call IsGenericDescriptor :Desc)))
     (;;; a. NOTE: No further validation is required.
     ))
    ;;; 6. Else if ! SameValue(! IsDataDescriptor(current), ! IsDataDescriptor(Desc)) is false, then
    ((is-false (! (call SameValue (! (call IsDataDescriptor :current)) (! (call IsDataDescriptor :Desc)))))
     (;;; a. If current.[[Configurable]] is false, return false.
      (if (is-false (:current -> Configurable)) ((return false)))
      ;;; b. If IsDataDescriptor(current) is true, then
      (if (is-true (call IsDataDescriptor :current))
          (;;; i. If O is not undefined, convert the property named P of object O from a data property to an
           ;;;    accessor property. Preserve the existing values of the converted property's [[Configurable]]
           ;;;    and [[Enumerable]] attributes and set the rest of the property's attributes to their default
           ;;;    values.
           (if (isnt-undef :O)
               ((P = (:O => :P))
                (:P Get <- undefined)
                (:P Set <- undefined)
                (:P Value <-)
                (:P Writable <-))))
          ;;; c. Else,
          (;;; i. If O is not undefined, convert the property named P of object O from an accessor property to
           ;;;    a data property. Preserve the existing values of the converted property's [[Configurable]]
           ;;;    and [[Enumerable]] attributes and set the rest of the property's attributes to their default
           ;;;    values.
           (if (isnt-undef :O)
               ((P = (:O => :P))
                (:P Value <- undefined)
                (:P Writable <- undefined)
                (:P Get <-)
                (:P Set <-)))))))
    ;;; 7. Else if IsDataDescriptor(current) and IsDataDescriptor(Desc) are both true, then
    ((both (call IsDataDescriptor :current) (call IsDataDescriptor :Desc) is-true)
     (;;; a. If current.[[Configurable]] is false and current.[[Writable]] is false, then
      (if (both (:current -> Configurable) (:current -> Writable) is-false)
          (;;; i. If Desc.[[Writable]] is present and Desc.[[Writable]] is true, return false.
           (record-do-slot writable :Desc Writable (if (is-true :writable) ((return true))))
           ;;; ii. If Desc.[[Value]] is present and SameValue(Desc.[[Value]], current.[[Value]]) is false, return false.
           (if (record-has-slot :Desc Value)
               ((if (is-false (call SameValue (:Desc -> Value) (:current -> Value)))
                    ((return false)))))
           ;;; iii. Return true.
           (return true)))))
    ;;;  8. Else,
    (;;; a. Assert: ! IsAccessorDescriptor(current) and ! IsAccessorDescriptor(Desc) are both true.
     (assert
      (both (! (call IsAccessorDescriptor :current)) (! (call IsAccessorDescriptor :Desc)) is-true)
      "! IsAccessorDescriptor(current) and ! IsAccessorDescriptor(Desc) are both true")
     ;;; b. If current.[[Configurable]] is false, then
     (if (is-false (:current -> Configurable))
         (;;; i. If Desc.[[Set]] is present and SameValue(Desc.[[Set]], current.[[Set]]) is false, return false.
          (if ((record-has-slot :Desc Set))
              ((if (is-false (call SameValue (:Desc -> Set) (:current -> Set)))
                   ((return false)))))
          ;;; ii. If Desc.[[Get]] is present and SameValue(Desc.[[Get]], current.[[Get]]) is false, return false.
          (if ((record-has-slot :Desc Get))
              ((if (is-false (call SameValue (:Desc -> Get) (:current -> Get)))
                   ((return false)))))
          ;;; iii. Return true.
          (return true)))))
   ;;; 9. If O is not undefined, then
   (if (isnt-undef :O)
       (;;; a. For each field of Desc that is present, set the corresponding attribute of the property named P of object O
        ;;;    to the value of the field.
        (P = (:O => :P))
        (record-copy-slot-if-present :Desc :P Value)
        (record-copy-slot-if-present :Desc :P Writable)
        (record-copy-slot-if-present :Desc :P Get)
        (record-copy-slot-if-present :Desc :P Set)
        (record-copy-slot-if-present :Desc :P Configurable)
        (record-copy-slot-if-present :Desc :P Enumerable)))
   ;;; 10. Return true
   (return true)))

(section
  (:10.1.12 OrdinaryObjectCreate (proto, additionalInternalSlotsList))
  (;;; 1. Let internalSlotsList be « [[Prototype]], [[Extensible]] ».
   (internalSlotsList = (list-new-2 (trivial-slot Prototype) (trivial-slot Extensible)))
   ;;; 2. If additionalInternalSlotsList is present, append each of its elements to internalSlotsList.
   (for :additionalInternalSlotsList
        ((list-push :internalSlotsList for-item)))
   ;;; 3. Let O be ! MakeBasicObject(internalSlotsList).
   (O = (! (call MakeBasicObject :internalSlotsList)))
   ;;; 4. Set O.[[Prototype]] to proto.
   (:O Prototype <- :proto)
   ;;; 5. Return O.
   (return :O)))

(section
  (:16.1.6 ScriptEvaluation (scriptRecord))
  (;;; 1. Let globalEnv be scriptRecord.[[Realm]].[[GlobalEnv]].
   (globalEnv = ((:scriptRecord -> Realm) -> GlobalEnv))
   ;;; 2. Let scriptContext be a new ECMAScript code execution context.
   (scriptContext = record-new)
   ;;; 3. Set the Function of scriptContext to null.
   (:scriptContext Function <- null)
   ;;; 4. Set the Realm of scriptContext to scriptRecord.[[Realm]].
   (:scriptContext Realm <- (:scriptRecord -> Realm))
   ;;; 5. Set the ScriptOrModule of scriptContext to scriptRecord.
   (:scriptContext ScriptOrModule <- :scriptRecord)
   ;;; 6. Set the VariableEnvironment of scriptContext to globalEnv.
   (:scriptContext VariableEnvironment <- :globalEnv)
   ;;; 7. Set the LexicalEnvironment of scriptContext to globalEnv.
   (:scriptContext LexicalEnvironment <- :globalEnv)
   ;;; 8. Suspend the currently running execution context.
   ; (todo)
   ;;; 9. Push scriptContext onto the execution context stack; scriptContext is now the running execution context.
   (exec-ctx-stack-push :scriptContext)
   ;;; 10. Let scriptBody be scriptRecord.[[ECMAScriptCode]].
   (scriptBody = (:scriptRecord -> ECMAScriptCode))
   ;;; 11. Let result be GlobalDeclarationInstantiation(scriptBody, globalEnv).
   (result = (call GlobalDeclarationInstantiation :scriptBody :globalEnv))
   ;;; 12. If result.[[Type]] is normal, then
   (result = (if (is-normal (:result -> Type))
                 (;;; a. Set result to the result of evaluating scriptBody.
                  (evaluating :scriptBody))
                 (:result)))
   ;;; 13. If result.[[Type]] is normal and result.[[Value]] is empty, then
   (result = (if ((is-normal (:result -> Type)) and (is-empty (:result -> Value)))
                 (;;; a. Set result to NormalCompletion(undefined).
                  (NormalCompletion undefined))
                 (:result)))
   ;;; 14. Suspend scriptContext and remove it from the execution context stack.
   ; (todo)
   ;;; 15. Assert: The execution context stack is not empty.
   (assert (0 != (list-len (get-global -> JSSATExecutionContextStack))) "The execution context stack is not empty.")
   ;;; 16. Resume the context that is now on the top of the execution context stack as the running execution context.
   ; (todo)
   ;;; 17. Return Completion(result).
   (return :result)))

(section
  (:16.1.5 ParseScript (sourceText, realm, hostDefined, body))
  (;;; 1. Let body be ParseText(sourceText, Script).
   ; we have already parsed the script
   ;;; 2. If body is a List of errors, return body.
   ;;; 3. Return Script Record { [[Realm]]: realm, [[ECMAScriptCode]]: body, [[HostDefined]]: hostDefined }.
   (scriptRecord = record-new)
   (:scriptRecord Realm <- :realm)
   (:scriptRecord ECMAScriptCode <- :body)
   (:scriptRecord HostDefined <- :hostDefined)
   (return :scriptRecord)))

(section
  (:16.1.7 GlobalDeclarationInstantiation (script, env))
  (;;; 1. Assert: env is a global Environment Record.
   ; (todo)
   ;;; 2. Let lexNames be the LexicallyDeclaredNames of script.
   (lexNames = (call LexicallyDeclaredNames :script))
   ;;; 3. Let varNames be the VarDeclaredNames of script.
   (varNames = (call VarDeclaredNames :script))
   ;;; 4. For each element name of lexNames, do
   (for :varNames
        ((name = for-item)
         ;;; a. If env.HasVarDeclaration(name) is true, throw a SyntaxError exception.
         (if (:env .. HasVarDeclaration :name)
             ((throw (SyntaxError :env "env.HasVarDeclaration(name) is true"))))
         ;;; b. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
         (if (:env .. HasLexicalDeclaration :name)
             ((throw (SyntaxError :env "env.HasLexicalDeclaration(name) is true"))))
         ;;; c. Let hasRestrictedGlobal be ? env.HasRestrictedGlobalProperty(name).
         (hasRestrictedGlobal = (? (:env .. HasRestrictedGlobalProperty :name)))
         ;;; d. If hasRestrictedGlobal is true, throw a SyntaxError exception.
         (if (is-true :hasRestrictedGlobal)
             ((throw (SyntaxError :env "If hasRestrictedGlobal is true"))))))
   ;;; 5. For each element name of varNames, do
   (for :varNames
        ((name = for-item)
         ;;; a. If env.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
         (hasLexicalDeclaration = (:env .. HasLexicalDeclaration :name))
         (if (is-true :hasLexicalDeclaration)
             ((throw (SyntaxError :env "If env.HasLexicalDeclaration(name) is true"))))))
   ;;; 6. Let varDeclarations be the VarScopedDeclarations of script.
   (varDeclarations = (call VarScopedDeclarations :script))
   ;;; 7. Let functionsToInitialize be a new empty List.
   (functionsToInitialize = list-new)
   ;;; 8. Let declaredFunctionNames be a new empty List.
   (declaredFunctionNames = list-new)
   ;;; 9. For each element d of varDeclarations, in reverse List order, do
   (for :varDeclarations
        ((d = for-item-rev)
         ;;; a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
         ;;; i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
         ;;; ii. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
         ;;; iii. Let fn be the sole element of the BoundNames of d.
         (fn = (sole-element (call BoundNames :d)))
         ;;; iv. If fn is not an element of declaredFunctionNames, then
         ;;; 1. Let fnDefinable be ? env.CanDeclareGlobalFunction(fn).
         ;;; 2. If fnDefinable is false, throw a TypeError exception.
         ;;; 3. Append fn to declaredFunctionNames.
         ;;; 4. Insert d as the first element of functionsToInitialize.
        ))
   ;;; 10. Let declaredVarNames be a new empty List.
   (declaredVarNames = list-new)
   ;;; 11. For each element d of varDeclarations, do
   (for :varDeclarations
        ((d = for-item)
         ;;; a. If d is a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
         ;;; i. For each String vn of the BoundNames of d, do
         (boundNamesOfD = (call BoundNames :d))
         (for :boundNamesOfD
              ((vn = for-item)
               ;;; 1. If vn is not an element of declaredFunctionNames, then
               ;;; a. Let vnDefinable be ? env.CanDeclareGlobalVar(vn).
               ;;; b. If vnDefinable is false, throw a TypeError exception.
               ;;; c. If vn is not an element of declaredVarNames, then
               ;;; i. Append vn to declaredVarNames.
              ))))
   ;;; 12. NOTE: No abnormal terminations occur after this algorithm step if the global object is an ordinary object.However, if the global object is a Proxy exotic object it may exhibit behaviours that cause abnormalterminations in some of the following steps.
   ;;; 13. NOTE: Annex B.3.3.2 adds additional steps at this point.
   ;;; 14. Let lexDeclarations be the LexicallyScopedDeclarations of script.
   (lexDeclarations = (call LexicallyScopedDeclarations :script))
   ;;; 15. For each element d of lexDeclarations, do
   (for :lexDeclarations
        ((d = for-item)
         ;;; a. NOTE: Lexically declared names are only instantiated here but not initialized.
         ;;; b. For each element dn of the BoundNames of d, do
         (boundNamesOfD = (call BoundNames :d))
         (for :boundNamesOfD
              ((dn = for-item)
               ;;; i. If IsConstantDeclaration of d is true, then
               ;;; 1. Perform ? env.CreateImmutableBinding(dn, true).
               ;;; ii. Else,
               ;;; 1. Perform ? env.CreateMutableBinding(dn, false).
              ))))
   ;;; 16. For each Parse Node f of functionsToInitialize, do
   (for :functionsToInitialize
        ((f = for-item)
         ;;; a. Let fn be the sole element of the BoundNames of f.
         (fn = (sole-element (call BoundNames :f)))
         ;;; b. Let fo be InstantiateFunctionObject of f with argument env.
         ;;; c. Perform ? env.CreateGlobalFunctionBinding(fn, fo, false).
        ))
   ;;; 17. For each String vn of declaredVarNames, do
   (for :declaredVarNames
        ((vn = for-item)
         ;;; a. Perform ? env.CreateGlobalVarBinding(vn, false).
        ))
   ;;; 18. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))