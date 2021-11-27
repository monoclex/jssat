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

; OPTIMIZATION:
; `?` will potentially unwrap the completion record, so we should box it back into a
; completion record with (NormalCompletion ...)
;
; but that's super wasteful! we could just return the operation itself
(def (NormalCompletion (? :x)) :x)

(def (and :a :b) (:a and :b))
(def (or :a :b) (:a or :b))
(def (and3 :a :b :c) (and (and :a :b) :c))
(def (and4 :a :b :c :d) (and3 :a :b (and :c :d)))
(def (and6 :1 :2 :3 :4 :5 :6) (and (and3 :1 :2 :3) (and3 :4 :5 :6)))
(def (or3 :1 :2 :3) (or (or :1 :2) :3))
(def (or4 :1 :2 :3 :4) ((:1 or :2) or (:3 or :4)))
(def (or7 :1 :2 :3 :4 :5 :6 :7) (or3 (or3 :1 :2 :3) (or3 :4 :5 :6) :7))
(def (both :a :b (:x :y)) (and (:a :x :y) (:b :x :y)))
(def (both :1 :2 :f) (and (:f :1) (:f :2)))
(def (either :a :b (:x :y)) (or (:a :x :y) (:b :x :y)))
(def (todo) (assert false "TODO"))
(def (:a - :b) (:a + (not :b)))
(def (throw :x) (return (ThrowCompletion :x)))
(def (ret-comp :x) (return (NormalCompletion :x)))

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

; TODO: implement a real list pop
(def (exec-ctx-stack-pop) (get-global JSSATExecutionContextStack <- list-new))
(def exec-ctx-stack (get-global -> JSSATExecutionContextStack))
(def (exec-ctx-stack-push :x) (list-push exec-ctx-stack :x))
(def exec-ctx-stack-size (list-len exec-ctx-stack))
(def curr-exec-ctx (list-get exec-ctx-stack (list-end exec-ctx-stack)))
(def current-realm (curr-exec-ctx -> Realm))

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
  (list-set :list (math-max (list-end :list) 0) :x))

(def
  (list-pop :list)
  (expr-block
   ((it = (list-get :list (list-end :list)))
    (list-del :list (list-end :list))
    (:it))))

(def (list-end :list) ((list-len :list) - 1))

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

(def
  (lazyOr :1 :2)
  (expr-block
   ((if :1
        ((true))
        ((if :2
             ((true))
             ((false))))))))

(def
  (lazyAnd :1 :2)
  (expr-block
   ((if :1
        ((if :2
             ((true))
             ((false))))
        (false)))))

; only `not x`, `x == y`, and `x < y` are implemented. create the other operators here
(def (:x != :y) (not (:x == :y)))
(def (:x <= :y) ((:x == :y) or (:x < :y)))
(def (:x > :y) (:y < :x))
(def (:x >= :y) (:y <= :x))

(def String Bytes)
(def BigInt BigNumber)

(def null (atom Null))
(def undefined (atom Undefined))
(def normal (atom Normal))
(def empty (atom Empty))
(def sync (atom Sync))
(def unresolvable (atom Unresolvable))
(def non-lexical-this (atom NonLexicalThis))
(def lexical-this (atom LexicalThis))
(def lexical (atom Lexical))
(def initialized (atom Initialized))
(def uninitialized (atom Uninitialized))
(def atom-strict (atom Strict))
(def atom-global (atom Global))
(def atom-return (atom Return))
(def atom-base (atom Base))
(def atom-throw (atom Throw))
(def (atom throw) (atom Throw))

(def (ifAtom :x :y) (lazyAnd (is-type-of Atom :x) :y))

(def (is-undef :x) (ifAtom :x (:x == undefined)))
(def (isnt-undef :x) (not (is-undef :x)))
(def (is-null :x) (ifAtom :x (:x == null)))
(def (isnt-null :x) (not (is-null :x)))
(def (is-true :x) (:x == true))
(def (is-false :x) (:x == false))
(def (is-normal :x) (:x == normal))
(def (is-unresolvable :x) (:x == unresolvable))
(def (is-empty :x) (:x == empty))
(def (isnt-empty :x) (not (is-empty :x)))
(def (is-string :x) (is-type-of String :x))
; (def (is-symbol :x) (is-type-of Symbol :x))
(def (is-symbol :x) (false))
(def (is-number :x) (is-type-of Number :x))
(def (is-bigint :x) (is-type-of BigInt :x))
(def (is-bool :x) (is-type-of Boolean :x))
(def (is-record :x) (is-type-of Record :x))
(def (isnt-record :x) (not (is-record :x)))
(def (is-object :x) (is-record :x))
(def (isnt-object :x) (not (is-object :x)))

(def (pn-kind-is :parseNode :kind) (:parseNode -> JSSATParseNodeKind == (atom :kind)))
(def (pn-kind-isnt :parseNode :kind) (not (pn-kind-is :parseNode :kind)))
(def (pn-variant-is :parseNode :variant_idx) (:parseNode -> JSSATParseNodeVariant == :variant_idx))
(def (pn-variant-isnt :parseNode :variant_idx) (not (pn-variant-is :parseNode :variant_idx)))

(def
  (match-pn :parseNode (atom :kind) :variant_idx)
  (and (pn-kind-is :parseNode :kind) (pn-variant-is :parseNode :variant_idx)))

(def (match-pn :parseNode :kind :variant_idx) (match-pn :parseNode (atom :kind) :variant_idx))

(def
  (is-pn :kind :variant_idx)
  (match-pn :parseNode :kind :variant_idx))

(def (isnt-type-as :x :y) (not (is-type-as :x :y)))

(def
  (math-max :x :y)
  (expr-block
   ((if (:x > :y)
        ((:x))
        ((:y))))))

(def (:1 = :2 -> :3) (:1 = (:2 -> :3)))
(def (:1 = :2 => :3) (:1 = (:2 => :3)))
(def (:1 -> :2) (record-get-slot :1 :2))
(def (:1 -> :2 == :3) ((:1 -> :2) == :3))
(def (:1 -> :2 -> :3) ((:1 -> :2) -> :3))
(def (:1 => :2) (record-get-prop :1 :2))
(def (:1 => :2 -> :3) ((:1 => :2) -> :3))
(def (:record :slot <- :expr) (record-set-slot :record :slot :expr))
(def (:record :slot <-) (record-del-slot :record :slot))
(def (record-absent-slot :record :slot) (not (record-has-slot :record :slot)))
(def (record-absent-prop :record :prop) (not (record-has-prop :record :prop)))

(def
  (record-do-slot :bind :record :slot :action)
  (if (record-has-slot :record :slot)
      ((:bind = (record-get-slot :record :slot))
       :action)))

(def (record-copy-slot :src :dest :slot) (:dest :slot <- (:src -> :slot)))

(def
  (record-copy-slot-or-default :src :dest :slot :default)
  (if (record-absent-slot :src :slot)
      ((record-set-slot :dest :slot :default))
      ((record-copy-slot :src :dest :slot))))

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

; TODO: more stuff ig
(def (isnt-reference-record :x) (not (is-reference-record :x)))
(def
  (is-reference-record :x)
  (expr-block
   ((if (is-record :x)
        ((record-has-slot :x Base))
        (false)))))

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

; TODO: implement these as an intrinsic for more performance
(def (list-contains :list :element) (call JSSATListContains :list :element))
(section
  (:0.0.0.0 JSSATListContains (list, element))
  ((for :list
        ((i = for-item)
         (if (:i == :element)
             ((return true)))))
   (return false)))

(def (list-insert-front :list :element) (call JSSATListInsertFront :list :element))
(section
  (:0.0.0.0 JSSATListInsertFront (list, element))
  (; list looks like [1, 2, ..., n]
   ; 1. reverse list
   ;    [n, ..., 2, 1]
   (list-reverse :list)
   ; 1. push `element` onto list
   ;    [n, ..., 2, 1, element]
   (list-push :list :element)
   ; 2. reverse list
   ;    [element, 1, 2, ..., n]
   (list-reverse :list)
   (return)))

(def (list-reverse :list) (call JSSATListReverse :list))
(section
  (:0.0.0.0 JSSATListReverse (list))
  (; list : [1, 2, ..., n]
   ; tmp  : []
   (tmp = list-new)
   ; 1. copy list to tmp
   ; list : [1, 2, ..., n]
   ; tmp  : [1, 2, ..., n]
   (for :list
        ((list-push :tmp for-item)))
   ; 2. iterate tmp in reverse, copy to list
   (for :tmp
        ((i = for-item-rev)
         (target-i = :jssat_i)
         (list-set :list :target-i :i)))
   (return)))

(def (list-has-duplicates :list) (call JSSATListHasDuplicates :list))
(section
  (:0.0.0.0 JSSATListHasDuplicates (list))
  ((for :list
        ((iIdx = :jssat_i)
         (i = for-item)
         (for :list
              ((jIdx = :jssat_i)
               (j = for-item)
               (if (:iIdx != :jIdx)
                   ((if (:i == :j)
                        ((return true)))))))))
   (return false)))

(def (list-clone :list) (call JSSATListClone :list))
(section
  (:0.0.0.0 JSSATListClone (list))
  ((new = list-new)
   (for :list
        ((list-push :new for-item)))
   (return :new)))

; TODO: use some kind of `Kind`/`Type` key to identify it
;       for now we just try to check if one of the virtual methods exists
(def (is-environment-record :x) (record-has-slot :x GetBindingValue))

; TODO: somehow use `env` to load the `SyntaxError` object and construct it
(def (SyntaxError :env :msg) (:msg))
; same for TypeError (:env can be gotten via curr-exec-ctx)
(def (TypeError :msg) (:msg))
(def (ReferenceError :msg) (:msg))

; "Let <thing> be the sole element of <list>"
(def
  (sole-element :x)
  ((let
     sole_element_jssat_list
     =
     :x
     in
     (; assert that the list is a list with a singular element
      (assert ((list-len :sole_element_jssat_list) == 1) "to get the 'sole element' of a list, it must be a singleton list")
      (assert (list-has :sole_element_jssat_list 0) "sanity check")
      (list-get :sole_element_jssat_list 0)))))

; 5.2.3.4 ReturnIfAbrupt Shorthands
(def
  (! :OperationName)
  (expr-block
   (;;; 1. Let val be OperationName().
    (val = :OperationName)
    ; if we're not dealing with an object, it's already unwrapped
    (if (isnt-record :val)
        ((:val))
        (;;; 2. Assert: val is never an abrupt completion.
         (assert (isnt-abrupt-completion :val) "val is never an abrupt completion")
         ;;; 3. If val is a Completion Record, set val to val.[[Value]].
         (if (is-completion-record :val)
             ((record-get-slot :val Value))
             (:val)))))))

(def (Perform ! :x)
  (if (is-record :x)
      (;;; 2. Assert: val is never an abrupt completion.
       (assert (isnt-abrupt-completion :x) "val is never an abrupt completion"))))

; 5.2.3.4 ReturnIfAbrupt Shorthands
(def
  (? :x)
  (expr-block
   ((jssat_arg = :x)
    (; if we're not dealing with an object, it's already unwrapped
     (if (isnt-record :jssat_arg)
         ((:jssat_arg))
         (;;; 1. If argument is an abrupt completion, return argument.
          (if (is-abrupt-completion :jssat_arg)
              ((return :jssat_arg)
               (unreachable))
              ;;; 2. Else if argument is a Completion Record, set argument to argument.[[Value]].
              (elif (is-completion-record :jssat_arg)
                    ((record-get-slot :jssat_arg Value))
                    (:jssat_arg)))))))))

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
    (:jssat_throw_completion Type <- (atom throw))
    (:jssat_throw_completion Value <- :x)
    (:jssat_throw_completion Target <- empty)
    (:jssat_throw_completion))))

; "new declarative environment record"
(def new-declarative-environment-record
  (expr-block
   ((rec = record-new)
    (:rec JSSATHasBinding <- (get-fn-ptr DeclarativeEnvironmentRecord_HasBinding))
    (:rec CreateMutableBinding <- (get-fn-ptr DeclarativeEnvironmentRecord_CreateMutableBinding))
    (:rec CreateImmutableBinding <- (get-fn-ptr DeclarativeEnvironmentRecord_CreateImmutableBinding))
    (:rec InitializeBinding <- (get-fn-ptr DeclarativeEnvironmentRecord_InitializeBinding))
    (:rec SetMutableBinding <- (get-fn-ptr DeclarativeEnvironmentRecord_SetMutableBinding))
    (:rec GetBindingValue <- (get-fn-ptr DeclarativeEnvironmentRecord_GetBindingValue))
    (:rec WithBaseObject <- (get-fn-ptr DeclarativeEnvironmentRecord_WithBaseObject))
    (:rec))))

;;;;;;;
; virt calls
;;;;;;;

(def (virt0 :actor :slot) (call-virt (:actor -> :slot) :actor))
(def (virt1 :actor :slot :1) (call-virt (:actor -> :slot) :actor :1))
(def (virt2 :actor :slot :1 :2) (call-virt (:actor -> :slot) :actor :1 :2))
(def (virt3 :actor :slot :1 :2 :3) (call-virt (:actor -> :slot) :actor :1 :2 :3))

; we use `..` instead of `.` because `.` is a cons cell :v
(def (:env .. HasVarDeclaration :N) (call-virt (:env -> JSSATHasVarDeclaration) :env :N))
(def (:env .. HasLexicalDeclaration :N) (call-virt (:env -> JSSATHasLexicalDeclaration) :env :N))
(def (:env .. HasRestrictedGlobalProperty :N) (call-virt (:env -> JSSATHasRestrictedGlobalProperty) :env :N))

(def (:env .. HasBinding :N) (call-virt (:env -> JSSATHasBinding) :env :N))
(def (:env .. CreateMutableBinding :N :D) (virt2 :env CreateMutableBinding :N :D))
(def (:env .. CreateImmutableBinding :N :D) (virt2 :env CreateImmutableBinding :N :D))
(def (:env .. InitializeBinding :N :D) (virt2 :env InitializeBinding :N :D))
(def (:env .. SetMutableBinding :N :D :S) (virt3 :env SetMutableBinding :N :D :S))

(def (:env .. CanDeclareGlobalVar :N) (call CanDeclareGlobalVar :env :N))
(def (:env .. CanDeclareGlobalFunction :N) (call CanDeclareGlobalFunction :env :N))
(def (:env .. CreateGlobalFunctionBinding :N :V :D) (call CreateGlobalFunctionBinding :env :N :V :D))

(def (:env .. WithBaseObject) (virt0 :env WithBaseObject))
(def (:O .. GetBindingValue :1 :2) (virt2 :O GetBindingValue :1 :2))

; TODO: once we have all of these defined we should then replace them all with the single rule
; (def (:O .. :slot :P) (virt1 :O :slot :P))
; but for now we have each of these listed explicitly so we know how much we've done
(def (:O .. GetOwnProperty :P) (call-virt (:O -> GetOwnProperty) :O :P))
(def (:O .. GetPrototypeOf) (call-virt (:O -> GetPrototypeOf) :O))
(def (:O .. HasOwnProperty :P) (call-virt (:O -> HasOwnProperty) :O :P))
(def (:O .. HasProperty :P) (call-virt (:O -> HasProperty) :O :P))
(def (:O .. DefineOwnProperty :P :Desc) (call-virt (:O -> DefineOwnProperty) :O :P :Desc))
(def (:O .. IsExtensible) (virt0 :O IsExtensible))

(def (:O .. Get :1 :2) (virt2 :O Get :1 :2))
(def (:O .. Set :1 :2 :3) (virt3 :O Set :1 :2 :3))

(def (:func .. Call :thisValue :argumentList) (virt2 :func Call :thisValue :argumentList))

(def (evaluating :x) (chainEval :x))
(def (chainEval :x) (call-virt (:x -> JSSATParseNodeEvaluate) :x))

; Table 34

(def (inject-table-34 :list)
  (_dontCare =
             (expr-block
              ((list-push :internalSlotsList (atom Environment))
               (list-push :internalSlotsList (atom PrivateEnvironment))
               (list-push :internalSlotsList (atom FormalParameters))
               (list-push :internalSlotsList (atom ECMAScriptCode))
               (list-push :internalSlotsList (atom ConstructorKind))
               (list-push :internalSlotsList (atom Realm))
               (list-push :internalSlotsList (atom ScriptOrModule))
               (list-push :internalSlotsList (atom ThisMode))
               (list-push :internalSlotsList (atom Strict))
               (list-push :internalSlotsList (atom HomeObject))
               (list-push :internalSlotsList (atom SourceText))
               (list-push :internalSlotsList (atom Fields))
               (list-push :internalSlotsList (atom PrivateMethods))
               (list-push :internalSlotsList (atom ClassFieldInitializerName))
               (list-push :internalSlotsList (atom IsClassConstructor))
               (undefined)))))

;;;;;;;;;;;;;;;;;;
; something ; (STATIC SEMANTICS AND RUNTIME SEMANTICS WIP section)
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

(def (is-fn-obj :x) (record-has-slot :x Call))

(section
  (:6.1.6.1.14 Number::sameValue (x, y))
  ((return (:x == :y))))

(section
  (:6.1.6.2.14 BigInt::sameValue (x, y))
  ((return (:x == :y))))

(section
  (:6.2.3.4 UpdateEmpty (completionRecord, value))
  (;;; 1. Assert: If completionRecord.[[Type]] is either return or throw, then completionRecord.[[Value]] is not empty.
   (if (either atom-return atom-throw (== (:completionRecord -> Type)))
       ((assert (isnt-empty (:completionRecord -> Value)) "If completionRecord.[[Type]] is either return or throw, then completionRecord.[[Value]] is not empty.")))
   ;;; 2. If completionRecord.[[Value]] is not empty, return Completion(completionRecord).
   (if (isnt-empty (:completionRecord -> Value))
       ((return :completionRecord)))
   ;;; 3. Return Completion { [[Type]]: completionRecord.[[Type]], [[Value]]: value, [[Target]]: completionRecord.[[Target]] }.
   (rec = record-new)
   (record-copy-slot :completionRecord :rec Type)
   (:rec Value <- :value)
   (record-copy-slot :completionRecord :rec Target)
   (return :rec)))

(section
  (:6.2.4.1 IsPropertyReference (V))
  ((assert (is-reference-record :V) "V is a reference record")
   ;;; 1. If V.[[Base]] is unresolvable, return false.
   (if ((:V -> Base) == unresolvable)
       ((return false)))
   ;;; 2. If V.[[Base]] is an Environment Record, return false; otherwise return true.
   (return (not (is-environment-record (:V -> Base))))))

(section
  (:6.2.4.2 IsUnresolvableReference (V))
  ((assert (is-reference-record :V) "V is a reference record")
   ;;; 1. If V.[[Base]] is unresolvable, return true; otherwise return false.
   (return (is-unresolvable (:V -> Base)))))

(section
  (:6.2.4.3 IsSuperReference (V))
  (;;; 1. If V.[[ThisValue]] is not empty, return true; otherwise return false.
   (return (isnt-empty (:V -> ThisValue)))))

(section
  (:6.2.4.4 IsPrivateReference (V))
  (;;; 1. If V.[[ReferencedName]] is a Private Name, return true; otherwise return false.
   ; TODO: this
   (return false)))

(section
  (:6.2.4.5 GetValue (V))
  (;;; 1. ReturnIfAbrupt(V).
   (V = (? :V))
   ;;; 2. If V is not a Reference Record, return V.
   (if (isnt-reference-record :V)
       ((return :V)))
   ;;; 3. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
   (if (call IsUnresolvableReference :V)
       ((throw (ReferenceError "oopsies woopsies!"))))
   ;;; 4. If IsPropertyReference(V) is true, then
   (if (is-true (call IsPropertyReference :V))
       (;;; a. Let baseObj be ? ToObject(V.[[Base]]).
        (baseObj = (? (call ToObject (:V -> Base))))
        ;;; b. If IsPrivateReference(V) is true, then
        (if (is-true (call IsPrivateReference :V))
            (;;; i. Return ? PrivateGet(baseObj, V.[[ReferencedName]]).
             (return (? (call PrivateGet :baseObj (:V -> ReferencedName))))))
        ;;; c. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
        (return (? (:baseObj .. Get (:V -> ReferencedName) (call GetThisValue :V)))))
       ;;; 5. Else,
       (;;; a. Let base be V.[[Base]].
        (base = (:V -> Base))
        ;;; b. Assert: base is an Environment Record.
        (assert (is-environment-record :base) "base is an Environment Record.")
        ;;; c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
        (return (? (:base .. GetBindingValue (:V -> ReferencedName) (:V -> Strict))))))
   (return unreachable)))

(section
  (:6.2.4.7 GetThisValue (V))
  (;;; 1. Assert: IsPropertyReference(V) is true.
   (assert (call IsPropertyReference :V) "IsPropertyReference(V) is true.")
   ;;; 2. If IsSuperReference(V) is true, return V.[[ThisValue]]; otherwise return V.[[Base]].
   (if (call IsSuperReference :V)
       ((return (:V -> ThisValue))))
   (return (:V -> Base))))

(section
  (:6.2.4.8 InitializeReferencedBinding (V, W))
  (;;; 1. ReturnIfAbrupt(V).
   (V = (? :V))
   ;;; 2. ReturnIfAbrupt(W).
   (W = (? :W))
   ;;; 3. Assert: V is a Reference Record.
   (assert (is-reference-record :V) "V is a Reference Record.")
   ;;; 4. Assert: IsUnresolvableReference(V) is false.
   (assert (is-false (call IsUnresolvableReference :V)) "IsUnresolvableReference(V) is false.")
   ;;; 5. Let base be V.[[Base]].
   (base = :V -> Base)
   ;;; 6. Assert: base is an Environment Record.
   ;;; 7. Return base.InitializeBinding(V.[[ReferencedName]], W).
   (return (:base .. InitializeBinding (:V -> ReferencedName) :W))))

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
  (:7.1.2 ToBoolean (argument))
  ((if (is-undef :argument)
       ((return false)))
   (if (is-null :argument)
       ((return false)))
   (if (is-bool :argument)
       ((return :argument)))
   (if (is-number :argument)
       ((return ((:argument == 0) or (:argument == (not 0))))))
   (if (:argument == "")
       ((return false))
       ((return true)))
   (if (is-symbol :argument)
       ((return true)))
   (if (is-bigint :argument)
       ((if (:argument == 0)
            ((return false))
            ((return true)))))
   (if (is-object :argument)
       ((return true)))
   (return unreachable)))

(section
  (:7.1.17 ToString (argument))
  ((if (is-undef :argument)
       ((return "undefined")))
   (if (is-null :argument)
       ((return "null")))
   (if (lazyAnd (is-bool :argument) (is-true :argument))
       ((return "true")))
   (if (lazyAnd (is-bool :argument) (is-false :argument))
       ((return "false")))
   (if (is-number :argument)
       ((return "0.00TODO: 7.1.17 ToString (number)00")))
   (if (is-symbol :argument)
       ((throw (TypeError "cant do symbol to string"))))
   (if (is-bigint :argument)
       ((return "false")))
   (if (is-object :argument)
       ((return "{ TODO: 7.1.17 ToString (object) }")))
   (return unreachable)))

(section
  (:7.1.18 ToObject (argument))
  ((if (is-undef :argument)
       ((throw (TypeError "undefined -> object no worky"))))
   (if (is-null :argument)
       ((throw (TypeError "null -> object no worky"))))
   (if (is-bool :argument)
       ((wrapper = record-new)
        (:wrapper BooleanData <- :argument)
        (return :wrapper)))
   (if (is-number :argument)
       ((wrapper = record-new)
        (:wrapper NumberData <- :argument)
        (return :wrapper)))
   (if (is-string :argument)
       ((wrapper = record-new)
        (:wrapper StringData <- :argument)
        (return :wrapper)))
   (if (is-symbol :argument)
       ((wrapper = record-new)
        (:wrapper SymbolData <- :argument)
        (return :wrapper)))
   (if (is-bigint :argument)
       ((wrapper = record-new)
        (:wrapper BigIntData <- :argument)
        (return :wrapper)))
   (if (is-record :argument)
       ((return :argument)))
   (return unreachable)))

(section
  (:7.2.3 IsCallable (argument))
  (;;; 1. If Type(argument) is not Object, return false.
   (if (isnt-object :argument)
       ((return false)))
   ;;; 2. If argument has a [[Call]] internal method, return true.
   ;;; 3. Return false.
   (return (record-has-slot :argument Call))))

(section
  (:7.2.4 IsConstructor (argument))
  (;;; 1. If Type(argument) is not Object, return false.
   (if (isnt-object :argument)
       ((return false)))
   ;;; 2. If argument has a [[Construct]] internal method, return true.
   ;;; 3. Return false.
   (return (record-has-slot :argument Construct))))

(section
  (:7.2.5 IsExtensible (O))
  (;;; 1. Return ? O.[[IsExtensible]]().
   (return (? (:O .. IsExtensible)))))

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
   (:obj GetPrototypeOf <- (get-fn-ptr OrdinaryObjectInternalMethods_GetPrototypeOf))
   (:obj IsExtensible <- (get-fn-ptr OrdinaryObjectInternalMethods_IsExtensible))
   (:obj GetOwnProperty <- (get-fn-ptr OrdinaryObjectInternalMethods_GetOwnProperty))
   (:obj HasProperty <- (get-fn-ptr OrdinaryObjectInternalMethods_HasProperty))
   (:obj DefineOwnProperty <- (get-fn-ptr OrdinaryObjectInternalMethods_DefineOwnProperty))
   (:obj Get <- (get-fn-ptr OrdinaryObjectInternalMethods_Get))
   (:obj Set <- (get-fn-ptr OrdinaryObjectInternalMethods_Set))
   ;;; 3. Assert: If the caller will not be overriding both obj's [[GetPrototypeOf]] and [[SetPrototypeOf]] essential internal
   ;;;    methods, then internalSlotsList contains [[Prototype]].
   ;;; 4. Assert: If the caller will not be overriding all of obj's [[SetPrototypeOf]], [[IsExtensible]], and [[PreventExtensions]]
   ;;;    essential internal methods, then internalSlotsList contains [[Extensible]].
   ;;; 5. If internalSlotsList contains [[Extensible]], set obj.[[Extensible]] to true.
   (if (true)
       ((:obj Extensible <- true)))
   ;;; 6. Return obj.
   (return :obj)))

(section
  (:7.3.2 Get (O, P))
  (;;; 1. Return ? O.[[Get]](P, O).
   (return (:O .. Get :P :O))))

(section
  (:7.3.4 Set (O, P, V, Throw))
  (;;; 1. Let success be ? O.[[Set]](P, V, O).
   (success = (? (:O .. Set :P :V :O)))
   ;;; 2. If success is false and Throw is true, throw a TypeError exception.
   (if ((is-false :success) and (is-true :Throw))
       ((throw (TypeError "could not set peoprty"))))
   ;;; 3. Return success.
   (return :success)))

(section
  (:7.3.5 CreateDataProperty (O, P, V))
  (;;; 1. Let newDesc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true }.
   (newDesc = record-new)
   (:newDesc Value <- :V)
   (:newDesc Writable <- true)
   (:newDesc Enumerable <- true)
   (:newDesc Configurable <- true)
   ;;; 2. Return ? O.[[DefineOwnProperty]](P, newDesc).
   (return (? (:O .. DefineOwnProperty :P :newDesc)))))

(section
  (:7.3.7 CreateDataPropertyOrThrow (O, P, V))
  (;;; 1. Let success be ? CreateDataProperty(O, P, V).
   (success = (? (call CreateDataProperty :O :P :V)))
   ;;; 2. If success is false, throw a TypeError exception.
   (if (is-false :success)
       ((throw (TypeError "hha couldnt make the prop :((("))))
   ;;; 3. Return success.
   (return :success)))

(section
  (:7.3.9 DefinePropertyOrThrow (O, P, desc))
  (;;; 1. Let success be ? O.[[DefineOwnProperty]](P, desc).
   (success = (? (:O .. DefineOwnProperty :P :desc)))
   ;;; 2. If success is false, throw a TypeError exception.
   (if (is-false :success)
       ((throw (TypeError "can't define property"))))
   ;;; 3. Return success.
   (return :success)))

(section
  (:7.3.12 HasProperty (O, P))
  (;;; 1. Return ? O.[[HasProperty]](P).
   (return (? (:O .. HasProperty :P)))))

(section
  (:7.3.13 HasOwnProperty (O, P))
  (;;; 1. Let desc be ? O.[[GetOwnProperty]](P).
   (desc = (? (:O .. GetOwnProperty :P)))
   ;;; 2. If desc is undefined, return false.
   (if (is-undef :desc)
       ((return false)))
   ;;; 3. Return true.
   (return true)))

(section
  (:7.3.14 Call (F, V, argumentsList))
  (;;; 1. If argumentsList is not present, set argumentsList to a new empty List.
   ; the caller should be expected to do this
   ;;; 2. If IsCallable(F) is false, throw a TypeError exception.
   (if (is-false (call IsCallable :F))
       ((throw (TypeError "not callable :(("))))
   ;;; 3. Return ? F.[[Call]](V, argumentsList).
   (return (? (:F .. Call :V :argumentsList)))))

(section
  (:7.3.30 PrivateGet (O, P))
  (;;; 1. Let entry be ! PrivateElementFind(O, P).
   (todo)
   ;;; 2. If entry is empty, throw a TypeError exception.
   ;;; 3. If entry.[[Kind]] is field or method, then
   ;;; a. Return entry.[[Value]].
   ;;; 4. Assert: entry.[[Kind]] is accessor.
   ;;; 5. If entry.[[Get]] is undefined, throw a TypeError exception.
   ;;; 6. Let getter be entry.[[Get]].
   ;;; 7. Return ? Call(getter, O).
   (return unreachable)))

(section
  (:7.4.1 GetIterator (obj, hint, method))
  (;;; 1. If hint is not present, set hint to sync.
   (hint = (expr-block ((if (is-undef :hint) (sync) (:hint)))))
   ;;; 2. If method is not present, then
   (todo)
   ;;; a. If hint is async, then
   ;;; i. Set method to ? GetMethod(obj, @@asyncIterator).
   ;;; ii. If method is undefined, then
   ;;; 1. Let syncMethod be ? GetMethod(obj, @@iterator).
   ;;; 2. Let syncIteratorRecord be ? GetIterator(obj, sync, syncMethod).
   ;;; 3. Return ! CreateAsyncFromSyncIterator(syncIteratorRecord).
   ;;; b. Otherwise, set method to ? GetMethod(obj, @@iterator).
   ;;; 3. Let iterator be ? Call(method, obj).
   ;;; 4. If Type(iterator) is not Object, throw a TypeError exception.
   ;;; 5. Let nextMethod be ? GetV(iterator, "next").
   ;;; 6. Let iteratorRecord be the Record { [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false }.
   ;;; 7. Return iteratorRecord.
   (return unreachable)))

(section
  (:7.4.2 IteratorNext (iteratorRecord, value))
  (;;; 1. If value is not present, then
   (todo)
   (return unreachable)
   ;;; a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]]).
   ;;; 2. Else,
   ;;; a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]], « value »).
   ;;; 3. If Type(result) is not Object, throw a TypeError exception.
   ;;; 4. Return result.
  ))

(section
  (:7.4.3 IteratorComplete (iterResult))
  (;;; 1. Return ! ToBoolean(? Get(iterResult, "done")).
   (return (! (call ToBoolean (? (call Get :iterResult "done")))))))

(section
  (:7.4.4 IteratorValue (iterResult))
  (;;; 1. Return ? Get(iterResult, "value").
   (return (? (call Get :iterResult "value")))))

(section
  (:7.4.5 IteratorStep (iteratorRecord))
  (;;; 1. Let result be ? IteratorNext(iteratorRecord).
   (result = (? (call IteratorNext :iteratorRecord undefined)))
   ;;; 2. Let done be ? IteratorComplete(result).
   (done = (? (call IteratorComplete :result)))
   ;;; 3. If done is true, return false.
   (if (:done)
       ((return false)))
   ;;; 4. Return result.
   (return :result)))

(section (:7.4.10 CreateListIteratorRecord (list))
  (;;; 1. Let closure be a new Abstract Closure with no parameters that captures list and performs the following steps when called:
   ;;; a. For each element E of list, do
   ;;; i. Perform ? Yield(E).
   (todo) ; we need to yield
   ; generators may be some effort to do
   ;;; b. Return undefined.
   ;;; 2. Let iterator be ! CreateIteratorFromClosure(closure, empty, %IteratorPrototype%).
   ;;; 3. Return Record { [[Iterator]]: iterator, [[NextMethod]]: %GeneratorFunction.prototype.prototype.next%, [[Done]]: false }.
   (return unreachable)))
(section (:7.4.10-abstractclosure CreateListIteratorRecord_AbstractClosure (self)) ((todo) (return unreachable)))

(section
  (:8.1.1 BoundNames (parseNode))
  (; BindingIdentifier : Identifier
   (if (match-pn :parseNode (atom BindingIdentifier) 0)
       (;;; 1. Return a List whose sole element is the StringValue of Identifier.
        (return (list-new-1 (:parseNode -> JSSATParseNodeSlot1 -> JSSATParseNode_Identifier_StringValue)))))
   ; BindingIdentifier : yield
   (if (match-pn :parseNode (atom BindingIdentifier) 1)
       (;;; 1. Return a List whose sole element "yield".
        (return (list-new-1 "yield"))))
   ; BindingIdentifier : await
   (if (match-pn :parseNode (atom BindingIdentifier) 2)
       (;;; 1. Return a List whose sole element "await".
        (return (list-new-1 "await"))))
   ; FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
   (if (is-pn FunctionDeclaration 0)
       (;;; 1. Return the BoundNames of BindingIdentifier.
        (return (call BoundNames (:parseNode -> JSSATParseNodeSlot1)))))
   ; FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
   (if (is-pn FunctionDeclaration 1)
       (;;; 1. Return « "*default*" ».
        (return (list-new-1 "*default*"))))
   ; FormalParameters : [empty]
   (if (is-pn FormalParameters 0)
       (;;; 1. Return a new empty List.
        (return (list-new))))
   ; FormalParameterList : FormalParameterList , FormalParameter
   (if (is-pn FormalParameterList 1)
       (;;; 1. Let names1 be BoundNames of FormalParameterList.
        (names1 = (call BoundNames (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let names2 be BoundNames of FormalParameter.
        (names2 = (call BoundNames (:parseNode -> JSSATParseNodeSlot2)))
        ;;; 3. Return the list-concatenation of names1 and names2.
        (return (list-concat :names1 :names2))))
   ; SingleNameBinding : BindingIdentifier Initializeropt
   (if (pn-kind-is :parseNode SingleNameBinding)
       (;;; 1. Return the BoundNames of BindingIdentifier.
        (return (call BoundNames (:parseNode -> JSSATParseNodeSlot1)))))
   (return (call BoundNames (:parseNode -> JSSATParseNodeSlot1)))))

(section
  (:8.1.2 DeclarationPart (parseNode))
  (; HoistableDeclaration : FunctionDeclaration
   ;;; 1. Return FunctionDeclaration.
   ; HoistableDeclaration : GeneratorDeclaration
   ;;; 1. Return GeneratorDeclaration.
   ; HoistableDeclaration : AsyncFunctionDeclaration
   ;;; 1. Return AsyncFunctionDeclaration.
   ; HoistableDeclaration : AsyncGeneratorDeclaration
   ;;; 1. Return AsyncGeneratorDeclaration.
   ; Declaration : ClassDeclaration
   ;;; 1. Return ClassDeclaration.
   ; Declaration : LexicalDeclaration
   ;;; 1. Return LexicalDeclaration.

   ; key observation: every single production involes returning the first rule
   ; due to how JSSAT is set up, we can simply get the parse node in the first slot
   (return (:parseNode -> JSSATParseNodeSlot1))))

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
   (if (match-pn :parseNode (atom StatementList) 1)
       (;;; 1. Let declarations1 be VarScopedDeclarations of StatementList.
        (declarations1 = (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let declarations2 be VarScopedDeclarations of StatementListItem.
        (declarations2 = (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot2)))
        ;;; 3. Return the list-concatenation of declarations1 and declarations2.
        (return (list-concat :declarations1 :declarations2))))
   ; VariableDeclarationList : VariableDeclaration
   (if (match-pn :parseNode (atom VariableDeclarationList) 0)
       (;;; 1. Return a List whose sole element is VariableDeclaration.
        (return (list-new-1 (:parseNode -> JSSATParseNodeSlot1)))))
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

   ; TODO: the default path should fall through to calling `VarScopedDeclarations` again
   ; for now im too lazy to do that
   (if (is-pn Script 1)
       ((return (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))))
   (return list-new)))

(section
  (:8.1.8 TopLevelLexicallyDeclaredNames (parseNode))
  ((return list-new)))

(section
  (:8.1.11 TopLevelVarScopedDeclarations (parseNode))
  (; StatementList : StatementList StatementListItem
   (if (is-pn StatementList 1)
       (;;; 1. Let declarations1 be TopLevelVarScopedDeclarations of StatementList.
        (declarations1 = (call TopLevelVarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let declarations2 be TopLevelVarScopedDeclarations of StatementListItem.
        (declarations2 = (call TopLevelVarScopedDeclarations (:parseNode -> JSSATParseNodeSlot2)))
        ;;; 3. Return the list-concatenation of declarations1 and declarations2.
        (return (list-concat :declarations1 :declarations2))))
   ; StatementListItem : Statement
   (if (is-pn StatementListItem 0)
       (;;; 1. If Statement is Statement : LabelledStatement , return TopLevelVarScopedDeclarations of Statement.
        ;;; 2. Return VarScopedDeclarations of Statement.
        (return (call VarScopedDeclarations (:parseNode -> JSSATParseNodeSlot1)))))
   ; StatementListItem : Declaration
   (if (is-pn StatementListItem 1)
       (;;; 1. If Declaration is Declaration : HoistableDeclaration , then
        (Declaration = :parseNode -> JSSATParseNodeSlot1)
        ; Declaration : HoistableDeclaration
        (if (match-pn :Declaration Declaration 0)
            (;;; a. Let declaration be DeclarationPart of HoistableDeclaration.
             (HoistableDeclaration = :Declaration -> JSSATParseNodeSlot1)
             (declaration = (call DeclarationPart :HoistableDeclaration))
             ;;; b. Return « declaration ».
             (return (list-new-1 :declaration))))
        ;;; 2. Return a new empty List.
        (return list-new)))
   (return list-new)))

(section
  (:8.5.1 InstantiateFunctionObject (parseNode, scope, privateScope))
  (; FunctionDeclaration :
   ;     function BindingIdentifier ( FormalParameters ) { FunctionBody }
   ;     function ( FormalParameters ) { FunctionBody }
   (if (pn-kind-is :parseNode FunctionDeclaration)
       (;;; 1. Return ? InstantiateOrdinaryFunctionObject of FunctionDeclaration with arguments scope and privateScope.
        (return (? (call InstantiateOrdinaryFunctionObject :parseNode :scope :privateScope)))))
   (todo)
   (return unreachable)))

(section
  (:8.5.3 IteratorBindingInitialization (parseNode, iteratorRecord, environment))
  (; FormalParameters : [empty]
   (if (is-pn FormalParameters 0)
       (;;; 1. Return NormalCompletion(empty).
        (return (NormalCompletion empty))))
   ; FormalParameterList : FormalParameterList , FormalParameter
   (if (is-pn FormalParameterList 1)
       (;;; 1. Perform ? IteratorBindingInitialization for FormalParameterList using iteratorRecord and environment as the arguments.
        ;;; 2. Return the result of performing IteratorBindingInitialization for FormalParameter using iteratorRecord and environment as the arguments.
        ; we dont do iterators yet, so we will run our own hacky algo!
        ; we can mutate `:iteratorRecord` because we clone it before passing it in here
        ; TODO: figure out how to do this
        (todo)
        (_dontCare = (? (call IteratorBindingInitialization (:parseNode -> JSSATParseNodeSlot1) :iteratorRecord :environment)))
        (return (call IteratorBindingInitialization (:parseNode -> JSSATParseNodeSlot2) :iteratorRecord :environment))))
   ; SingleNameBinding : BindingIdentifier Initializeropt (opt not included)
   (if (is-pn SingleNameBinding 0)
       (;;; 1. Let bindingId be StringValue of BindingIdentifier.
        (bindingId = (StringValueOfBindingIdentifier (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let lhs be ? ResolveBinding(bindingId, environment).
        (lhs = (? (call ResolveBinding :bindingId :environment)))
        ;;; 3. Let v be undefined.
        (v = undefined)
        ;;; 4. If iteratorRecord.[[Done]] is false, then
        (v =
           (if ((list-len :iteratorRecord) > 0)
               (;;; a. Let next be IteratorStep(iteratorRecord).
                (next = (list-pop :iteratorRecord))
                ;;; b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
                ;;; c. ReturnIfAbrupt(next).
                ;;; d. If next is false, set iteratorRecord.[[Done]] to true.
                ;;; e. Else,
                ;;; i. Set v to IteratorValue(next).
                ;;; ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
                ;;; iii. ReturnIfAbrupt(v).
                (:next))
               ((:v))))
        ;;; 5. If Initializer is present and v is undefined, then
        ;;; a. If IsAnonymousFunctionDefinition(Initializer) is true, then
        ;;; i. Set v to the result of performing NamedEvaluation for Initializer with argument bindingId.
        ;;; b. Else,
        ;;; i. Let defaultValue be the result of evaluating Initializer.
        ;;; ii. Set v to ? GetValue(defaultValue).
        ;;; 6. If environment is undefined, return ? PutValue(lhs, v).
        (if (is-undef :environment)
            ((assert false "TODO: environment is undefined, implement Putvalue")))
        ;;; 7. Return InitializeReferencedBinding(lhs, v).
        (return (call InitializeReferencedBinding :lhs :v))))
   (return (call IteratorBindingInitialization (:parseNode -> JSSATParseNodeSlot1) :iteratorRecord :environment))))

(section
  (:9.1.1.1.1 DeclarativeEnvironmentRecord_HasBinding (envRec, N))
  (;;; 1. If envRec has a binding for the name that is the value of N, return true.
   (if (record-has-prop :envRec :N)
       ((return true)))
   ;;; 2. Return false.
   (return false)))

(section
  (:9.1.1.1.2 DeclarativeEnvironmentRecord_CreateMutableBinding (envRec, N, D))
  (;;; 1. Assert: envRec does not already have a binding for N.
   (assert (is-false (:envRec .. HasBinding :N)) "envRec does not already have a binding for N.")
   ;;; 2. Create a mutable binding in envRec for N and record that it is uninitialized. If D is true, record that the
   ;;;    newly created binding may be deleted by a subsequent DeleteBinding call.
   (binding = record-new)
   (:binding IsMutable <- true)
   (:binding BindingInitialized <- uninitialized)
   (:binding DeletableByDeleteBinding <- true)
   (:binding Strict <- false)
   (record-set-prop :envRec :N :binding)
   ;;; 3. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))

(section
  (:9.1.1.1.3 DeclarativeEnvironmentRecord_CreateImmutableBinding (envRec, N, S))
  (;;; 1. Assert: envRec does not already have a binding for N.
   (assert (is-false (:envRec .. HasBinding :N)) "envRec does not already have a binding for N.")
   ;;; 2. Create an immutable binding in envRec for N and record that it is uninitialized. If S is true, record that the
   ;;;    newly created binding is a strict binding.
   (binding = record-new)
   (:binding IsMutable <- false)
   (:binding BindingInitialized <- uninitialized)
   (:binding DeletableByDeleteBinding <- false)
   (:binding Strict <- :S)
   (record-set-prop :envRec :N :binding)
   ;;; 3. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))

(section
  (:9.1.1.1.4 DeclarativeEnvironmentRecord_InitializeBinding (envRec, N, V))
  (;;; 1. Assert: envRec must have an uninitialized binding for N.
   (assert ((:envRec => :N -> BindingInitialized) == uninitialized) "envRec must have an uninitialized binding for N.")
   ;;; 2. Set the bound value for N in envRec to V.
   (binding = :envRec => :N)
   (:binding Value <- :V)
   ;;; 3. Record that the binding for N in envRec has been initialized.
   (:binding BindingInitialized <- initialized)
   ;;; 4. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))

(section
  (:9.1.1.1.5 DeclarativeEnvironmentRecord_SetMutableBinding (envRec, N, V, S))
  (;;; 1. If envRec does not have a binding for N, then
   (if (not (record-has-prop :envRec :N))
       (;;; a. If S is true, throw a ReferenceError exception.
        (if (is-true :S)
            ((throw (ReferenceError "set mutable binding in decl failed"))))
        ;;; b. Perform envRec.CreateMutableBinding(N, true).
        (:envRec .. CreateMutableBinding :N true)
        ;;; c. Perform envRec.InitializeBinding(N, V).
        (:envRec .. InitializeBinding :N :V)
        ;;; d. Return NormalCompletion(empty).
        (return (NormalCompletion empty))))
   ;;; 2. If the binding for N in envRec is a strict binding, set S to true.
   (binding = (:envRec => :N))
   (S = (:S or (is-true (:binding -> Strict))))
   ;;; 3. If the binding for N in envRec has not yet been initialized, throw a ReferenceError exception.
   (if (:binding -> BindingInitialized == uninitialized)
       ((throw (ReferenceError "binding not initialized")))
       ;;; 4. Else if the binding for N in envRec is a mutable binding, change its bound value to V.
       (elif (is-true (:binding -> IsMutable))
             ((:binding Value <- :V))
             ;;; 5. Else,
             (;;; a. Assert: This is an attempt to change the value of an immutable binding.
              (assert (is-false (:binding -> IsMutable)) "This is an attempt to change the value of an immutable binding.")
              ;;; b. If S is true, throw a TypeError exception.
              (if (is-true :S)
                  ((throw (TypeError "binding is changing")))))))
   ;;; 6. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))

(section
  (:9.1.1.1.6 DeclarativeEnvironmentRecord_GetBindingValue (envRec, N, S))
  (;;; 1. Assert: envRec has a binding for N.
   (assert (:envRec .. HasBinding :N) "envRec has a binding for N.")
   ;;; 2. If the binding for N in envRec is an uninitialized binding, throw a ReferenceError exception.
   (if ((:envRec => :N -> BindingInitialized) == uninitialized)
       ((throw (ReferenceError "not initialized :((("))))
   ;;; 3. Return the value currently bound to N in envRec.
   (return (:envRec => :N -> Value))))

(section
  (:9.1.1.1.10 DeclarativeEnvironmentRecord_WithBaseObject (envRec))
  (;;; 1. Return undefined.
   (return undefined)))

(section
  (:9.1.1.2.1 ObjectEnvironmentRecord_HasBinding (envRec, N))
  (;;; 1. Let bindingObject be envRec.[[BindingObject]].
   (bindingObject = (:envRec -> BindingObject))
   ;;; 2. Let foundBinding be ? HasProperty(bindingObject, N).
   (foundBinding = (? (call HasProperty :bindingObject :N)))
   ;;; 3. If foundBinding is false, return false.
   (if (is-false :foundBinding)
       ((return false)))
   ;;; 4. If envRec.[[IsWithEnvironment]] is false, return true.
   (if (is-false (:envRec -> IsWithEnvironment))
       ((return true)))
   ;;; 5. Let unscopables be ? Get(bindingObject, @@unscopables).
   ;;; 6. If Type(unscopables) is Object, then
   ;;; a. Let blocked be ! ToBoolean(? Get(unscopables, N)).
   ;;; b. If blocked is true, return false.
   ;;; 7. Return true.
   (return true)))

(section
  (:9.1.1.2.2 ObjectEnvironmentRecord_CreateMutableBinding (envRec, N, D))
  (;;; 1. Let bindingObject be envRec.[[BindingObject]].
   (bindingObject = :envRec -> BindingObject)
   ;;; 2. Return ? DefinePropertyOrThrow(bindingObject, N, PropertyDescriptor { [[Value]]: undefined, [[Writable]]: true,
   ;;;    [[Enumerable]]: true, [[Configurable]]: D }).
   (desc = record-new)
   (:desc Value <- undefined)
   (:desc Writable <- true)
   (:desc Enumerable <- true)
   (:desc Configurable <- :D)
   (return (? (call DefinePropertyOrThrow :bindingObject :N :desc)))))

(section
  (:9.1.1.2.3 ObjectEnvironmentRecord_CreateImmutableBinding (N, S))
  (; The CreateImmutableBinding concrete method of an object Environment Record is never used within this specification.
   (return)))

(section
  (:9.1.1.2.4 ObjectEnvironmentRecord_InitializeBinding (envRec, N, V))
  (;;; 1. Return ? envRec.SetMutableBinding(N, V, false).
   (return (? (:envRec .. SetMutableBinding :N :V false)))))

(section
  (:9.1.1.2.5 ObjectEnvironmentRecord_SetMutableBinding (envRec, N, V, S))
  (;;; 1. Let bindingObject be envRec.[[BindingObject]].
   (bindingObject = :envRec -> BindingObject)
   ;;; 2. Let stillExists be ? HasProperty(bindingObject, N).
   (stillExists = (? (call HasProperty :bindingObject :N)))
   ;;; 3. If stillExists is false and S is true, throw a ReferenceError exception.
   (if ((is-false :stillExists) and (is-true :S))
       ((throw (ReferenceError "set mtuable bindign!!!"))))
   ;;; 4. Return ? Set(bindingObject, N, V, S).
   (return (? (call Set :bindingObject :N :V :S)))))

(section
  (:9.1.1.2.6 ObjectEnvironmentRecord_GetBindingValue (envRec, N, S))
  (;;; 1. Let bindingObject be envRec.[[BindingObject]].
   (bindingObject = (:envRec -> BindingObject))
   ;;; 2. Let value be ? HasProperty(bindingObject, N).
   (value = (? (call HasProperty :bindingObject :N)))
   ;;; 3. If value is false, then
   (if (is-false :value)
       (;;; a. If S is false, return the value undefined; otherwise throw a ReferenceError exception.
        (if (is-false :S)
            ((return undefined))
            ((throw (ReferenceError "le strict mode lack of binding"))))))
   ;;; 4. Return ? Get(bindingObject, N).
   (return (? (call Get :bindingObject :N)))))

(section
  (:9.1.1.2.10 ObjectEnvironmentRecord_WithBaseObject (envRec))
  (;;; 1. If envRec.[[IsWithEnvironment]] is true, return envRec.[[BindingObject]].
   (if (:envRec -> IsWithEnvironment)
       ((return (:envRec -> BindingObject))))
   ;;; 2. Otherwise, return undefined.
   (return undefined)))

(section
  (:9.1.1.3.1 BindThisValue (envRec, V))
  (;;; 1. Assert: envRec.[[ThisBindingStatus]] is not lexical.
   (assert ((:envRec -> ThisBindingStatus) != lexical) "envRec.[[ThisBindingStatus]] is not lexical.")
   ;;; 2. If envRec.[[ThisBindingStatus]] is initialized, throw a ReferenceError exception.
   (if ((:envRec -> ThisBindingStatus) == initialized)
       ((throw (ReferenceError "couldnt bind this value idk"))))
   ;;; 3. Set envRec.[[ThisValue]] to V.
   (:envRec ThisValue <- :V)
   ;;; 4. Set envRec.[[ThisBindingStatus]] to initialized.
   (:envRec ThisBindingStatus <- initialized)
   ;;; 5. Return V.
   (return :V)))

(section
  (:9.1.1.4.1 GlobalEnvironmentRecord_HasBinding (envRec, N))
  (;;; 1. Let DclRec be envRec.[[DeclarativeRecord]].
   (DclRec = (:envRec -> DeclarativeRecord))
   ;;; 2. If DclRec.HasBinding(N) is true, return true.
   (if (is-true (:DclRec .. HasBinding :N))
       ((return true)))
   ;;; 3. Let ObjRec be envRec.[[ObjectRecord]].
   (ObjRec = (:envRec -> ObjectRecord))
   ;;; 4. Return ? ObjRec.HasBinding(N).
   (return (? (:ObjRec .. HasBinding :N)))))

(section
  (:9.1.1.4.2 GlobalEnvironmentRecord_CreateMutableBinding (envRec, N, D))
  (;;; 1. Let DclRec be envRec.[[DeclarativeRecord]].
   (DclRec = :envRec -> DeclarativeRecord)
   ;;; 2. If DclRec.HasBinding(N) is true, throw a TypeError exception.
   (if (is-true (:DclRec .. HasBinding :N))
       ((throw (TypeError "it already has binding"))))
   ;;; 3. Return DclRec.CreateMutableBinding(N, D).
   (return (:DclRec .. CreateMutableBinding :N :D))))

(section
  (:9.1.1.4.3 GlobalEnvironmentRecord_CreateImmutableBinding (envRec, N, S))
  (;;; 1. Let DclRec be envRec.[[DeclarativeRecord]].
   (DclRec = :envRec -> DeclarativeRecord)
   ;;; 2. If DclRec.HasBinding(N) is true, throw a TypeError exception.
   (if (is-true (:DclRec .. HasBinding :N))
       ((throw (TypeError "already has bindigni!NN!IN!"))))
   ;;; 3. Return DclRec.CreateImmutableBinding(N, S).
   (return (:DclRec .. CreateImmutableBinding :N :S))))

(section
  (:9.1.1.4.4 GlobalEnvironmentRecord_InitializeBinding (envRec, N, V))
  (;;; 1. Let DclRec be envRec.[[DeclarativeRecord]].
   (DclRec = :envRec -> DeclarativeRecord)
   ;;; 2. If DclRec.HasBinding(N) is true, then
   (if (is-true (:DclRec .. HasBinding :N))
       (;;; a. Return DclRec.InitializeBinding(N, V).
        (return (:DclRec .. InitializeBinding :N :V))))
   ;;; 3. Assert: If the binding exists, it must be in the object Environment Record.
   (ObjRec = :envRec -> ObjectRecord)
   (assert (:ObjRec .. HasBinding :N) "If the binding exists, it must be in the object Environment Record.")
   ;;; 4. Let ObjRec be envRec.[[ObjectRecord]].
   ;;; 5. Return ? ObjRec.InitializeBinding(N, V).
   (return (? (:ObjRec .. InitializeBinding :N :V)))))

(section
  (:9.1.1.4.5 GlobalEnvironmentRecord_SetMutableBinding (envRec, N, V, S))
  (;;; 1. Let DclRec be envRec.[[DeclarativeRecord]].
   (DclRec = :envRec -> DeclarativeRecord)
   ;;; 2. If DclRec.HasBinding(N) is true, then
   (if (is-true (:DclRec .. HasBinding :N))
       (;;; a. Return DclRec.SetMutableBinding(N, V, S).
        (return (:DclRec .. SetMutableBinding :N :V :S))))
   ;;; 3. Let ObjRec be envRec.[[ObjectRecord]].
   (ObjRec = :envRec -> ObjectRecord)
   ;;; 4. Return ? ObjRec.SetMutableBinding(N, V, S).
   (return (? (:ObjRec .. SetMutableBinding :N :V :S)))))

(section
  (:9.1.1.4.6 GlobalEnvironmentRecord_GetBindingValue (envRec, N, S))
  (;;; 1. Let DclRec be envRec.[[DeclarativeRecord]].
   (DclRec = (:envRec -> DeclarativeRecord))
   ;;; 2. If DclRec.HasBinding(N) is true, then
   (if (:DclRec .. HasBinding :N)
       (;;; a. Return DclRec.GetBindingValue(N, S).
        (return (:DclRec .. GetBindingValue :N :S))))
   ;;; 3. Let ObjRec be envRec.[[ObjectRecord]].
   (ObjRec = (:envRec -> ObjectRecord))
   ;;; 4. Return ? ObjRec.GetBindingValue(N, S).
   (return (? (:ObjRec .. GetBindingValue :N :S)))))

(section
  (:9.1.1.4.10 GlobalEnvironmentRecord_WithBaseObject (envRec))
  (;;; 1. Return undefined.
   (return undefined)))

(section
  (:9.1.1.4.15 CanDeclareGlobalVar (envRec, N))
  (;;; 1. Let ObjRec be envRec.[[ObjectRecord]].
   (ObjRec = (:envRec -> ObjectRecord))
   ;;; 2. Let globalObject be ObjRec.[[BindingObject]].
   (globalObject = (:ObjRec -> BindingObject))
   ;;; 3. Let hasProperty be ? HasOwnProperty(globalObject, N).
   (hasProperty = (? (call HasOwnProperty :globalObject :N)))
   ;;; 4. If hasProperty is true, return true.
   (if (is-true :hasProperty)
       ((return true)))
   ;;; 5. Return ? IsExtensible(globalObject).
   (return (? (call IsExtensible :globalObject)))))

(section
  (:9.1.1.4.16 CanDeclareGlobalFunction (envRec, N))
  (;;; 1. Let ObjRec be envRec.[[ObjectRecord]].
   (ObjRec = (:envRec -> ObjectRecord))
   ;;; 2. Let globalObject be ObjRec.[[BindingObject]].
   (globalObject = (:ObjRec -> BindingObject))
   ;;; 3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
   (existingProp = (? (:globalObject .. GetOwnProperty :N)))
   ;;; 4. If existingProp is undefined, return ? IsExtensible(globalObject).
   (if (is-undef :existingProp)
       ((return (? (call IsExtensible :globalObject)))))
   ;;; 5. If existingProp.[[Configurable]] is true, return true.
   (if (is-true (:existingProp -> Configurable))
       ((return true)))
   ;;; 6. If IsDataDescriptor(existingProp) is true and existingProp has attribute values { [[Writable]]: true, [[Enumerable]]: true }, return true.
   (if ((is-true (call IsDataDescriptor :existingProp)) and (and (:existingProp -> Writable == true) (:existingProp -> Enumerable == true)))
       ((return true)))
   ;;; 7. Return false.
   (return false)))

(section
  (:9.1.1.4.18 CreateGlobalFunctionBinding (envRec, N, V, D))
  (;;; 1. Let ObjRec be envRec.[[ObjectRecord]].
   (ObjRec = :envRec -> ObjectRecord)
   ;;; 2. Let globalObject be ObjRec.[[BindingObject]].
   (globalObject = :ObjRec -> BindingObject)
   ;;; 3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
   (existingProp = (? (:globalObject .. GetOwnProperty :N)))
   ;;; 4. If existingProp is undefined or existingProp.[[Configurable]] is true, then
   (desc =
         (if (lazyOr (is-undef :existingProp) (is-true (:existingProp -> Configurable)))
             (;;; a. Let desc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: D }.
              (desc = record-new)
              (:desc Value <- :V)
              (:desc Writable <- true)
              (:desc Enumerable <- true)
              (:desc Configurable <- :D)
              (:desc))
             ;;; 5. Else,
             (;;; a. Let desc be the PropertyDescriptor { [[Value]]: V }.
              (desc = record-new)
              (:desc Value <- :V)
              (:desc))))
   ;;; 6. Perform ? DefinePropertyOrThrow(globalObject, N, desc).
   (_dontCare = (? (call DefinePropertyOrThrow :globalObject :N :desc)))
   ;;; 7. Perform ? Set(globalObject, N, V, false).
   (_dontCare = (? (call Set :globalObject :N :V false)))
   ;;; 8. Let varDeclaredNames be envRec.[[VarNames]].
   (varDeclaredNames = :envRec -> VarNames)
   ;;; 9. If varDeclaredNames does not contain N, then
   (if (not (list-contains :varDeclaredNames :N))
       (;;; a. Append N to varDeclaredNames.
        (list-push :varDeclaredNames :N)))
   ;;; 10. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))

(section
  (:9.1.2.1 GetIdentifierReference (env, name, strict))
  (;;; 1. If env is the value null, then
   (if (is-null :env)
       (;;; a. Return the Reference Record { [[Base]]: unresolvable, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
        (refRec = record-new)
        (:refRec Base <- unresolvable)
        (:refRec ReferencedName <- :name)
        (:refRec Strict <- :strict)
        (:refRec ThisValue <- empty)
        (return :refRec)))
   ;;; 2. Let exists be ? env.HasBinding(name).
   (exists = (? (:env .. HasBinding :name)))
   ;;; 3. If exists is true, then
   (if (is-true :exists)
       (;;; a. Return the Reference Record { [[Base]]: env, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty }.
        (refRec = record-new)
        (:refRec ReferencedName <- :name)
        (:refRec Strict <- :strict)
        (:refRec ThisValue <- empty)
        (:refRec Base <- :env)
        (return :refRec))
       ;;; 4. Else,
       (;;; a. Let outer be env.[[OuterEnv]].
        (outer = (:env -> OuterEnv))
        ;;; b. Return ? GetIdentifierReference(outer, name, strict).
        (return (? (call GetIdentifierReference :outer :name :strict)))))
   (return unreachable)))

(section
  (:9.1.2.2 NewDeclarativeEnvironment (E))
  (;;; 1. Let env be a new declarative Environment Record containing no bindings.
   (env = new-declarative-environment-record)
   ;;; 2. Set env.[[OuterEnv]] to E.
   (:env OuterEnv <- :E)
   ;;; 3. Return env.
   (return :env)))

(section
  (:9.1.2.3 NewObjectEnvironment (O, W, E))
  (;;; 1. Let env be a new object Environment Record.
   (env = record-new)
   (:env JSSATHasBinding <- (get-fn-ptr ObjectEnvironmentRecord_HasBinding))
   (:env CreateMutableBinding <- (get-fn-ptr ObjectEnvironmentRecord_CreateMutableBinding))
   (:env CreateImmutableBinding <- (get-fn-ptr ObjectEnvironmentRecord_CreateImmutableBinding))
   (:env InitializeBinding <- (get-fn-ptr ObjectEnvironmentRecord_InitializeBinding))
   (:env SetMutableBinding <- (get-fn-ptr ObjectEnvironmentRecord_SetMutableBinding))
   (:env GetBindingValue <- (get-fn-ptr ObjectEnvironmentRecord_GetBindingValue))
   (:env WithBaseObject <- (get-fn-ptr ObjectEnvironmentRecord_WithBaseObject))
   ;;; 2. Set env.[[BindingObject]] to O.
   (:env BindingObject <- :O)
   ;;; 3. Set env.[[IsWithEnvironment]] to W.
   (:env IsWithEnvironment <- :W)
   ;;; 4. Set env.[[OuterEnv]] to E.
   (:env OuterEnv <- :E)
   ;;; 5. Return env.
   (return :env)))

(section
  (:9.1.2.4 NewFunctionEnvironment (F, newTarget))
  (;;; 1. Let env be a new function Environment Record containing no bindings.
   ; "A function Environment Record is a declarative Environment Record that is"
   (env = new-declarative-environment-record)
   ;;; 2. Set env.[[FunctionObject]] to F.
   (:env FunctionObject <- :F)
   ;;; 3. If F.[[ThisMode]] is lexical, set env.[[ThisBindingStatus]] to lexical.
   (if ((:F -> ThisMode) == lexical)
       ((:env ThisBindingStatus <- lexical))
       (;;; 4. Else, set env.[[ThisBindingStatus]] to uninitialized.
        (:env ThisBindingStatus <- uninitialized)))
   ;;; 5. Set env.[[NewTarget]] to newTarget.
   (:env NewTarget <- :newTarget)
   ;;; 6. Set env.[[OuterEnv]] to F.[[Environment]].
   (:env OuterEnv <- (:F -> Environment))
   ;;; 7. Return env.
   (return :env)))

(section
  (:9.1.2.5 NewGlobalEnvironment (G, thisValue))
  (;;; 1. Let objRec be NewObjectEnvironment(G, false, null).
   (objRec = (call NewObjectEnvironment :G false null))
   ;;; 2. Let dclRec be a new declarative Environment Record containing no bindings.
   (dclRec = new-declarative-environment-record)
   ;;; 3. Let env be a new global Environment Record.
   (env = record-new)
   (:env JSSATHasBinding <- (get-fn-ptr GlobalEnvironmentRecord_HasBinding))
   (:env CreateMutableBinding <- (get-fn-ptr GlobalEnvironmentRecord_CreateMutableBinding))
   (:env CreateImmutableBinding <- (get-fn-ptr GlobalEnvironmentRecord_CreateImmutableBinding))
   (:env InitializeBinding <- (get-fn-ptr GlobalEnvironmentRecord_InitializeBinding))
   (:env SetMutableBinding <- (get-fn-ptr GlobalEnvironmentRecord_SetMutableBinding))
   (:env GetBindingValue <- (get-fn-ptr GlobalEnvironmentRecord_GetBindingValue))
   (:env WithBaseObject <- (get-fn-ptr GlobalEnvironmentRecord_WithBaseObject))
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
                     ; TODO: actually use the intrinsics
                     (! (call OrdinaryObjectCreate null list-new)))
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
  (:9.4.1 GetActiveScriptOrModule ())
  (;;; 1. If the execution context stack is empty, return null.
   (if (exec-ctx-stack-size == 0)
       ((return null)))
   ;;; 2. Let ec be the topmost execution context on the execution context stack whose ScriptOrModule component is not null.
   ;;; 3. If no such execution context exists, return null. Otherwise, return ec's ScriptOrModule.
   (for exec-ctx-stack
        ((execCtx = for-item-rev)
         (scriptOrModule = (:execCtx -> ScriptOrModule))
         (if (isnt-null :scriptOrModule)
             ((return :scriptOrModule)))))
   (return null)))

(section
  (:9.4.2 ResolveBinding (name, env))
  (;;; 1. If env is not present or if env is undefined, then
   (env = (expr-block
           ((if (is-undef :env)
                (;;; a. Set env to the running execution context's LexicalEnvironment.
                 (curr-exec-ctx -> LexicalEnvironment))
                (:env)))))
   ;;; 2. Assert: env is an Environment Record.
   ;;; 3. If the source text matched by the syntactic production that is being evaluated is contained in strict mode code,
   ;;;    let strict be true; else let strict be false.
   (strict = true) ; TODO: revisit this
   ;;; 4. Return ? GetIdentifierReference(env, name, strict).
   (return (? (call GetIdentifierReference :env :name :strict)))))

(section
  (:9.5 InitializeHostDefinedRealm ())
  (;;; 1. Let realm be CreateRealm().
   (realm = (call CreateRealm))
   ;;; 2. Let newContext be a new execution context.
   (newContext = record-new)
   ; TODO: have a subroutine to make new execution context properly
   (:newContext LexicalEnvironment <- record-new)
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
  (:10.1.1 OrdinaryObjectInternalMethods_GetPrototypeOf (O))
  (;;; 1. Return ! OrdinaryGetPrototypeOf(O).
   (return (! (call OrdinaryGetPrototypeOf :O)))))

(section
  (:10.1.1.1 OrdinaryGetPrototypeOf (O))
  (;;; 1. Return O.[[Prototype]].
   (return (:O -> Prototype))))

(section
  (:10.1.3 OrdinaryObjectInternalMethods_IsExtensible (O))
  (;;; 1. Return ! OrdinaryIsExtensible(O).
   (return (! (call OrdinaryIsExtensible :O)))))

(section
  (:10.1.3.1 OrdinaryIsExtensible (O))
  (;;; 1. Return O.[[Extensible]].
   (return (:O -> Extensible))))

(section
  (:10.1.5 OrdinaryObjectInternalMethods_GetOwnProperty (O, P))
  (;;; 1. Return ! OrdinaryGetOwnProperty(O, P).
   (return (! (call OrdinaryGetOwnProperty :O :P)))))

(section
  (:10.1.5.1 OrdinaryGetOwnProperty (O, P))
  (;;; 1. If O does not have an own property with key P, return undefined.
   (if (record-absent-prop :O :P)
       ((return undefined)))
   ;;; 2. Let D be a newly created Property Descriptor with no fields.
   (D = record-new)
   ;;; 3. Let X be O's own property whose key is P.
   (X = (:O => :P))
   ;;; 4. If X is a data property, then
   (if (call IsDataDescriptor :X)
       (;;; a. Set D.[[Value]] to the value of X's [[Value]] attribute.
        (:D Value <- (:X -> Value))
        ;;; b. Set D.[[Writable]] to the value of X's [[Writable]] attribute.
        (:D Writable <- (:X -> Writable)))
       ;;; 5. Else,
       (;;; a. Assert: X is an accessor property.
        (assert (call IsAccessorDescriptor :X) "X is an accessor property.")
        ;;; b. Set D.[[Get]] to the value of X's [[Get]] attribute.
        (:D Get <- (:X -> Get))
        ;;; c. Set D.[[Set]] to the value of X's [[Set]] attribute.
        (:D Set <- (:X -> Set))))
   ;;; 6. Set D.[[Enumerable]] to the value of X's [[Enumerable]] attribute.
   (:D Enumerable <- (:X -> Enumerable))
   ;;; 7. Set D.[[Configurable]] to the value of X's [[Configurable]] attribute.
   (:D Configurable <- (:X -> Configurable))
   ;;; 8. Return D.
   (return :D)))

(section
  (:10.1.6 OrdinaryObjectInternalMethods_DefineOwnProperty (O, P, Desc))
  (;;; 1. Return ? OrdinaryDefineOwnProperty(O, P, Desc).
   (return (? (call OrdinaryDefineOwnProperty :O :P :Desc)))))

(section
  (:10.1.6.1 OrdinaryDefineOwnProperty (O, P, Desc))
  (;;; 1. Let current be ? O.[[GetOwnProperty]](P).
   (current = (? (:O .. GetOwnProperty :P)))
   ;;; 2. Let extensible be ? IsExtensible(O).
   (extensible = (? (call IsExtensible :O)))
   ;;; 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
   (return (call ValidateAndApplyPropertyDescriptor :O :P :extensible :Desc :current))))

(section
  (:10.1.7 OrdinaryObjectInternalMethods_HasProperty (O, P))
  (;;; 1. Return ? OrdinaryHasProperty(O, P).
   (return (? (call OrdinaryHasProperty :O :P)))))

(section
  (:10.1.7.1 OrdinaryHasProperty (O, P))
  (;;; 1. Let hasOwn be ? O.[[GetOwnProperty]](P).
   (hasOwn = (? (:O .. GetOwnProperty :P)))
   ;;; 2. If hasOwn is not undefined, return true.
   (if (isnt-undef :hasOwn)
       ((return true)))
   ;;; 3. Let parent be ? O.[[GetPrototypeOf]]().
   (parent = (? (:O .. GetPrototypeOf)))
   ;;; 4. If parent is not null, then
   (if (isnt-null :parent)
       (;;; a. Return ? parent.[[HasProperty]](P).
        (return (? (:parent .. HasProperty :P)))))
   ;;; 5. Return false.
   (return false)))

(section
  (:10.1.8 OrdinaryObjectInternalMethods_Get (O, P, Receiver))
  (;;; 1. Return ? OrdinaryGet(O, P, Receiver).
   (return (? (call OrdinaryGet :O :P :Receiver)))))

(section
  (:10.1.8.1 OrdinaryGet (O, P, Receiver))
  (;;; 1. Let desc be ? O.[[GetOwnProperty]](P).
   (desc = (? (:O .. GetOwnProperty :P)))
   ;;; 2. If desc is undefined, then
   (if (is-undef :desc)
       (;;; a. Let parent be ? O.[[GetPrototypeOf]]().
        (parent = (? (:O .. GetPrototypeOf)))
        ;;; b. If parent is null, return undefined.
        (if (is-null :parent) ((return undefined)))
        ;;; c. Return ? parent.[[Get]](P, Receiver).
        (return (? (:parent .. Get :P :Receiver)))))
   ;;; 3. If IsDataDescriptor(desc) is true, return desc.[[Value]].
   (if (call IsDataDescriptor :desc)
       ((return (:desc -> Value))))
   ;;; 4. Assert: IsAccessorDescriptor(desc) is true.
   (assert (call IsAccessorDescriptor :desc) "IsAccessorDescriptor(desc) is true.")
   ;;; 5. Let getter be desc.[[Get]].
   (getter = (:desc -> Get))
   ;;; 6. If getter is undefined, return undefined.
   (if (is-undef :getter) ((return undefined)))
   ;;; 7. Return ? Call(getter, Receiver).
   (return (? (call Call :getter :Receiver list-new)))))

(section
  (:10.1.9 OrdinaryObjectInternalMethods_Set (O, P, V, Receiver))
  (;;; Return ? OrdinarySet(O, P, V, Receiver).
   (return (? (call OrdinarySet :O :P :V :Receiver)))))

(section
  (:10.1.9.1 OrdinarySet (O, P, V, Receiver))
  (;;; 1. Let ownDesc be ? O.[[GetOwnProperty]](P).
   (ownDesc = (? (:O .. GetOwnProperty :P)))
   ;;; 2. Return OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
   (return (call OrdinarySetWithOwnDescriptor :O :P :V :Receiver :ownDesc))))

(section
  (:10.1.9.2 OrdinarySetWithOwnDescriptor (O, P, V, Receiver, ownDesc))
  (;;; 1. If ownDesc is undefined, then
   (ownDesc =
            (if (is-undef :ownDesc)
                (;;; a. Let parent be ? O.[[GetPrototypeOf]]().
                 (parent = (? (:O .. GetPrototypeOf)))
                 ;;; b. If parent is not null, then
                 (if (isnt-null :parent)
                     (;;; i. Return ? parent.[[Set]](P, V, Receiver).
                      (return (? (:parent .. Set :P :V :Receiver)))
                      (unreachable))
                     ;;; c. Else,
                     (;;; i. Set ownDesc to the PropertyDescriptor { [[Value]]: undefined, [[Writable]]: true,
                      ;;;    [[Enumerable]]: true, [[Configurable]]: true }. 
                      (desc = record-new)
                      (:desc Value <- undefined)
                      (:desc Writable <- true)
                      (:desc Enumerable <- true)
                      (:desc Configurable <- true)
                      (:desc))))
                (:ownDesc)))
   ;;; 2. If IsDataDescriptor(ownDesc) is true, then
   (if (is-true (call IsDataDescriptor :ownDesc))
       (;;; a. If ownDesc.[[Writable]] is false, return false.
        (if (is-false (:ownDesc -> Writable))
            ((return false)))
        ;;; b. If Type(Receiver) is not Object, return false.
        (if (isnt-object :Receiver)
            ((return false)))
        ;;; c. Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
        (existingDescriptor = (? (:Receiver .. GetOwnProperty :P)))
        ;;; d. If existingDescriptor is not undefined, then
        (if (isnt-undef :existingDescriptor)
            (;;; i. If IsAccessorDescriptor(existingDescriptor) is true, return false.
             (if (is-true (call IsAccessorDescriptor :existingDescriptor))
                 ((return false)))
             ;;; ii. If existingDescriptor.[[Writable]] is false, return false.
             (if (is-false (:existingDescriptor -> Writable))
                 ((return false)))
             ;;; iii. Let valueDesc be the PropertyDescriptor { [[Value]]: V }.
             (valueDesc = record-new)
             (:valueDesc Value <- :V)
             ;;; iv. Return ? Receiver.[[DefineOwnProperty]](P, valueDesc).
             (return (? (:Receiver .. DefineOwnProperty :P :valueDesc))))
            ;;; e. Else,
            (;;; i. Assert: Receiver does not currently have a property P.
             (assert (is-false (:Receiver .. HasProperty :P)) "Receiver does not currently have a property P.")
             ;;; ii. Return ? CreateDataProperty(Receiver, P, V).
             (return (? (call CreateDataProperty :Receiver :P :V)))))))
   ;;; 3. Assert: IsAccessorDescriptor(ownDesc) is true.
   (assert (call IsAccessorDescriptor :ownDesc) "IsAccessorDescriptor(ownDesc) is true.")
   ;;; 4. Let setter be ownDesc.[[Set]].
   (setter = :ownDesc -> Set)
   ;;; 5. If setter is undefined, return false.
   (if (is-undef :setter)
       ((return false)))
   ;;; 6. Perform ? Call(setter, Receiver, « V »).
   (_dontCaare = (? (call Call :setter :Receiver (list-new-1 :V))))
   ;;; 7. Return true.
   (return true)))

(section
  (:10.1.12 OrdinaryObjectCreate (proto, additionalInternalSlotsList))
  (;;; 1. Let internalSlotsList be « [[Prototype]], [[Extensible]] ».
   (internalSlotsList = (list-new-2 (atom Prototype) (atom Extensible)))
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
  (:10.2.1 FunctionObject_Call (F, thisArgument, argumentsList))
  (;;; 1. Let callerContext be the running execution context.
   (callerContext = curr-exec-ctx)
   ;;; 2. Let calleeContext be PrepareForOrdinaryCall(F, undefined).
   (calleeContext = (call PrepareForOrdinaryCall :F undefined))
   ;;; 3. Assert: calleeContext is now the running execution context.
   (assert (:calleeContext == curr-exec-ctx) "calleeContext is now the running execution context.")
   ;;; 4. If F.[[IsClassConstructor]] is true, then
   (if (is-true (:F -> IsClassConstructor))
       (;;; a. Let error be a newly created TypeError object.
        ;;; b. NOTE: error is created in calleeContext with F's associated Realm Record.
        (error = (TypeError "function is class constructor"))
        ;;; c. Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
        (exec-ctx-stack-pop)
        (exec-ctx-stack-push :callerContext)
        ;;; d. Return ThrowCompletion(error).
        (return (ThrowCompletion :error))))
   ;;; 5. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
   (call OrdinaryCallBindThis :F :calleeContext :thisArgument)
   ;;; 6. Let result be OrdinaryCallEvaluateBody(F, argumentsList).
   (result = (call OrdinaryCallEvaluateBody :F :argumentsList))
   ;;; 7. Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
   (exec-ctx-stack-pop)
   (exec-ctx-stack-push :callerContext)
   ;;; 8. If result.[[Type]] is return, return NormalCompletion(result.[[Value]]).
   (if (:result -> Type == atom-return)
       ((return (NormalCompletion (:result -> Value)))))
   ;;; 9. ReturnIfAbrupt(result).
   (result = (? :result))
   ;;; 10. Return NormalCompletion(undefined).
   (return (NormalCompletion undefined))))

(section
  (:10.2.1.1 PrepareForOrdinaryCall (F, newTarget))
  (;;; 1. Let callerContext be the running execution context.
   (callerContext = curr-exec-ctx)
   ;;; 2. Let calleeContext be a new ECMAScript code execution context.
   (calleeContext = record-new)
   ;;; 3. Set the Function of calleeContext to F.
   (:calleeContext Function <- :F)
   ;;; 4. Let calleeRealm be F.[[Realm]].
   (calleeRealm = (:F -> Realm))
   ;;; 5. Set the Realm of calleeContext to calleeRealm.
   (:calleeContext Realm <- :calleeRealm)
   ;;; 6. Set the ScriptOrModule of calleeContext to F.[[ScriptOrModule]].
   (:calleeContext ScriptOrModule <- (:F -> ScriptOrModule))
   ;;; 7. Let localEnv be NewFunctionEnvironment(F, newTarget).
   (localEnv = (call NewFunctionEnvironment :F :newTarget))
   ;;; 8. Set the LexicalEnvironment of calleeContext to localEnv.
   (:calleeContext LexicalEnvironment <- :localEnv)
   ;;; 9. Set the VariableEnvironment of calleeContext to localEnv.
   (:calleeContext VariableEnvironment <- :localEnv)
   ;;; 10. Set the PrivateEnvironment of calleeContext to F.[[PrivateEnvironment]].
   (:calleeContext PrivateEnvironment <- (:F -> PrivateEnvironment))
   ;;; 11. If callerContext is not already suspended, suspend callerContext.
   (exec-ctx-stack-pop)
   ;;; 12. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
   (exec-ctx-stack-push :calleeContext)
   ;;; 13. NOTE: Any exception objects produced after this point are associated with calleeRealm.
   ;;; 14. Return calleeContext.
   (return :calleeContext)))

(section
  (:10.2.1.2 OrdinaryCallBindThis (F, calleeContext, thisArgument))
  (;;; 1. Let thisMode be F.[[ThisMode]].
   (thisMode = (:F -> ThisMode))
   ;;; 2. If thisMode is lexical, return NormalCompletion(undefined).
   (if (:thisMode == lexical)
       ((return (NormalCompletion undefined))))
   ;;; 3. Let calleeRealm be F.[[Realm]].
   (calleeRealm = (:F -> Realm))
   ;;; 4. Let localEnv be the LexicalEnvironment of calleeContext.
   (localEnv = (:calleeContext -> LexicalEnvironment))
   ;;; 5. If thisMode is strict, let thisValue be thisArgument.
   (thisValue =
              (expr-block
               ((if (:thisMode == atom-strict)
                    ((:thisArgument))
                    ;;; 6. Else,
                    (;;; a. If thisArgument is undefined or null, then
                     (if ((is-undef :thisArgument) or (is-null :thisArgument))
                         (;;; i. Let globalEnv be calleeRealm.[[GlobalEnv]].
                          (globalEnv = (:calleeRealm -> GlobalEnv))
                          ;;; ii. Assert: globalEnv is a global Environment Record.
                          ;;; iii. Let thisValue be globalEnv.[[GlobalThisValue]].
                          (:globalEnv -> GlobalThisValue))
                         ;;; b. Else,
                         (;;; i. Let thisValue be ! ToObject(thisArgument).
                          ;;; ii. NOTE: ToObject produces wrapper objects using calleeRealm.
                          (! (call ToObject :thisArgument)))))))))
   ;;; 7. Assert: localEnv is a function Environment Record.
   ;;; 8. Assert: The next step never returns an abrupt completion because localEnv.[[ThisBindingStatus]] is not initialized.
   ;;; 9. Return localEnv.BindThisValue(thisValue).
   (return (call BindThisValue :localEnv :thisValue))))

(section
  (:10.2.1.3 EvaluateBody (parseNode, F, argumentsList))
  (; FunctionBody : FunctionStatementList
   (if (is-pn FunctionBody 0)
       (;;; 1. Return ? EvaluateFunctionBody of FunctionBody with arguments functionObject and argumentsList.
        ; all ecmascript ops must implicitly return a completion record
        ; espeically this one
        (return (call EvaluateFunctionBody :parseNode :F :argumentsList))))
   (return (call EvaluateBody (:parseNode -> JSSATParseNodeSlot1) :F :argumentsList))))

(section
  (:10.2.1.4 OrdinaryCallEvaluateBody (F, argumentsList))
  (;;; 1. Return the result of EvaluateBody of the parsed code that is F.[[ECMAScriptCode]] passing F and argumentsList
   ;;;    as the arguments.
   (return (call EvaluateBody (:F -> ECMAScriptCode) :F :argumentsList))))

(section
  (:10.2.2 FunctionObject_Construct (argumentsList, newTarget))
  (;;; 1. Let callerContext be the running execution context.
   ;;; 2. Let kind be F.[[ConstructorKind]].
   ;;; 3. If kind is base, then
   ;;; a. Let thisArgument be ? OrdinaryCreateFromConstructor(newTarget, "%Object.prototype%").
   ;;; 4. Let calleeContext be PrepareForOrdinaryCall(F, newTarget).
   ;;; 5. Assert: calleeContext is now the running execution context.
   ;;; 6. If kind is base, then
   ;;; a. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
   ;;; b. Let initializeResult be InitializeInstanceElements(thisArgument, F).
   ;;; c. If initializeResult is an abrupt completion, then
   ;;; i. Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
   ;;; ii. Return Completion(initializeResult).
   ;;; 7. Let constructorEnv be the LexicalEnvironment of calleeContext.
   ;;; 8. Let result be OrdinaryCallEvaluateBody(F, argumentsList).
   ;;; 9. Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
   ;;; 10. If result.[[Type]] is return, then
   ;;; a. If Type(result.[[Value]]) is Object, return NormalCompletion(result.[[Value]]).
   ;;; b. If kind is base, return NormalCompletion(thisArgument).
   ;;; c. If result.[[Value]] is not undefined, throw a TypeError exception.
   ;;; 11. Else, ReturnIfAbrupt(result).
   ;;; 12. Return ? constructorEnv.GetThisBinding().
   (todo)
   (return unreachable)))

(section
  (:10.2.3 OrdinaryFunctionCreate (functionPrototype, sourceText, ParameterList, Body, thisMode, Scope, PrivateScope))
  (;;; 1. Let internalSlotsList be the internal slots listed in Table 34.
   (internalSlotsList = list-new)
   (inject-table-34 :internalSlotsList)
   ;;; 2. Let F be ! OrdinaryObjectCreate(functionPrototype, internalSlotsList).
   (F = (call OrdinaryObjectCreate :functionPrototype :internalSlotsList))
   ;;; 3. Set F.[[Call]] to the definition specified in 10.2.1.
   (:F Call <- (get-fn-ptr FunctionObject_Call))
   ;;; 4. Set F.[[SourceText]] to sourceText.
   (:F SourceText <- :sourceText)
   ;;; 5. Set F.[[FormalParameters]] to ParameterList.
   (:F FormalParameters <- :ParameterList)
   ;;; 6. Set F.[[ECMAScriptCode]] to Body.
   (:F ECMAScriptCode <- :Body)
   ;;; 7. If the source text matched by Body is strict mode code, let Strict be true; else let Strict be false.
   (Strict = true) ; TODO: strict mode stuff
   ;;; 8. Set F.[[Strict]] to Strict.
   (:F Strict <- :Strict)
   ;;; 9. If thisMode is lexical-this, set F.[[ThisMode]] to lexical.
   (if (:thisMode == lexical-this)
       ((:F ThisMode <- lexical))
       ;;; 10. Else if Strict is true, set F.[[ThisMode]] to strict.
       (elif (is-true :Strict)
             ((:F ThisMode <- atom-strict))
             (;;; 11. Else, set F.[[ThisMode]] to global.
              (:F ThisMode <- atom-global))))
   ;;; 12. Set F.[[IsClassConstructor]] to false.
   (:F IsClassConstructor <- false)
   ;;; 13. Set F.[[Environment]] to Scope.
   (:F Environment <- :Scope)
   ;;; 14. Set F.[[PrivateEnvironment]] to PrivateScope.
   (:F PrivateEnvironment <- :PrivateScope)
   ;;; 15. Set F.[[ScriptOrModule]] to GetActiveScriptOrModule().
   (:F ScriptOrModule <- (call GetActiveScriptOrModule))
   ;;; 16. Set F.[[Realm]] to the current Realm Record.
   (:F Realm <- current-realm)
   ;;; 17. Set F.[[HomeObject]] to undefined.
   (:F HomeObject <- undefined)
   ;;; 18. Set F.[[Fields]] to a new empty List.
   (:F Fields <- list-new)
   ;;; 19. Set F.[[PrivateMethods]] to a new empty List.
   (:F PrivateMethods <- list-new)
   ;;; 20. Set F.[[ClassFieldInitializerName]] to empty.
   (:F ClassFieldInitializerName <- empty)
   ;;; 21. Let len be the ExpectedArgumentCount of ParameterList.
   (len = (call ExpectedArgumentCount :ParameterList))
   ;;; 22. Perform ! SetFunctionLength(F, len).
   (tmp = (call SetFunctionLength :F :len))
   (Perform ! :tmp)
   ;;; 23. Return F.
   (return :F)))

(section
  (:10.2.5 MakeConstructor (F, writablePrototype, prototype))
  (;;; 1. If F is an ECMAScript function object, then
   (if (is-fn-obj :F)
       (;;; a. Assert: IsConstructor(F) is false.
        (assert (is-false (call IsConstructor :F)) "IsConstructor(F) is false.")
        ;;; b. Assert: F is an extensible object that does not have a "prototype" own property.
        ; TODO
        ;;; c. Set F.[[Construct]] to the definition specified in 10.2.2.
        (:F Construct <- (get-fn-ptr FunctionObject_Construct)))
       ;;; 2. Else,
       (;;; a. Set F.[[Construct]] to the definition specified in 10.3.2.
        (todo)))
   ;;; 3. Set F.[[ConstructorKind]] to base.
   (:F ConstructorKind <- atom-base)
   ;;; 4. If writablePrototype is not present, set writablePrototype to true.
   (writablePrototype =
                      (if (is-undef :writablePrototype)
                          ((true))
                          ((:writablePrototype))))
   ;;; 5. If prototype is not present, then
   (prototype =
              (if (is-undef :prototype)
                  (;;; a. Set prototype to ! OrdinaryObjectCreate(%Object.prototype%).
                   (prototype = (! (call OrdinaryObjectCreate null list-new)))
                   ;;; b. Perform ! DefinePropertyOrThrow(prototype, "constructor", PropertyDescriptor {
                   ;;;    [[Value]]: F, [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: true }).
                   (desc = record-new)
                   (:desc Value <- :F)
                   (:desc Writable <- :writablePrototype)
                   (:desc Enumerable <- false)
                   (:desc Configurable <- true)
                   (_dontCare = (! (call DefinePropertyOrThrow :prototype "constructor" :desc)))
                   (:prototype))
                  (:prototype)))
   ;;; 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor { [[Value]]: prototype,
   ;;     [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: false }).
   (desc = record-new)
   (:desc Value <- :prototype)
   (:desc Writable <- :writablePrototype)
   (:desc Enumerable <- false)
   (:desc Configurable <- false)
   (_dontCare = (! (call DefinePropertyOrThrow :F "prototype" :desc)))
   ;;; 7. Return NormalCompletion(undefined).
   (return (NormalCompletion undefined))))

(section
  (:10.2.9 SetFunctionName (F, name, prefix))
  (;;; 1. Assert: F is an extensible object that does not have a "name" own property.
   ;;; 2. If Type(name) is Symbol, then
   (name =
         (expr-block
          ((if (is-symbol :name)
               (;;; a. Let description be name's [[Description]] value.
                (description = (:name -> Description))
                ;;; b. If description is undefined, set name to the empty String.
                (if (is-undef :description)
                    ("")
                    ;;; c. Else, set name to the string-concatenation of "[", description, and "]".
                    (; TODO: string concat
                     :description)))
               ;;; 3. Else if name is a Private Name, then
               (elif (false)
                     (;;; a. Set name to name.[[Description]].
                      (:name -> Description))
                     (:name))))))
   ;;; 4. If F has an [[InitialName]] internal slot, then
   (if (record-has-slot :F InitialName)
       (;;; a. Set F.[[InitialName]] to name.
        (:F InitialName <- :name)))
   ;;; 5. If prefix is present, then
   (name =
         (expr-block
          ((if (isnt-undef :prefix)
               (;;; a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and name.
                ; TODO: string concatenation
                (nameTemp = :name)
                ;;; b. If F has an [[InitialName]] internal slot, then
                (if (record-has-slot :F InitialName)
                    (;;; i. Optionally, set F.[[InitialName]] to name.
                     (:F InitialName <- :nameTemp)))
                (:nameTemp))
               (:name)))))
   ;;; 6. Return ! DefinePropertyOrThrow(F, "name", PropertyDescriptor { [[Value]]: name, [[Writable]]: false, [[Enumerable]]: false,
   ;;;                                              [[Configurable]]: true }).
   (propDesc = record-new)
   (:propDesc Value <- :name)
   (:propDesc Writable <- false)
   (:propDesc Enumerable <- false)
   (:propDesc Configurable <- true)
   (return (! (call DefinePropertyOrThrow :F "name" :propDesc)))))

(section
  (:10.2.10 SetFunctionLength (F, length))
  (;;; 1. Assert: F is an extensible object that does not have a "length" own property.
   ;;; 2. Return ! DefinePropertyOrThrow(F, "length", PropertyDescriptor { [[Value]]: 𝔽(length), [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }).
   (propDesc = record-new)
   (:propDesc Value <- :length)
   (:propDesc Writable <- false)
   (:propDesc Enumerable <- false)
   (:propDesc Configurable <- true)
   (return (! (call DefinePropertyOrThrow :F "length" :propDesc)))))

(section
  (:10.2.11 FunctionDeclarationInstantiation (func, argumentsList))
  (;;; 1. Let calleeContext be the running execution context.
   (calleeContext = curr-exec-ctx)
   ;;; 2. Let code be func.[[ECMAScriptCode]].
   (code = :func -> ECMAScriptCode)
   ;;; 3. Let strict be func.[[Strict]].
   (strict = :func -> Strict)
   ;;; 4. Let formals be func.[[FormalParameters]].
   (formals = :func -> FormalParameters)
   ;;; 5. Let parameterNames be the BoundNames of formals.
   (parameterNames = (call BoundNames :formals))
   ;;; 6. If parameterNames has any duplicate entries, let hasDuplicates be true. Otherwise, let hasDuplicates be false.
   (hasDuplicates = (list-has-duplicates :parameterNames))
   ;;; 7. Let simpleParameterList be IsSimpleParameterList of formals.
   (simpleParameterList = (call IsSimpleParameterList :formals))
   ;;; 8. Let hasParameterExpressions be ContainsExpression of formals.
   (hasParameterExpressions = (call ContainsExpression :formals))
   ;;; 9. Let varNames be the VarDeclaredNames of code.
   (varNames = (call VarDeclaredNames :code))
   ;;; 10. Let varDeclarations be the VarScopedDeclarations of code.
   (varDeclarations = (call VarScopedDeclarations :code))
   ;;; 11. Let lexicalNames be the LexicallyDeclaredNames of code.
   (lexicalNames = (call LexicallyDeclaredNames :code))
   ;;; 12. Let functionNames be a new empty List.
   (functionNames = list-new)
   ;;; 13. Let functionsToInitialize be a new empty List.
   (functionsToInitialize = list-new)
   ;;; 14. For each element d of varDeclarations, in reverse List order, do
   (for :varDeclarations
        ((d = for-item-rev)
         ;;; a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
         (if (and3 (pn-kind-isnt :d VariableDeclaration) (pn-kind-isnt :d ForBinding) (pn-kind-isnt :d BindingIdentifier))
             (;;; i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
              (assert (or4
                       (pn-kind-is :d FunctionDeclaration)
                       (pn-kind-is :d GeneratorDeclaration)
                       (pn-kind-is :d AsyncFunctionDeclaration)
                       (pn-kind-is :d AsyncGeneratorDeclaration))
                      "d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.")
              ;;; ii. Let fn be the sole element of the BoundNames of d.
              (fn = (sole-element (call BoundNames :d)))
              ;;; iii. If fn is not an element of functionNames, then
              (if (not (list-contains :functionNames :fn))
                  (;;; 1. Insert fn as the first element of functionNames.
                   (list-insert-front :functionNames :fn)
                   ;;; 2. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
                   ;;; 3. Insert d as the first element of functionsToInitialize.
                   (list-insert-front :functionsToInitialize :d)))))))
   ;;; 15. Let argumentsObjectNeeded be true.
   (argumentsObjectNeeded = true)
   (argumentsObjectNeeded =
                          ;;; 16. If func.[[ThisMode]] is lexical, then
                          (if ((:func -> ThisMode) == lexical)
                              (;;; a. NOTE: Arrow functions never have an arguments object.
                               ;;; b. Set argumentsObjectNeeded to false.
                               (false))
                              ;;; 17. Else if "arguments" is an element of parameterNames, then
                              (elif (list-contains :parameterNames "arguments")
                                    (;;; a. Set argumentsObjectNeeded to false.
                                     (false))
                                    ;;; 18. Else if hasParameterExpressions is false, then
                                    (elif (is-false :hasParameterExpressions)
                                          (;;; a. If "arguments" is an element of functionNames or if "arguments" is an element of lexicalNames, then
                                           (if ((list-contains :functionNames "arguments") or (list-contains :lexicalNames "arguments"))
                                               (;;; i. Set argumentsObjectNeeded to false.
                                                (false))
                                               ((:argumentsObjectNeeded))))
                                          ((:argumentsObjectNeeded))))))
   ;;; 19. If strict is true or if hasParameterExpressions is false, then
   (env =
        (if ((is-true :strict) or (is-false :hasParameterExpressions))
            (;;; a. NOTE: Only a single Environment Record is needed for the parameters, since calls to eval in strict mode code cannot create new bindings which are visible outside of the eval.
             ;;; b. Let env be the LexicalEnvironment of calleeContext.
             (:calleeContext -> LexicalEnvironment))
            ;;; 20. Else,
            (;;; a. NOTE: A separate Environment Record is needed to ensure that bindings created by direct eval calls in the formal parameter list are outside the environment where parameters are declared.
             ;;; b. Let calleeEnv be the LexicalEnvironment of calleeContext.
             (calleeEnv = :calleeContext -> LexicalEnvironment)
             ;;; c. Let env be NewDeclarativeEnvironment(calleeEnv).
             (env = (call NewDeclarativeEnvironment :calleeEnv))
             ;;; d. Assert: The VariableEnvironment of calleeContext is calleeEnv.
             (assert ((:calleeContext -> VariableEnvironment) == :calleeEnv) "The VariableEnvironment of calleeContext is calleeEnv.")
             ;;; e. Set the LexicalEnvironment of calleeContext to env.
             (:calleeContext LexicalEnvironment <- :env)
             (:env))))
   ;;; 21. For each String paramName of parameterNames, do
   (for :parameterNames
        ((paramName = for-item)
         ;;; a. Let alreadyDeclared be env.HasBinding(paramName).
         (alreadyDeclared = (:env .. HasBinding :paramName))
         ;;; b. NOTE: Early errors ensure that duplicate parameter names can only occur in non-strict functions that do not have parameter default values or rest parameters.
         ;;; c. If alreadyDeclared is false, then
         (if (is-false :alreadyDeclared)
             (;;; i. Perform ! env.CreateMutableBinding(paramName, false).
              (_dontCare = (! (:env .. CreateMutableBinding :paramName false)))
              ;;; ii. If hasDuplicates is true, then
              (if (is-true :hasDuplicates)
                  (;;; 1. Perform ! env.InitializeBinding(paramName, undefined).
                   (_dontCare = (! (:env .. InitializeBinding :paramName undefined)))))))))
   ;;; 22. If argumentsObjectNeeded is true, then
   (parameterBindings =
                      (if (is-true :argumentsObjectNeeded)
                          (;;; a. If strict is true or if simpleParameterList is false, then
                           (ao =
                               (if ((is-true :strict) or (is-false :simpleParameterList))
                                   (;;; i. Let ao be CreateUnmappedArgumentsObject(argumentsList).
                                    (call CreateUnmappedArgumentsObject :argumentsList))
                                   ;;; b. Else,
                                   (;;; i. NOTE: A mapped argument object is only provided for non-strict functions that don't have a rest parameter, any parameter default value initializers, or any destructured parameters.
                                    ;;; ii. Let ao be CreateMappedArgumentsObject(func, formals, argumentsList, env).
                                    (call CreateMappedArgumentsObject :func :formals :argumentsList :env))))
                           ;;; c. If strict is true, then
                           (if (is-true :strict)
                               (;;; i. Perform ! env.CreateImmutableBinding("arguments", false).
                                (_dontCare = (! (:env .. CreateImmutableBinding "arguments" false))))
                               ;;; d. Else,
                               (;;; i. Perform ! env.CreateMutableBinding("arguments", false).
                                (_dontCare = (! (:env .. CreateMutableBinding "arguments" false)))))
                           ;;; e. Call env.InitializeBinding("arguments", ao).
                           (:env .. InitializeBinding "arguments" :ao)
                           ;;; f. Let parameterBindings be the list-concatenation of parameterNames and « "arguments" ».
                           (list-concat :parameterNames (list-new-1 "arguments")))
                          ;;; 23. Else,
                          (;;; a. Let parameterBindings be parameterNames.
                           (:parameterNames))))
   ;  ;;; 24. Let iteratorRecord be CreateListIteratorRecord(argumentsList).
   ;  (iteratorRecord = (call CreateListIteratorRecord :argumentsList))
   ;;; 25. If hasDuplicates is true, then
   (iteratorRecord = (list-clone :argumentsList)) ; not implementing yield yet lmao
   (if (is-true :hasDuplicates)
       (;;; a. Perform ? IteratorBindingInitialization for formals with iteratorRecord and undefined as arguments.
        (_dontCare = (? (call IteratorBindingInitialization :formals :iteratorRecord undefined))))
       ;;; 26. Else,
       (;;; a. Perform ? IteratorBindingInitialization for formals with iteratorRecord and env as arguments.
        (_dontCare = (? (call IteratorBindingInitialization :formals :iteratorRecord :env)))))
   ;;; 27. If hasParameterExpressions is false, then
   (varEnv =
           (if (is-false :hasParameterExpressions)
               (;;; a. NOTE: Only a single Environment Record is needed for the parameters and top-level vars.
                ;;; b. Let instantiatedVarNames be a copy of the List parameterBindings.
                (instantiatedVarNames = (list-clone :parameterBindings))
                ;;; c. For each element n of varNames, do
                (for :varNames
                     ((n = for-item)
                      ;;; i. If n is not an element of instantiatedVarNames, then
                      (if (not (list-contains :instantiatedVarNames :n))
                          (;;; 1. Append n to instantiatedVarNames.
                           (list-push :instantiatedVarNames :n)
                           ;;; 2. Perform ! env.CreateMutableBinding(n, false).
                           (_dontCare = (! (:env .. CreateMutableBinding :n false)))
                           ;;; 3. Call env.InitializeBinding(n, undefined).
                           (:env .. InitializeBinding :n undefined)))))
                ;;; d. Let varEnv be env.
                (:env))
               ;;; 28. Else,
               (;;; a. NOTE: A separate Environment Record is needed to ensure that closures created by expressions in the formal parameter list do not have visibility of declarations in the function body.
                ;;; b. Let varEnv be NewDeclarativeEnvironment(env).
                (varEnv = (call NewDeclarativeEnvironment :env))
                ;;; c. Set the VariableEnvironment of calleeContext to varEnv.
                (:calleeContext VariableEnvironment <- :varEnv)
                ;;; d. Let instantiatedVarNames be a new empty List.
                (instantiatedVarNames = list-new)
                ;;; e. For each element n of varNames, do
                (for :varNames
                     ((n = for-item)
                      ;;; i. If n is not an element of instantiatedVarNames, then
                      (if (not (list-contains :instantiatedVarNames :n))
                          (;;; 1. Append n to instantiatedVarNames.
                           (list-push :instantiatedVarNames :n)
                           ;;; 2. Perform ! varEnv.CreateMutableBinding(n, false).
                           (_dontCare = (! (:varEnv .. CreateMutableBinding :n false)))
                           ;;; 3. If n is not an element of parameterBindings or if n is an element of functionNames, let initialValue be undefined.
                           (initialValue =
                                         (if ((not (list-contains :parameterBindings :n)) or (list-contains :functionNames :n))
                                             ((undefined))
                                             ;;; 4. Else,
                                             (;;; a. Let initialValue be ! env.GetBindingValue(n, false).
                                              (! (:env .. GetBindingValue :n false)))))
                           ;;; 5. Call varEnv.InitializeBinding(n, initialValue).
                           (:varEnv .. InitializeBinding :n :initialValue)
                           ;;; 6. NOTE: A var with the same name as a formal parameter initially has the same value as the
                           ;;;    corresponding initialized parameter.
                          ))))
                (:varEnv))))
   ;;; 29. NOTE: Annex B.3.2.1 adds additional steps at this point.
   ;;; 30. If strict is false, then
   (lexEnv =
           (if (is-false :strict)
               (;;; a. Let lexEnv be NewDeclarativeEnvironment(varEnv).
                ;;; b. NOTE: Non-strict functions use a separate Environment Record for top-level lexical declarations so that a direct eval can determine whether any var scoped declarations introduced by the eval code conflict with pre-existing top-level lexically scoped declarations. This is not needed for strict functions because a strict direct eval always places all declarations into a new Environment Record.
                (call NewDeclarativeEnvironment :varEnv))
               ;;; 31. Else, let lexEnv be varEnv.
               (:varEnv)))
   ;;; 32. Set the LexicalEnvironment of calleeContext to lexEnv.
   (:calleeContext LexicalEnvironment <- :lexEnv)
   ;;; 33. Let lexDeclarations be the LexicallyScopedDeclarations of code.
   (lexDeclarations = (call LexicallyScopedDeclarations :code))
   ;;; 34. For each element d of lexDeclarations, do
   (for :lexDeclarations
        ((d = for-item)
         ;;; a. NOTE: A lexically declared name cannot be the same as a function/generator declaration, formal parameter, or a var name. Lexically declared names are only instantiated here but not initialized.
         ;;; b. For each element dn of the BoundNames of d, do
         (boundNames = (call BoundNames :d))
         (for :boundNames
              ((dn = for-item)
               ;;; i. If IsConstantDeclaration of d is true, then
               (if (false)
                   ; (is-true (call IsConstantDeclaration :d))
                   (;;; 1. Perform ! lexEnv.CreateImmutableBinding(dn, true).
                    (_dontCare = (! (:lexEnv .. CreateImmutableBinding :dn true))))
                   ;;; ii. Else,
                   (;;; 1. Perform ! lexEnv.CreateMutableBinding(dn, false).
                    (_dontCare = (! (:lexEnv .. CreateMutableBinding :dn false)))))))))
   ;;; 35. Let privateEnv be the PrivateEnvironment of calleeContext.
   (privateEnv = :calleeContext -> PrivateEnvironment)
   ;;; 36. For each Parse Node f of functionsToInitialize, do
   (for :functionsToInitialize
        ((f = for-item)
         ;;; a. Let fn be the sole element of the BoundNames of f.
         (fn = (sole-element (call BoundNames :f)))
         ;;; b. Let fo be InstantiateFunctionObject of f with arguments lexEnv and privateEnv.
         (fo = (call InstantiateFunctionObject :f :lexEnv :privateEnv))
         ;;; c. Perform ! varEnv.SetMutableBinding(fn, fo, false).
         (_dontCare = (! (:varEnv .. SetMutableBinding :fn :fo false)))))
   ;;; 37. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))

(section
  (:10.3.3 CreateBuiltinFunction (behaviour, length, name, additionalInternalSlotsList, realm, prototype, prefix))
  (;;; 1. If realm is not present, set realm to the current Realm Record.
   (realm = (expr-block
             ((if (is-undef :realm)
                  ((curr-exec-ctx -> Realm))
                  (:realm)))))
   ;;; 2. If prototype is not present, set prototype to realm.[[Intrinsics]].[[%Function.prototype%]].
   (prototype = (expr-block
                 ((if (is-undef :prototype)
                      (record-new)
                      ; TODO
                      (:prototype)))))
   ;;; 3. Let internalSlotsList be a List containing the names of all the internal slots that 10.3 requires for the built-in
   ;;;    function object that is about to be created.
   (internalSlotsList = list-new)
   (list-push :internalSlotsList (atom Prototype))
   (list-push :internalSlotsList (atom Extensible))
   (inject-table-34 :internalSlotsList)
   (list-push :internalSlotsList (atom InitialName))
   ;;; 4. Append to internalSlotsList the elements of additionalInternalSlotsList.
   (for :additionalInternalSlotsList
        ((list-push :internalSlotsList for-item)))
   ;;; 5. Let func be a new built-in function object that, when called, performs the action described by behaviour using
   ;;;    the provided arguments as the values of the corresponding parameters specified by behaviour. The new function
   ;;;    object has internal slots whose names are the elements of internalSlotsList, and an [[InitialName]] internal
   ;;;    slot.
   ; (! (MakeBasicObject)) determined to be suitable here
   ; because engine262 does this
   (func = (! (call MakeBasicObject :internalSlotsList)))
   (:func InitialName <- undefined) ; this must be present i guess
   (:func Call <- :behaviour)
   ;;; 6. Set func.[[Prototype]] to prototype.
   (:func Prototype <- :prototype)
   ;;; 7. Set func.[[Extensible]] to true.
   (:func Extensible <- true)
   ;;; 8. Set func.[[Realm]] to realm.
   (:func Realm <- :realm)
   ;;; 9. Set func.[[InitialName]] to null.
   (:func InitialName <- null)
   ;;; 10. Perform ! SetFunctionLength(func, length).
   (Perform ! (call SetFunctionLength :func :length))
   ;;; 11. If prefix is not present, then
   (if (is-undef :prefix)
       (;;; a. Perform ! SetFunctionName(func, name).
        (Perform ! (call SetFunctionName :func :name undefined)))
       ;;; 12. Else,
       (;;; a. Perform ! SetFunctionName(func, name, prefix).
        (Perform ! (call SetFunctionName :func :name :prefix))))
   ;;; 13. Return func.
   (return :func)))

(section
  (:10.4.4.6 CreateUnmappedArgumentsObject (argumentsList))
  (;;; 1. Let len be the number of elements in argumentsList.
   (len = (list-len :argumentsList))
   ;;; 2. Let obj be ! OrdinaryObjectCreate(%Object.prototype%, « [[ParameterMap]] »).
   (obj = (! (call OrdinaryObjectCreate null (list-new-1 (atom ParameterMap)))))
   ;;; 3. Set obj.[[ParameterMap]] to undefined.
   (:obj ParameterMap <- undefined)
   ;;; 4. Perform DefinePropertyOrThrow(obj, "length", PropertyDescriptor { [[Value]]: 𝔽(len), [[Writable]]: true,
   ;;;    [[Enumerable]]: false, [[Configurable]]: true }).
   (desc = record-new)
   (:desc Value <- :len)
   (:desc Writable <- true)
   (:desc Enumerable <- false)
   (:desc Configurable <- true)
   (call DefinePropertyOrThrow :obj "length" :desc)
   ;;; 5. Let index be 0.
   ;;; 6. Repeat, while index < len,
   (for :argumentsList
        (;;; a. Let val be argumentsList[index].
         (val = for-item)
         ;;; b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
         (_dontCare = (! (call CreateDataPropertyOrThrow :obj (! (call ToString :jssat_i)) :val)))
         ;;; c. Set index to index + 1.
        ))
   ;;; 7. Perform ! DefinePropertyOrThrow(obj, @@iterator, PropertyDescriptor { [[Value]]: %Array.prototype.values%,
   ;;;    [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }).
   ;;; 8. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor { [[Get]]: %ThrowTypeError%, [[Set]]: %ThrowTypeError%, [[Enumerable]]: false, [[Configurable]]: false }).
   ;;; 9. Return obj.
   (return :obj)))

(section
  (:10.4.4.7 CreateMappedArgumentsObject (func, formals, argumentsList, env))
  (;;; 1. Assert: formals does not contain a rest parameter, any binding patterns, or any initializers. It may contain duplicate identifiers.
   ;;; 2. Let len be the number of elements in argumentsList.
   ;;; 3. Let obj be ! MakeBasicObject(« [[Prototype]], [[Extensible]], [[ParameterMap]] »).
   ;;; 4. Set obj.[[GetOwnProperty]] as specified in 10.4.4.1.
   ;;; 5. Set obj.[[DefineOwnProperty]] as specified in 10.4.4.2.
   ;;; 6. Set obj.[[Get]] as specified in 10.4.4.3.
   ;;; 7. Set obj.[[Set]] as specified in 10.4.4.4.
   ;;; 8. Set obj.[[Delete]] as specified in 10.4.4.5.
   ;;; 9. Set obj.[[Prototype]] to %Object.prototype%.
   ;;; 10. Let map be ! OrdinaryObjectCreate(null).
   ;;; 11. Set obj.[[ParameterMap]] to map.
   ;;; 12. Let parameterNames be the BoundNames of formals.
   ;;; 13. Let numberOfParameters be the number of elements in parameterNames.
   ;;; 14. Let index be 0.
   ;;; 15. Repeat, while index < len,
   ;;; a. Let val be argumentsList[index].
   ;;; b. Perform ! CreateDataPropertyOrThrow(obj, ! ToString(𝔽(index)), val).
   ;;; c. Set index to index + 1.
   ;;; 16. Perform ! DefinePropertyOrThrow(obj, "length", PropertyDescriptor { [[Value]]: 𝔽(len), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }).
   ;;; 17. Let mappedNames be a new empty List.
   ;;; 18. Set index to numberOfParameters - 1.
   ;;; 19. Repeat, while index ≥ 0,
   ;;; a. Let name be parameterNames[index].
   ;;; b. If name is not an element of mappedNames, then
   ;;; i. Add name as an element of the list mappedNames.
   ;;; ii. If index < len, then
   ;;; 1. Let g be MakeArgGetter(name, env).
   ;;; 2. Let p be MakeArgSetter(name, env).
   ;;; 3. Perform map.[[DefineOwnProperty]](! ToString(𝔽(index)), PropertyDescriptor { [[Set]]: p, [[Get]]: g, [[Enumerable]]: false, [[Configurable]]: true }).
   ;;; c. Set index to index - 1.
   ;;; 20. Perform ! DefinePropertyOrThrow(obj, @@iterator, PropertyDescriptor { [[Value]]: %Array.prototype.values%, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }).
   ;;; 21. Perform ! DefinePropertyOrThrow(obj, "callee", PropertyDescriptor { [[Value]]: func, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }).
   ;;; 22. Return obj.
   (assert false "TODO: CreateMappedArgumentsObject")
   (return unreachable)))

; 13.1.2 `StringValue`
(def
  (StringValueOfBindingIdentifier :parseNode)
  ; BindingIdentifier : Identifier (0)
  (StringValueOfIdentifier (:parseNode -> JSSATParseNodeSlot1)))

(def
  (StringValueOfIdentifier :parseNode)
  (:parseNode -> JSSATParseNode_Identifier_StringValue))

(section
  (:13.1.3 Evaluation_IdentifierReference (parseNode))
  (; IdentifierReference : Identifier
   (if (is-pn IdentifierReference 0)
       (;;; 1. Return ? ResolveBinding(StringValue of Identifier).
        (ret-comp (? (call ResolveBinding (StringValueOfIdentifier (:parseNode -> JSSATParseNodeSlot1)) undefined)))))
   ; IdentifierReference : yield
   (if (is-pn IdentifierReference 1)
       (;;; 1. Return ? ResolveBinding("yield").
        (ret-comp (? (call ResolveBinding "yield" undefined)))))
   ; IdentifierReference : await
   (if (is-pn IdentifierReference 2)
       (;;; 1. Return ? ResolveBinding("await").
        (ret-comp (? (call ResolveBinding "await" undefined)))))
   (return unreachable)))

(section
  (:13.2.3.1 Evaluation_Literal (parseNode))
  (; Literal : NullLiteral
   (if (is-pn Literal 0)
       (;;; 1. Return null.
        (return null)))
   ; Literal : BooleanLiteral
   (if (is-pn Literal 1)
       (;;; 1. If BooleanLiteral is the token false, return false.
        (todo)
        ;;; 2. If BooleanLiteral is the token true, return true.
       ))
   ; Literal : NumericLiteral
   (if (is-pn Literal 2)
       (;;; 1. Return the NumericValue of NumericLiteral as defined in 12.8.3.
        (todo)))
   ; Literal : StringLiteral
   (if (is-pn Literal 3)
       (;;; 1. Return the SV of StringLiteral as defined in 12.8.4.2.
        (return (:parseNode -> JSSATParseNode_StringLiteral_StringValue))))
   (return unreachable)))

(section
  (:13.3.6.1 Evaluation_CallExpression (parseNode))
  (; CallExpression : CoverCallExpressionAndAsyncArrowHead
   (if (is-pn CallExpression 0)
       (;;; 1. Let expr be the CallMemberExpression that is covered by CoverCallExpressionAndAsyncArrowHead.
        (expr = (:parseNode -> JSSATParseNodeSlot1 -> JSSATParseNodeSlot3))
        ;;; 2. Let memberExpr be the MemberExpression of expr.
        (memberExpr = (:expr -> JSSATParseNodeSlot1))
        ;;; 3. Let arguments be the Arguments of expr.
        (arguments = (:expr -> JSSATParseNodeSlot2))
        ;;; 4. Let ref be the result of evaluating memberExpr.
        (ref = (evaluating :memberExpr))
        ;;; 5. Let func be ? GetValue(ref).
        (func = (? (call GetValue :ref)))
        ;;; 6. If ref is a Reference Record, IsPropertyReference(ref) is false, and ref.[[ReferencedName]] is "eval", then
        (if (lazyAnd (is-reference-record :ref) (lazyAnd (is-false (call IsPropertyReference :ref)) ((:ref -> ReferencedName) == "eval")))
            (;;; a. If SameValue(func, %eval%) is true, then
             (todo)
             ;;; i. Let argList be ? ArgumentListEvaluation of arguments.
             ;;; ii. If argList has no elements, return undefined.
             ;;; iii. Let evalArg be the first element of argList.
             ;;; iv. If the source text matched by this CallExpression is strict mode code, let strictCaller be true. Otherwise let strictCaller be false.
             ;;; v. Let evalRealm be the current Realm Record.
             ;;; vi. Return ? PerformEval(evalArg, evalRealm, strictCaller, true).
            ))
        ;;; 7. Let thisCall be this CallExpression.
        (thisCall = :parseNode)
        ;;; 8. Let tailCall be IsInTailPosition(thisCall).
        (tailCall = false)
        ;;; 9. Return ? EvaluateCall(func, ref, arguments, tailCall).
        (return (call EvaluateCall :func :ref :arguments :tailCall))))
   ; CallExpression : CallExpression Arguments
   (if (is-pn CallExpression 3)
       (;;; 1. Let ref be the result of evaluating CallExpression.
        (ref = (call Evaluation_CallExpression (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let func be ? GetValue(ref).
        (func = (? (call GetValue :ref)))
        ;;; 3. Let thisCall be this CallExpression.
        (thisCall = :parseNode)
        ;;; 4. Let tailCall be IsInTailPosition(thisCall).
        (tailCall = false) ; lol im not implementing that
        ;;; 5. Return ? EvaluateCall(func, ref, Arguments, tailCall).
        (return (call EvaluateCall :func :ref (:parseNode -> JSSATParseNodeSlot2) :tailCall))))
   (return unreachable)))

(section
  (:13.3.6.2 EvaluateCall (func, ref, arguments, tailPosition))
  (;;; 1. If ref is a Reference Record, then
   (thisValue =
              (expr-block
               ((if (is-reference-record :ref)
                    (;;; a. If IsPropertyReference(ref) is true, then
                     (if (is-true (call IsPropertyReference :ref))
                         (;;; i. Let thisValue be GetThisValue(ref).
                          (call GetThisValue :ref))
                         ;;; b. Else,
                         (;;; i. Let refEnv be ref.[[Base]].
                          (refEnv = (:ref -> Base))
                          ;;; ii. Assert: refEnv is an Environment Record.
                          ;;; iii. Let thisValue be refEnv.WithBaseObject().
                          (:refEnv .. WithBaseObject))))

                    ;;; 2. Else,
                    (;;; a. Let thisValue be undefined.
                     (undefined))))))
   ;;; 3. Let argList be ? ArgumentListEvaluation of arguments.
   (argList = (? (call ArgumentListEvaluation :arguments)))
   ;;; 4. If Type(func) is not Object, throw a TypeError exception.
   (if (isnt-object :func)
       ((throw (TypeError "not an object :("))))
   ;;; 5. If IsCallable(func) is false, throw a TypeError exception.
   (if (is-false (call IsCallable :func))
       ((throw (TypeError ":(((((((((("))))
   ;;; 6. If tailPosition is true, perform PrepareForTailCall().
   (if :tailPosition
       ((call PrepareForTailCall)))
   ;;; 7. Let result be Call(func, thisValue, argList).
   (result = (? (call Call :func :thisValue :argList)))
   ;;; 8. Assert: If result is not an abrupt completion, then Type(result) is an ECMAScript language type.
   ;;; 9. Return result.
   (ret-comp :result)))

; NOTE: for some reason this is dual-purposed as a Evaluation_ArgumentList too
(section
  (:13.3.8.1 ArgumentListEvaluation (parseNode))
  (; Arguments : ( )
   (if (is-pn Arguments 0)
       (;;; 1. Return a new empty List.
        (return list-new)))
   ; ArgumentList : AssignmentExpression
   (if (is-pn ArgumentList 0)
       (;;; 1. Let ref be the result of evaluating AssignmentExpression.
        (ref = (evaluating (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let arg be ? GetValue(ref).
        (arg = (? (call GetValue :ref)))
        ;;; 3. Return a List whose sole element is arg.
        (return (list-new-1 :arg))))
   ; ArgumentList : ... AssignmentExpression
   (if (is-pn ArgumentList 1)
       (;;; 1. Let list be a new empty List.
        (list = list-new)
        ;;; 2. Let spreadRef be the result of evaluating AssignmentExpression.
        (spreadRef = (evaluating (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 3. Let spreadObj be ? GetValue(spreadRef).
        (spreadObj = (? (call GetValue :spreadRef)))
        ;;; 4. Let iteratorRecord be ? GetIterator(spreadObj).
        (iteratorRecord = (? (call GetIterator :spreadObj undefined undefined)))
        ;;; 5. Repeat,
        (loop () (true) ()
              (;;; a. Let next be ? IteratorStep(iteratorRecord).
               (next = (? (call IteratorStep :iteratorRecord)))
               ;;; b. If next is false, return list.
               (if (is-false :next)
                   ((return :list)))
               ;;; c. Let nextArg be ? IteratorValue(next).
               (nextArg = (? (call IteratorValue :next)))
               ;;; d. Append nextArg as the last element of list.
               (list-push :list :nextArg)))))
   ; ArgumentList : ArgumentList , AssignmentExpression
   (if (is-pn ArgumentList 2)
       (;;; 1. Let precedingArgs be ? ArgumentListEvaluation of ArgumentList.
        (precedingArgs = (? (call ArgumentListEvaluation (:parseNode -> JSSATParseNodeSlot1))))
        ;;; 2. Let ref be the result of evaluating AssignmentExpression.
        (ref = (evaluating (:parseNode -> JSSATParseNodeSlot2)))
        ;;; 3. Let arg be ? GetValue(ref).
        (arg = (? (call GetValue :ref)))
        ;;; 4. Return the list-concatenation of precedingArgs and « arg ».
        (return (list-concat :precedingArgs (list-new-1 :arg)))))
   ; ArgumentList : ArgumentList , ... AssignmentExpression
   (if (is-pn ArgumentList 3)
       (;;; 1. Let precedingArgs be ? ArgumentListEvaluation of ArgumentList.
        (precedingArgs = (? (call ArgumentListEvaluation (:parseNode -> JSSATParseNodeSlot1))))
        ;;; 2. Let spreadRef be the result of evaluating AssignmentExpression.
        (spreadRef = (evaluating (:parseNode -> JSSATParseNodeSlot2)))
        ;;; 3. Let iteratorRecord be ? GetIterator(? GetValue(spreadRef)).
        (iteratorRecord = (? (call GetIterator (? (call GetValue :spreadRef)) undefined undefined)))
        ;;; 4. Repeat,
        (loop () (true) ()
              (;;; a. Let next be ? IteratorStep(iteratorRecord).
               (next = (? (call IteratorStep :iteratorRecord)))
               ;;; b. If next is false, return precedingArgs.
               (if (is-false :next)
                   ((return :precedingArgs)))
               ;;; c. Let nextArg be ? IteratorValue(next).
               (nextArg = (? (call IteratorValue :next)))
               ;;; d. Append nextArg as the last element of precedingArgs.
               (list-push :precedingArgs :nextArg)))))
   (return (chainEval :parseNode))))

(section
  (:14.2.2 StatementList_Evaluation (parseNode))
  (; StatementList : StatementList StatementListItem
   ;;; 1. Let sl be the result of evaluating StatementList.
   (sl = (evaluating (:parseNode -> JSSATParseNodeSlot1)))
   ;;; 2. ReturnIfAbrupt(sl).
   (sl = (? :sl))
   ;;; 3. Let s be the result of evaluating StatementListItem.
   (s = (evaluating (:parseNode -> JSSATParseNodeSlot2)))
   ;;; 4. Return Completion(UpdateEmpty(s, sl)).
   (return (call UpdateEmpty :s :sl))))

(section
  (:15.1.2 ContainsExpression (parseNode))
  (; ObjectBindingPattern :
   ; { }
   ; { BindingRestProperty }
   ;;; 1. Return false.
   ; ObjectBindingPattern : { BindingPropertyList , BindingRestProperty }
   ;;; 1. Return ContainsExpression of BindingPropertyList.
   ; ArrayBindingPattern : [ Elisionopt ]
   ;;; 1. Return false.
   ; ArrayBindingPattern : [ Elisionopt BindingRestElement ]
   ;;; 1. Return ContainsExpression of BindingRestElement.
   ; ArrayBindingPattern : [ BindingElementList , Elisionopt ]
   ;;; 1. Return ContainsExpression of BindingElementList.
   ; ArrayBindingPattern : [ BindingElementList , Elisionopt BindingRestElement ]
   ;;; 1. Let has be ContainsExpression of BindingElementList.
   ;;; 2. If has is true, return true.
   ;;; 3. Return ContainsExpression of BindingRestElement.
   ; BindingPropertyList : BindingPropertyList , BindingProperty
   ;;; 1. Let has be ContainsExpression of BindingPropertyList.
   ;;; 2. If has is true, return true.
   ;;; 3. Return ContainsExpression of BindingProperty.
   ; BindingElementList : BindingElementList , BindingElisionElement
   ;;; 1. Let has be ContainsExpression of BindingElementList.
   ;;; 2. If has is true, return true.
   ;;; 3. Return ContainsExpression of BindingElisionElement.
   ; BindingElisionElement : Elisionopt BindingElement
   ;;; 1. Return ContainsExpression of BindingElement.
   ; BindingProperty : PropertyName : BindingElement
   ;;; 1. Let has be IsComputedPropertyKey of PropertyName.
   ;;; 2. If has is true, return true.
   ;;; 3. Return ContainsExpression of BindingElement.
   ; BindingElement : BindingPattern Initializer
   ;;; 1. Return true.
   ; SingleNameBinding : BindingIdentifier
   ;;; 1. Return false.
   ; SingleNameBinding : BindingIdentifier Initializer
   ;;; 1. Return true.
   ; BindingRestElement : ... BindingIdentifier
   ;;; 1. Return false.
   ; BindingRestElement : ... BindingPattern
   ;;; 1. Return ContainsExpression of BindingPattern.
   ; FormalParameters : [empty]
   ;;; 1. Return false.
   ; FormalParameters : FormalParameterList , FunctionRestParameter
   ;;; 1. If ContainsExpression of FormalParameterList is true, return true.
   ;;; 2. Return ContainsExpression of FunctionRestParameter.
   ; FormalParameterList : FormalParameterList , FormalParameter
   ;;; 1. If ContainsExpression of FormalParameterList is true, return true.
   ;;; 2. Return ContainsExpression of FormalParameter.
   ; ArrowParameters : BindingIdentifier
   ;;; 1. Return false.
   ; ArrowParameters : CoverParenthesizedExpressionAndArrowParameterList
   ;;; 1. Let formals be the ArrowFormalParameters that is covered by CoverParenthesizedExpressionAndArrowParameterList.
   ;;; 2. Return ContainsExpression of formals.
   ; AsyncArrowBindingIdentifier : BindingIdentifier
   ;;; 1. Return false.
   (return false)))

(section
  (:15.1.3 IsSimpleParameterList (parseNode))
  (; BindingElement : BindingPattern
   ;;; 1. Return false.
   (if (is-pn BindingElement 1) ((return false)))
   ; BindingElement : BindingPattern Initializer
   ;;; 1. Return false.
   (if (is-pn BindingElement 2) ((return false)))
   ; SingleNameBinding : BindingIdentifier
   ;;; 1. Return true.
   (if (is-pn SingleNameBinding 0) ((return true)))
   ; SingleNameBinding : BindingIdentifier Initializer
   ;;; 1. Return false.
   (if (is-pn SingleNameBinding 1) ((return false)))
   ; FormalParameters : [empty]
   ;;; 1. Return true.
   (if (is-pn FormalParameters 0) ((return true)))
   ; FormalParameters : FunctionRestParameter
   ;;; 1. Return false.
   (if (is-pn FormalParameters 1) ((return false)))
   ; FormalParameters : FormalParameterList , FunctionRestParameter
   ;;; 1. Return false.
   (if (is-pn FormalParameters 4) ((return false)))
   ; FormalParameterList : FormalParameterList , FormalParameter
   (if (is-pn FormalParameterList 1)
       (;;; 1. If IsSimpleParameterList of FormalParameterList is false, return false.
        (if (is-false (call IsSimpleParameterList (:parseNode -> JSSATParseNodeSlot1)))
            ((return false)))
        ;;; 2. Return IsSimpleParameterList of FormalParameter.
        (return (call IsSimpleParameterList (:parseNode -> JSSATParseNodeSlot2)))))
   ; FormalParameter : BindingElement
   ;;; 1. Return IsSimpleParameterList of BindingElement.
   ; ^ wait wtf isn't this suppose to happen anyway???
   ; or hmm maybe not because it's a static semantic
   (if (is-pn FormalParameter 0) ((return (call IsSimpleParameterList (:parseNode -> JSSATParseNodeSlot1)))))
   ; ArrowParameters : BindingIdentifier
   ;;; 1. Return true.
   (if (is-pn ArrowParameters 0) ((return true)))
   ; ArrowParameters : CoverParenthesizedExpressionAndArrowParameterList
   (if (is-pn ArrowParameters 1)
       (;;; 1. Let formals be the ArrowFormalParameters that is covered by CoverParenthesizedExpressionAndArrowParameterList.
        (formals = 0)
        (assert false "TODO: handle the `covered`")
        ;;; 2. Return IsSimpleParameterList of formals.
        (return (call IsSimpleParameterList :formals))))
   ; AsyncArrowBindingIdentifier : BindingIdentifier
   ;;; 1. Return true.
   (if (is-pn AsyncArrowBindingIdentifier 0) ((return true)))
   ; CoverCallExpressionAndAsyncArrowHead : MemberExpression Arguments
   (if (is-pn CoverCallExpressionAndAsyncArrowHead 0)
       (;;; 1. Let head be the AsyncArrowHead that is covered by CoverCallExpressionAndAsyncArrowHead.
        (head = 0)
        (assert false "TODO: handle the `covered` here")
        ;;; 2. Return IsSimpleParameterList of head.
        (return (call IsSimpleParameterList :head))))
   ; TODO: should we do this? is this only for rubntime smentaics?
   (return (call IsSimpleParameterList (:parseNode -> JSSATParseNodeSlot1)))))

(section
  (:15.1.4 HasInitializer (parseNode))
  (; BindingElement : BindingPattern
   (if (is-pn BindingElement 1)
       (;;; 1. Return false.
        (return false)))
   ; BindingElement : BindingPattern Initializer
   (if (is-pn BindingElement 2)
       (;;; 1. Return true.
        (return true)))
   ; SingleNameBinding : BindingIdentifier
   (if (is-pn SingleNameBinding 0)
       (;;; 1. Return false.
        (return false)))
   ; SingleNameBinding : BindingIdentifier Initializer
   (if (is-pn SingleNameBinding 1)
       (;;; 1. Return true.
        (return true)))
   ; FormalParameterList : FormalParameterList , FormalParameter
   (if (is-pn FormalParameterList 1)
       (;;; 1. If HasInitializer of FormalParameterList is true, return true.
        (if (is-true (call HasInitializer (:parseNode -> JSSATParseNodeSlot1)))
            ((return true)))
        ;;; 2. Return HasInitializer of FormalParameter.
        (return (call HasInitializer (:parseNode -> JSSATParseNodeSlot2)))))
   (return (call HasInitializer (:parseNode -> JSSATParseNodeSlot1)))))

(section
  (:15.1.5 ExpectedArgumentCount (parseNode))
  (; FormalParameters :
   ;     [empty]
   ;     FunctionRestParameter
   (if (or (is-pn FormalParameters 0) (is-pn FormalParameters 1))
       (;;; 1. Return 0.
        (return 0)))
   ; FormalParameters : FormalParameterList , FunctionRestParameter
   (if (is-pn FormalParameters 4)
       (;;; 1. Return ExpectedArgumentCount of FormalParameterList.
        (return (call ExpectedArgumentCount (:parseNode -> JSSATParseNodeSlot1)))))
   ; FormalParameterList : FormalParameter
   (if (is-pn FormalParameterList 0)
       (;;; 1. If HasInitializer of FormalParameter is true, return 0.
        (if (is-true (call HasInitializer (:parseNode -> JSSATParseNodeSlot1)))
            ((return 0)))
        ;;; 2. Return 1.
        (return 1)))
   ; FormalParameterList : FormalParameterList , FormalParameter
   (if (is-pn FormalParameterList 1)
       (;;; 1. Let count be ExpectedArgumentCount of FormalParameterList.
        (count = (call ExpectedArgumentCount (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. If HasInitializer of FormalParameterList is true or HasInitializer of FormalParameter is true, return count.
        (if (or
             (is-true (call HasInitializer (:parseNode -> JSSATParseNodeSlot1)))
             (is-true (call HasInitializer (:parseNode -> JSSATParseNodeSlot2))))
            ((return :count)))
        ;;; 3. Return count + 1.
        (return (:count + 1))))
   ; ArrowParameters : BindingIdentifier
   (if (is-pn ArrowParameters 0)
       (;;; 1. Return 1.
        (return 1)))
   ; ArrowParameters : CoverParenthesizedExpressionAndArrowParameterList
   (if (is-pn ArrowParameters 1)
       (;;; 1. Let formals be the ArrowFormalParameters that is covered by CoverParenthesizedExpressionAndArrowParameterList.
        (todo)
        (assert false "in the future we'll have every parse node that's a `Cover X` automatically try to be parsed as a")
        (assert false "cover thing, so that we can just perform a member lookup")
        ;;; 2. Return ExpectedArgumentCount of formals.
       ))
   ; PropertySetParameterList : FormalParameter
   (if (is-pn PropertySetParameterList 0)
       (;;; 1. If HasInitializer of FormalParameter is true, return 0.
        (if (is-true (call HasInitializer (:parseNode -> JSSATParseNodeSlot1)))
            ((return 0)))
        ;;; 2. Return 1.
        (return 1)))
   ; AsyncArrowBindingIdentifier : BindingIdentifier
   (if (is-pn AsyncArrowBindingIdentifier 0)
       (;;; 1. Return 1.
        (return 1)))
   (return (call ExpectedArgumentCount (:parseNode -> JSSATParseNodeSlot1)))))

(section
  (:15.2.3 EvaluateFunctionBody (parseNode, functionObject, argumentsList))
  (; FunctionBody : FunctionStatementList
   (if (is-pn FunctionBody 0)
       (;;; 1. Perform ? FunctionDeclarationInstantiation(functionObject, argumentsList).
        (_dontCare = (? (call FunctionDeclarationInstantiation :functionObject :argumentsList)))
        ;;; 2. Return the result of evaluating FunctionStatementList.
        (return (evaluating (:parseNode -> JSSATParseNodeSlot1)))))
   (return (NormalCompletion empty))))

(section
  (:15.2.4 InstantiateOrdinaryFunctionObject (parseNode, scope, privateScope))
  (; FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
   (if (pn-variant-is :parseNode 0)
       (;;; 1. Let name be StringValue of BindingIdentifier.
        (name = (StringValueOfBindingIdentifier (:parseNode -> JSSATParseNodeSlot1)))
        ;;; 2. Let sourceText be the source text matched by FunctionDeclaration.
        (sourceText = :parseNode -> JSSATParseNodeSourceText)
        ;;; 3. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters, FunctionBody,
        ;;;    non-lexical-this, scope, privateScope).
        (F = (call OrdinaryFunctionCreate null :sourceText (:parseNode -> JSSATParseNodeSlot2) (:parseNode -> JSSATParseNodeSlot3)
                   non-lexical-this :scope :privateScope))
        ;;; 4. Perform SetFunctionName(F, name).
        (call SetFunctionName :F :name undefined)
        ;;; 5. Perform MakeConstructor(F).
        (call MakeConstructor :F undefined undefined)
        ;;; 6. Return F.
        (return :F)))
   ; FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
   (if (pn-variant-is :parseNode 1)
       (;;; 1. Let sourceText be the source text matched by FunctionDeclaration.
        (sourceText = :parseNode -> JSSATParseNodeSourceText)
        ;;; 2. Let F be OrdinaryFunctionCreate(%Function.prototype%, sourceText, FormalParameters, FunctionBody,
        ;;;    non-lexical-this, scope, privateScope).
        (F = (call OrdinaryFunctionCreate null :sourceText (:parseNode -> JSSATParseNodeSlot2) (:parseNode -> JSSATParseNodeSlot3)
                   non-lexical-this :scope :privateScope))
        ;;; 3. Perform SetFunctionName(F, "default").
        (call SetFunctionName :F "default" undefined)
        ;;; 4. Perform MakeConstructor(F).
        (call MakeConstructor :F undefined undefined)
        ;;; 5. Return F.
        (return :F)))
   (todo)
   (return unreachable)))

(section
  (:15.2.6 FunctionDeclaration_Evaluation (parseNode))
  (; FunctionDeclaration : function BindingIdentifier ( FormalParameters ) { FunctionBody }
   ;;; 1. Return NormalCompletion(empty).
   ; FunctionDeclaration : function ( FormalParameters ) { FunctionBody }
   ;;; 1. Return NormalCompletion(empty).
   ; FunctionStatementList : [empty]
   (if (is-pn FunctionStatementList 0)
       (;;; 1. Return NormalCompletion(undefined).
        (return (NormalCompletion undefined))))
   (return (NormalCompletion empty))))

(section
  (:15.10.3 PrepareForTailCall ())
  (; i am sorry to disapoint you dear reader, but there appears to be nothing here!
   (return)
   ;;; 1. Assert: The current execution context will not subsequently be used for the evaluation of any ECMAScript code or built-in functions. The invocation of Call subsequent to the invocation of this abstract operation will create and push a new execution context before performing any such evaluation.
   ;;; 2. Discard all resources associated with the current execution context.
  ))

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
   (exec-ctx-stack-pop)
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
   (assert (0 != exec-ctx-stack-size) "The execution context stack is not empty.")
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
   (numVarDecls = (list-len :varDeclarations))
   ;;; 7. Let functionsToInitialize be a new empty List.
   (functionsToInitialize = list-new)
   ;;; 8. Let declaredFunctionNames be a new empty List.
   (declaredFunctionNames = list-new)
   ;;; 9. For each element d of varDeclarations, in reverse List order, do
   (for :varDeclarations
        ((d = for-item-rev)
         ;;; a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
         ; "X is neither an A nor B nor C"
         ; "X is not an A, X is not a B, X is not a C"
         (if (and3 (pn-kind-isnt :d VariableDeclaration) (pn-kind-isnt :d ForBinding) (pn-kind-isnt :d BindingIdentifier))
             (;;; i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.
              (assert (or4
                       (pn-kind-is :d FunctionDeclaration)
                       (pn-kind-is :d GeneratorDeclaration)
                       (pn-kind-is :d AsyncFunctionDeclaration)
                       (pn-kind-is :d AsyncGeneratorDeclaration))
                      "d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration.")
              ;;; ii. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
              ; ^ that seems hard
              ;;; iii. Let fn be the sole element of the BoundNames of d.
              (fn = (sole-element (call BoundNames :d)))
              ;;; iv. If fn is not an element of declaredFunctionNames, then
              (if (not (list-contains :declaredFunctionNames :fn))
                  (;;; 1. Let fnDefinable be ? env.CanDeclareGlobalFunction(fn).
                   (fnDefinable = (? (:env .. CanDeclareGlobalFunction :fn)))
                   ;;; 2. If fnDefinable is false, throw a TypeError exception.
                   (if (is-false :fnDefinable)
                       ((throw (TypeError "fn definable falsy :((("))))
                   ;;; 3. Append fn to declaredFunctionNames.
                   (list-push :declaredFunctionNames :fn)
                   ;;; 4. Insert d as the first element of functionsToInitialize.
                   (list-insert-front :functionsToInitialize :d)))))))
   ;;; 10. Let declaredVarNames be a new empty List.
   (declaredVarNames = list-new)
   ;;; 11. For each element d of varDeclarations, do
   (for :varDeclarations
        ((d = for-item)
         ;;; a. If d is a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
         (if (or3 (pn-kind-is :d VariableDeclaration) (pn-kind-is :d ForBinding) (pn-kind-is :d BindingIdentifier))
             (;;; i. For each String vn of the BoundNames of d, do
              (boundNamesOfD = (call BoundNames :d))
              (for :boundNamesOfD
                   ((vn = for-item)
                    ;;; 1. If vn is not an element of declaredFunctionNames, then
                    (if (not (list-contains :declaredFunctionNames :vn))
                        (;;; a. Let vnDefinable be ? env.CanDeclareGlobalVar(vn).
                         (vnDefinable = (? (:env .. CanDeclareGlobalVar :vn)))
                         ;;; b. If vnDefinable is false, throw a TypeError exception.
                         (if (is-false :vnDefinable)
                             ((throw (TypeError "vnDefinable false"))))
                         ;;; c. If vn is not an element of declaredVarNames, then
                         (if (not (list-contains :declaredVarNames :vn))
                             (;;; i. Append vn to declaredVarNames.
                              (list-push :declaredVarNames :vn)))))))))))
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
         (fo = (call InstantiateFunctionObject :f :env null))
         ;;; c. Perform ? env.CreateGlobalFunctionBinding(fn, fo, false).
         (_dontCare = (? (:env .. CreateGlobalFunctionBinding :fn :fo false)))))
   ;;; 17. For each String vn of declaredVarNames, do
   (for :declaredVarNames
        ((vn = for-item)
         ;;; a. Perform ? env.CreateGlobalVarBinding(vn, false).
         ;  (_dontCare = (? (:env .. CreateGlobalVarBinding :vn false)))
        ))
   ;;; 18. Return NormalCompletion(empty).
   (return (NormalCompletion empty))))