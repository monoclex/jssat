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

; syntax sugar
(def (and :a :b) (:a and :b))
(def (or :a :b) (:a or :b))
(def (and3 :a :b :c) (and (and :a :b) :c))
(def (both :a :b (:x :y)) (and (:a :x :y) (:b :x :y)))
(def (either :a :b (:x :y)) (or (:a :x :y) (:b :x :y)))

(def
  (elif :condition :then-expr :end-expr)
  ((if :condition :then-expr :end-expr)))

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

; `ValueType::Bytes` exists, not `ValueType::String`
(def String Bytes)

; shorthand for trivial types
(def null (trivial Null))
(def undefined (trivial Undefined))
(def normal (trivial Normal))
(def empty (trivial Empty))
(def (trivial throw) (trivial Throw))

; shorthand checks
(def (is-undef :x) (:x == undefined))
(def (isnt-undef :x) (:x != undefined))
(def (is-null :x) (:x == null))
(def (isnt-null :x) (:x != null))

; instruction helpers
(def (record-absent-slot :record :slot) (not (record-has-slot :record :slot)))
(def (record-absent-prop :record :prop) (not (record-has-prop :record :prop)))
(def
  (record-copy-slot-or-default :src :dest :slot :default)
  (if (record-absent-slot :src :slot)
      ((record-set-slot :dest :slot :default))
      ((record-set-slot :dest :slot (record-get-slot :src :slot)))))

; completion record checks
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
   ((arg = :x)
    (;;; 1. If argument is an abrupt completion, return argument.
     (if (is-abrupt-completion :arg)
         ((return :arg)
          (unreachable)))
     ;;; 2. Else if argument is a Completion Record, set argument to argument.[[Value]].
     (elif (is-completion-record :arg)
           ((record-get-slot :arg Value))
           (:arg))))))

(section
  (:6.2.5.1 IsAccessorDescriptor (Desc))
  (;;; 1. If Desc is undefined, return false.
   (if (is-undef :Desc)
       ((return false)))
   ;;; 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
   (if ((record-absent-slot :Desc Get) and (record-absent-slot :Desc Set))
       ((return false)))
   ;;; 3. Return true.
   (return true)))

(section
  (:6.2.5.2 IsDataDescriptor (Desc))
  (;;; 1. If Desc is undefined, return false.
   (if (is-undef :Desc)
       ((return false)))
   ;;; 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
   (if ((record-absent-slot :Desc Value) and (record-absent-slot :Desc Writable))
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
   (if (is-type-of String :argument)
       ((return true)))
   ;;; 2. If Type(argument) is Symbol, return true.
   (if (is-type-of Symbol :argument)
       ((return true)))
   ;;; 3. Return false.
   (return false)))

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
   (return)))
