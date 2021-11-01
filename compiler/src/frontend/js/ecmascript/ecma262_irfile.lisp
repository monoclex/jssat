(def
  (elif :condition :then-expr :end-expr)
  (else ((if :condition :then-expr :end-expr))))

(def (is-not-undef :x) (not (is-undef :x)))
(def (is-undef :x) (:x == undefined))

;; (def (! :x) ())
;; (def (? :x) (return-if-abrupt :x))

(def (record-absent-slot :record :slot) (not (record-has-slot :record :slot)))

(def (:x != :y) (not (:x == :y)))
(def undefined (trivial Undefined))
(def null (trivial Null))
(def String Bytes)

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
   (if (((call IsAccessorDescriptor :Desc) == false) and ((call IsDataDescriptor :Desc) == false))
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
   (if (is-not-undef :O)
       ((assert ((call IsPropertyKey :P) == true) "If O is not undefined, then IsPropertyKey(P) is true")))
   ;;; 2. If current is undefined, then
   (if (is-undef :current)
       (;;; a. If extensible is false, return false
        (if (:extensible == false)
            ((return false)))
        ;;; b. Assert: extensible is true.
        (assert (:extensible == true) "extensible is true")
        ;;; c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
        (if (((call IsGenericDescriptor :Desc) == true) or ((call IsDataDescriptor :Desc) == true))
            (;;; i. If O is not undefined, create an own data property named P of object O whose [[Value]],
             ;;;[[Writable]], [[Enumerable]], and [[Configurable]] attribute values are described by Desc. If the
             ;;;value of an attribute field of Desc is absent, the attribute of the newly created property is set to its
             ;;;default value.

             ; TODO
            ))))
   (return)))