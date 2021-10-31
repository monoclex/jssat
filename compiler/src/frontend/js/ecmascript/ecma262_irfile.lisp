(def
  (is-not-undefined :x)
  (:x == (trivial undefined)))

(def
  (elif :condition :then-expr :end-expr)
  (else ((if :condition :then-expr :end-expr))))

(section
  (:6.9 testing)
  ())

(section
  (:10.1.6.3 ValidateAndApplyPropertyDescriptor (O, P, extensible, Desc, current))
  (;;; 1. Assert: If O is not undefined, then IsPropertyKey(P) is true
   (if (is-not-undefined O)
       ((assert ((call IsPropertyKey P) == true) "If O is not undefined, then IsPropertyKey(P) is true")))
   ;;; 2. If current is undefined, then
   (if (is-undefined current)
       (;;; a. If extensible is false, return false
        (if (extensible == false)
            ((return false)))
        ;;; b. Assert: extensible is true.
        (assert (extensible == true) "extensible is true")
        ;;; c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
        (if (((call IsGenericDescriptor Desc) = true) or ((call IsDataDescriptor Desc) = true))
            (;;; i. If O is not undefined, create an own data property named P of object O whose [[Value]],
             ;;;[[Writable]], [[Enumerable]], and [[Configurable]] attribute values are described by Desc. If the
             ;;;value of an attribute field of Desc is absent, the attribute of the newly created property is set to its
             ;;;default value.
             ("todo")))))))