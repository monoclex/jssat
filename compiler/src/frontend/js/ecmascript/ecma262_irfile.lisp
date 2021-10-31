(def
  (:is-not-undefined x)
  (x = (trivial undefined)))

(section
  (header 6.9 testing)
  (body ()))

(section
  (header 10.1.6.3 ValidateAndApplyPropertyDescriptor (O, P, extensible, Desc, current))
  (body (;;; 1. Assert: If O is not undefined, then IsPropertyKey(P) is true
         (if (is-not-undefined O)
             (then ((assert ((IsPropertyKey P) = true) "If O is not undefined, then IsPropertyKey(P) is true"))))
         ;;; 2. If current is undefined, then
         (if (is-undefined current)
             (then (;;; a. If extensible is false, return false
                    (if (extensible = false)
                        (then ((return false))))
                    ;;; b. Assert: extensible is true.
                    (assert (extensible = true) "extensible is true")
                    ;;; c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
                    (if (((IsGenericDescriptor Desc) = true) or ((IsDataDescriptor Desc) = true))
                        (then (;;; i. If O is not undefined, create an own data property named P of object O whose [[Value]],
                               ;;;[[Writable]], [[Enumerable]], and [[Configurable]] attribute values are described by Desc. If the
                               ;;;value of an attribute field of Desc is absent, the attribute of the newly created property is set to its
                               ;;;default value.
                               ("todo"))))))))))