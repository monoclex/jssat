;;;;;;;;;;;;;;;;
; LIST IR FILE ;
;;;;;;;;;;;;;;;;
;
; Tests that lists are working
;

(section
  (:0.0.0 Add (x, y))
  ((result = (:x + :y))
   (return :result)))
