;;;;;;;;;;;;;;;;
; LIST IR FILE ;
;;;;;;;;;;;;;;;;
;
; Tests that lists are working
;

(section
  (:0 Add (x, y))
  ((result = (:x + :y))
   (return :result)))

(section
  (:0 GetList (list, idx))
  ((return (list-get :list :idx))))
