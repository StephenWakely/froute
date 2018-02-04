(in-package :froute-tests)



;; Test the more complicated scenarios where route classes
;; inherit from route classes.

(defclass base-route ()
  ()
  (:metaclass froute-class)
  (:route "base"))

(defclass ponk-route ()
  () 
  (:metaclass froute-class)
  (:route "ponk"))

(defclass shnorp-route (base-route)
  ()
  (:metaclass froute-class)
  (:route "shnorple"))

(defclass child-route (ponk-route base-route)
  ()
  (:metaclass froute-class)
  (:route "child"))


(defmethod run ((r shnorp-route) method)
  "Shnorple called")

(define-test inherited-class-adds-to-route
  (assert-equal "Shnorple called" (invoke-route "base/shnorple" :method :get)))

(defmethod run ((r child-route) method)
  "Child called")

(define-test multiple-inheritance-class-builds-the-route
  (assert-equal "Child called" (invoke-route "base/ponk/child" :method :get)))



(defclass plook-route ()
  ((plook :reader plook))
  (:metaclass froute-class)
  (:route "plook/:plook"))

(defclass norkwonk-route (plook-route)
  ((nork :reader nork))
  (:metaclass froute-class)
  (:route "nork/:nork"))

(defmethod run ((r norkwonk-route) method)
  (format nil "Plook is ~A, Nork is ~A" (plook r) (nork r)))

(define-test inherited-class-collects-variables
  (assert-equal "Plook is theplook, Nork is thenork"
                (invoke-route "plook/theplook/nork/thenork")))
