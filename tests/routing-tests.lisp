(in-package :froute-tests)



(define-test test-simple-route-matches
  (multiple-value-bind (vars remaining matched)
      (froute::route-matches "onk/ponk/" :any "onk/ponk" :get)
    (declare (ignore vars remaining))
    (assert-true matched)))

(define-test test-route-returns-remaining-path
  (multiple-value-bind (vars remaining matched)
      (froute::route-matches "onk/ponk/" :any "onk/ponk/orkfloot/tonk" :get)
    (declare (ignore vars))
    (assert-true matched)
    (assert-equal '("orkfloot" "tonk") remaining)))


(defclass test-route ()
  ()
  (:metaclass froute-class)
  (:route "test"))

(defmethod run ((r test-route) method)
  (format nil "test worked with a ~A" method))

(define-test simple-get-route
  (assert-equal "test worked with a GET" (invoke-route "test" :method :get)))

(define-test simple-post-route
  (assert-equal "test worked with a POST" (invoke-route "test" :method :post)))

(defclass route-with-params ()
  ((ponk :accessor ponk-param))
  (:metaclass froute-class)
  (:route "test-params/:ponk"))

(defmethod run ((r route-with-params) method)
  (declare (ignore method))
  (format nil "ponk~A" (ponk-param r)))

(define-test params-route
  (assert-equal "ponklots" (invoke-route "test-params/lots")))

(defclass wildcard-route ()
  ((erk :accessor erk-param))
  (:metaclass froute-class)
  (:route "ponk/:erk*"))

(defmethod run ((r wildcard-route) method)
  (declare (ignore method))
  (format nil "Ponk ~A" (erk-param r)))

(define-test wildcard-route
  (assert-equal "Ponk ookplork/flork" (invoke-route "ponk/ookplork/flork" :method :get)))
