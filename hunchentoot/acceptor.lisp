(in-package :froute-hunchentoot)

(defclass froute-acceptor (acceptor)
  ())

(defmethod acceptor-dispatch-request ((acceptor froute-acceptor) request)
  (let ((result (invoke-route
                 (request-uri*) :method (request-method*))))
    (or result (call-next-method acceptor request))))

