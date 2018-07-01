(defpackage :froute-system
  (:use :cl :asdf))

(in-package :froute-system)



(defsystem :froute
  :depends-on (:closer-mop :cl-ppcre)
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "froute-class" :depends-on ("package"))
                                     (:file "routes" :depends-on ("package" "froute-class"))
                                     (:file "invoke-route" :depends-on ("package" "routes")))))
  :in-order-to ((test-op (test-op :froute/test))))

(defsystem :froute/hunchentoot
  :depends-on (:hunchentoot :froute)
  :components ((:module "hunchentoot"
                        :components ((:file "package")
                                     (:file "acceptor" :depends-on ("package"))))))

(defsystem :froute/test
  :depends-on (:froute :lisp-unit)
  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "run" :depends-on ("package"))
                                     (:file "routing-tests" :depends-on ("package"))
                                     (:file "inheritance-tests" :depends-on ("package")))))
  :perform (test-op (o s)
                    (uiop:symbol-call :froute-tests 'run-froute-tests)))


