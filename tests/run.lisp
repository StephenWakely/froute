(in-package :froute-tests)

(defun run-froute-tests ()
  (let ((lisp-unit:*print-errors* t)
        (lisp-unit:*print-failures* t))
    (lisp-unit:run-tests :all :froute-tests)))
