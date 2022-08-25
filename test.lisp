(defpackage zpcalc/tests
  (:use :cl :zpcalc))
(in-package :zpcalc/tests)

(defvar *tests* nil)

(define-condition test-fail (error) ())

(defmacro deftest (name &rest body)
  `(push (cons ,(string name) (lambda () (block nil ,@body)))
    *tests*))

(defmacro fail ()
  `(signal 'test-fail))

(defmacro success ()
  `(return nil))

(defun run-tests ()
  (format t "Running tests...~%")
  (loop for test in *tests*
        do (progn
             (handler-case
               (progn (funcall (cdr test))
                      (format t "Test Passed: ~S~%" (car test)))
               (t (e) (format t "Test Failed: ~S ~%~4t~a~%" (car test) e)))))
  (format t "Finished testing.~%"))

(deftest hello
  (success))

(run-tests)
