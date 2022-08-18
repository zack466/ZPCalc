(in-package :cl-user)

(defpackage rpncalc/util
  (:use :cl)
  (:export #:mkstr #:symb #:setf-maybe #:symbol= #:with-gensyms #:compose))
(in-package :rpncalc/util)

;; Evaluates target once, and if it is not nil,
;; then it does (setf place target)
;; Also optionally takes a function to excecute if the target is nil
(defmacro setf-maybe (place target)
  (let ((g (gensym)))
    `(let ((,g ,target))
      (if (not (null,g))
          (setf ,place ,g)))))

(defun symbol= (a b)
  (if (and (symbolp a) (symbolp b))
      (string= (symbol-name a) (symbol-name b))
      nil))

;; utils taken from Paul Graham's On Lisp
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)

 (defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v)) 
                rest
                :initial-value (apply fn1 args)))))    ,@body))
