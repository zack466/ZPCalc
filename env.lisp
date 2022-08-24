(in-package :cl-user)

(defpackage rpncalc/env
  (:use :cl)
  (:import-from #:rpncalc/util #:acond)
  (:export
    #:make-env
    #:get-env))
(in-package :rpncalc/env)

;; A simple lexical environment backed by a hash table
;; environment :: (hash-table * Maybe environment)

;; initialize an environment, optionally with a parent environment
(defun make-env (&optional parent)
  (cons (make-hash-table) parent))

;; Get the value of a variable in an environment
;; If not found, returns nil
(defun get-env (key env)
  (acond ((gethash key (car env)) it)
         ((cdr env) (env-get it key))
         (t nil)))

;; Allow the use of setf to set variable values
(defsetf get-env (name env) (new-value)
  `(setf (gethash ,name (car ,env)) ,new-value))