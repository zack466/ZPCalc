(defpackage zpcalc/conditions
  (:use :cl)
  (:export
    #:calc-undefined-function
    #:calc-undefined-package
    #:calc-undefined-variable
    #:calc-stack-empty
    #:calc-name
    #:calc-element
    #:calc-cannot-undo
    #:calc-cannot-redo
    #:calc-cannot-evaluate
    #:calc-syntax-error
    #:calc-quit
    #:calc-package
    #:calc-invalid-package-name
    #:calc-invalid-function-name
    #:calc-cannot-enter-builtins
    #:calc-unreachable
    #:calc-message
    ))
(in-package :zpcalc/conditions)

(define-condition calc-undefined-function (error)
  ((name :initarg :name :initform nil :reader calc-name)
   (package :initarg :package :initform nil :reader calc-package)))

(define-condition calc-undefined-variable (error)
  ((name :initarg :name :initform nil :reader calc-name)))

(define-condition calc-undefined-package (error)
  ((name :initarg :name :initform nil :reader calc-name)))

(define-condition calc-stack-empty (error) ())

(define-condition calc-cannot-undo (error) ())
(define-condition calc-cannot-redo (error) ())

(define-condition calc-cannot-evaluate (error)
  ((element :initarg :element :initform nil :reader calc-element)))

(define-condition calc-syntax-error (error)
  ((element :initarg :element :initform nil :reader calc-element)))

(define-condition calc-quit (simple-condition) ())

(define-condition calc-invalid-package-name (warning) ())
(define-condition calc-invalid-function-name (warning) ())

(define-condition calc-cannot-enter-builtins (warning) ())

(define-condition calc-unreachable (error) ((message :initarg :message :initform nil :reader calc-message)))
