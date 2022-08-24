(defpackage rpncalc/conditions
  (:use :cl)
  (:export
    #:rpn-undefined-function
    #:rpn-undefined-package
    #:rpn-undefined-variable
    #:rpn-stack-empty
    #:rpn-name
    #:rpn-element
    #:rpn-cannot-undo
    #:rpn-cannot-redo
    #:rpn-cannot-evaluate
    #:rpn-syntax-error
    #:rpn-quit
    #:rpn-package
    #:rpn-invalid-package-name
    #:rpn-invalid-function-name
    #:rpn-cannot-enter-builtins
    #:rpn-unreachable
    ))
(in-package :rpncalc/conditions)

(define-condition rpn-undefined-function (error)
  ((name :initarg :name :initform nil :reader rpn-name)
   (package :initarg :package :initform nil :reader rpn-package)))

(define-condition rpn-undefined-variable (error)
  ((name :initarg :name :initform nil :reader rpn-name)))

(define-condition rpn-undefined-package (error)
  ((name :initarg :name :initform nil :reader rpn-name)))

(define-condition rpn-stack-empty (error) ())

(define-condition rpn-cannot-undo (error) ())
(define-condition rpn-cannot-redo (error) ())

(define-condition rpn-cannot-evaluate (error)
  ((element :initarg :element :initform nil :reader rpn-element)))

(define-condition rpn-syntax-error (error)
  ((element :initarg :element :initform nil :reader rpn-element)))

(define-condition rpn-quit (simple-condition) ())

(define-condition rpn-invalid-package-name (warning) ())
(define-condition rpn-invalid-function-name (warning) ())

(define-condition rpn-cannot-enter-builtins (warning) ())

(define-condition rpn-unreachable (error) ((message :initarg :message :initform nil :reader rpn-message)))
