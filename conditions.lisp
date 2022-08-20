(defpackage rpncalc/conditions
  (:use :cl)
  (:export
    #:rpn-undefined-function
    #:rpn-undefined-variable
    #:rpn-unsupported-element
    #:rpn-stack-empty
    #:rpn-name
    #:rpn-element
    #:rpn-cannot-undo
    #:rpn-cannot-redo
    #:rpn-cannot-eval
    ))
(in-package :rpncalc/conditions)

(define-condition rpn-undefined-function (error)
  ((name :initarg :name :initform nil :reader rpn-name)))

(define-condition rpn-undefined-variable (error)
  ((name :initarg :name :initform nil :reader rpn-name)))

(define-condition rpn-unsupported-element (error)
  ((element :initarg :element :initform nil :reader rpn-element)))

(define-condition rpn-stack-empty (error) ())

(define-condition rpn-cannot-undo (error) ())
(define-condition rpn-cannot-redo (error) ())

(define-condition rpn-cannot-eval (error)
  ((element :initarg :element :initform nil :reader rpn-element)))
