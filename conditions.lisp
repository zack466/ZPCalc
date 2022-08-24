(defpackage rpncalc/conditions
  (:use :cl)
  (:export
    #:rpn-undefined-function
    #:rpn-undefined-variable
    #:rpn-stack-empty
    #:rpn-name
    #:rpn-element
    #:rpn-cannot-undo
    #:rpn-cannot-redo
    #:rpn-cannot-evaluate
    #:rpn-syntax-error
    #:rpn-quit
    ))
(in-package :rpncalc/conditions)

(define-condition rpn-undefined-function (error)
  ((name :initarg :name :initform nil :reader rpn-name)))

(define-condition rpn-undefined-variable (error)
  ((name :initarg :name :initform nil :reader rpn-name)))

(define-condition rpn-stack-empty (error) ())

(define-condition rpn-cannot-undo (error) ())
(define-condition rpn-cannot-redo (error) ())

(define-condition rpn-cannot-evaluate (error)
  ((element :initarg :element :initform nil :reader rpn-element)))

(define-condition rpn-syntax-error (error)
  ((element :initarg :element :initform nil :reader rpn-element)))

(define-condition rpn-quit (simple-condition) ())
