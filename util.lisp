(in-package :cl-user)

(defpackage rpncalc/util
  (:use :cl)
  (:export
    #:mkstr
    #:symb
    #:symbol=
    #:with-gensyms
    #:compose
    #:square
    #:is-isquare
    #:is-pow
    #:log-exact
    #:expt-exact
    #:sqrt-exact
    #:->double
    #:bool->int
    #:make-keyword
    #:falsy
    #:truthy
    #:alambda
    #:aif
    #:acond
    #:package-designator
    ))
(in-package :rpncalc/util)

(defun symbol= (a b)
  (if (and (symbolp a) (symbolp b))
      (string= (symbol-name a) (symbol-name b))
      nil))

(defun square (x)
  (* x x))

(defun is-isquare (x)
  (= x (square (isqrt x))))

;; returns if x can be represented as an integer power of the base
(defun is-pow (x base)
  (if (or (complexp x) (floatp x) (floatp base))
    nil
    (let ((approx (round (log x base))))
      (if (= (expt base approx) x)
        approx
        nil))))

;; log, except tries to preserve exactness
(defun log-exact (num base)
  (let ((result (is-pow num base)))
    (if result
      result
      (log (->double num) (->double base)))))

(defun bool->int (b)
  (if b 1 0))

(defun ->double (x)
  (typecase x
    (complex (coerce x '(complex double-float)))
    (t (coerce x 'double-float))))

(defun sqrt-exact (x)
  (typecase x
    (integer (if (is-isquare x) (isqrt x) (sqrt (->double x))))
    (rational (if (and (is-isquare (numerator x))
                       (is-isquare (denominator x)))
                (/ (isqrt (numerator x)) (isqrt (denominator x)))
                (sqrt (->double x))))
    (t (sqrt (->double x)))))

(defun expt-exact (a b)
  (if (and (or (integerp a) (rationalp a)) (integerp b))
    (expt a b)
    (expt (->double a) (->double b))))

(defun falsy (x)
  (zerop x))

(defun truthy (x)
  (not (falsy x)))

;; taken from StackOverflow
(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

;; some utils taken from Paul Graham's On Lisp
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v)) 
                rest
                :initial-value (apply fn1 args)))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; modified to be package-agnostic
(defmacro aif (test-form then-form &optional else-form)
  (let ((it (intern (string 'it))))
    `(let ((,it ,test-form))
       (if ,it ,then-form ,else-form))))

;; modified - chained aif
(defmacro acond (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses)))
      `(aif ,(car cl1)
           ,@(cdr cl1)
           (acond ,@(cdr clauses))))))

;; Returns a symbol's proper name and the package it should be located in
;; If no package designation (no dot), return nil for the package
;; Ex: (package-designator :hello.world) ==> (values :hello :world)
(defun package-designator (symbol)
  (aif (position #\. (string symbol))
       (values
         (make-keyword (subseq (string symbol) 0 it))
         (make-keyword (subseq (string symbol) (1+ it))))
       (values nil symbol)))
