(in-package :cl-user)

(defpackage zpcalc/util
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
    #:invpow-exact
    #:->double
    #:bool->int
    #:make-keyword
    #:falsy
    #:truthy
    #:alambda
    #:aif
    #:acond
    #:package-designator
    #:approx-equal
    #:factorial
    #:choose
    #:permute
    #:prime
    #:primep
    #:phi
    #:fib))
(in-package :zpcalc/util)

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
;; log_base (num)
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

;; tries to preserve exactness of x ^ (1 / z)
;; need to check if y exists such that y^z = x
;; z = log_y (x)
;; y ^ z = x
(defun invpow-exact (x z)
  (typecase x
    (integer
      (if (integerp z)
        (let ((approx (expt x (/ 1 z))))
          (if (= (expt (round approx) z) x)
            (round approx)
            (expt (->double x) (/ 1 (->double z)))))
        (expt (->double x) (/ 1 (->double z)))))
    (rational
      (if (integerp z)
        (let ((approx-num (expt (numerator x) (/ 1 z)))
              (approx-den (expt (denominator x) (/ 1 z))))
          (if (and (= (expt (round approx-num) z) (numerator x))
                   (= (expt (round approx-den) z) (denominator x)))
            (/ (round approx-num) (round approx-den))
            (expt (->double x) (/ 1 (->double z)))))
        (expt (->double x) (/ 1 (->double z)))))
    (t (expt (->double x) (/ 1 (->double z))))))

(defun expt-exact (a b)
  (if (and (or (integerp a) (rationalp a)) (integerp b))
    (expt a b)
    (expt (->double a) (->double b))))

;; Taken from rosetta code for approximate equality
(defun approx-equal (float1 float2 &optional (threshold 0.000001))
  "Determine whether float1 and float2 are equal; THRESHOLD is the
maximum allowable difference between normalized significands of floats
with the same exponent. The significands are scaled appropriately
before comparison for floats with different exponents."
  (multiple-value-bind (sig1 exp1 sign1) (decode-float float1)
    (multiple-value-bind (sig2 exp2 sign2) (decode-float float2)
      (let ((cmp1 (float-sign sign1 (scale-float sig1 (floor (- exp1 exp2) 2))))
            (cmp2 (float-sign sign2 (scale-float sig2 (floor (- exp2 exp1) 2)))))
        (< (abs (- cmp1 cmp2)) threshold)))))

(defun falsy (x)
  (zerop x))

(defun truthy (x)
  (not (falsy x)))

(defun factorial (x)
  (check-type x integer)
  (if (< x 0)
    (error "~S is an invalid argument for factorial" x))
  (labels ((rec (x acc)
                (if (<= x 1)
                  acc
                  (rec (1- x) (* x acc)))))
    (rec x 1)))

;; binomial coefficient
;; aka number of ways to choose k items out of n
(defun choose (n k)
  (if (< n k)
    (error "~S and ~S are invalid arguments to choose" n k))
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;; number of ways to permute k items out of n
(defun permute (n k)
  (if (< n k)
    (error "~S and ~S are invalid arguments to permute" n k))
  (/ (factorial n) (*  (factorial (- n k)))))

;; nth fibonacci number using binet's formula
(defun fib (n)
  (if (< n 0)
    (error "~S is an invalid argument to fib" n))
  (round (/ (- (expt (/ (+ 1 (sqrt 5.0d0)) 2.0d0) n)
               (expt (/ (- 1 (sqrt 5.0d0)) 2.0d0) n)) (sqrt 5.0d0))))

;; totient function
;; copied algorithm from https://cp-algorithms.com/algebra/phi-function.html
(defun phi (n)
  (if (<= n 0)
    (error "~S is an invalid argument to totient" n))
  (let ((result n)
        (i 2))
    (loop while (<= (* i i) n)
          do (progn
               (when (= 0 (mod n i))
                 (loop while (= 0 (mod n i))
                       do (progn
                            (setf n (truncate (/ n i)))
                            (decf result (truncate (/ result i))))))
               (incf i)))
    (when (> n 1)
      (decf result (truncate (/ result n))))
    result))

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
