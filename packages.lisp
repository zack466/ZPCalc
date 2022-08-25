(in-package :cl-user)

(defpackage zpcalc/packages
  (:use :cl)
  (:import-from
    #:zpcalc/util
    #:compose
    #:bool->int
    #:truthy
    #:falsy
    #:square
    #:is-isquare
    #:is-pow
    #:log-exact
    #:expt-exact
    #:sqrt-exact
    #:->double
    #:bool->int)
  (:import-from
    #:zpcalc/actions
    #:apply-unary!
    #:apply-binary!
    #:pop!
    #:push!
    #:dup!
    #:roll!
    #:unroll!
    #:do!
    #:set!
    #:return!)
  (:export
    #:*all-packages*
    #:*builtins*))
(in-package :zpcalc/packages)

(defvar *all-packages* (make-hash-table))

;; environment :: symbol -> action
(defvar *builtins* (make-hash-table))

;; Builtins
;; numerical operations
(setf (gethash :+ *builtins*) (apply-binary! #'+))
(setf (gethash :- *builtins*) (apply-binary! #'-))
(setf (gethash :* *builtins*) (apply-binary! #'*))
(setf (gethash :/ *builtins*) (apply-binary! #'/))
(setf (gethash :// *builtins*) (apply-binary! (compose #'floor #'/))) ;; int division
(setf (gethash :_ *builtins*) (apply-unary! #'-))
(setf (gethash :NEG *builtins*) (apply-unary! #'-))
(setf (gethash :INC *builtins*) (apply-unary! #'1+))
(setf (gethash :DEC *builtins*) (apply-unary! #'1-))

;; stack primitives
(setf (gethash :DROP *builtins*) (pop!))
(setf (gethash :POP *builtins*) (pop!))
(setf (gethash :SWAP *builtins*) (do! (a <- pop!) (b <- pop!) (push! a) (push! b)))
(setf (gethash :DUP *builtins*) (dup!))
(setf (gethash :ROT *builtins*) (do! (a <- pop!) (b <- pop!) (c <- pop!)
                                     (push! a) (push! c) (push! b)))
(setf (gethash :ROLL *builtins*) (roll!))
(setf (gethash :UNROLL *builtins*) (unroll!))

;; Bitwise operations (two's complement)
(setf (gethash :LNOT *builtins*) (apply-unary! #'lognot))
(setf (gethash :LAND *builtins*) (apply-binary! #'logand))
(setf (gethash :LOR *builtins*) (apply-binary! #'logior))
(setf (gethash :LXOR *builtins*) (apply-binary! #'logxor))
(setf (gethash :LNAND *builtins*) (apply-binary! #'lognand))
(setf (gethash :LNOR *builtins*) (apply-binary! #'lognor))
(setf (gethash :BIT *builtins*) (apply-binary! #'logbitp))
(setf (gethash :<< *builtins*) (apply-binary! #'ash))
(setf (gethash :>> *builtins*) (apply-binary! #'(lambda (integer count) (ash integer (- count)))))

;; conditonals - 0 is false, everything else is true (but 1 is preferred)
(setf (gethash :SWITCH *builtins*) (do! (test <- pop!) (a <- pop!) (b <- pop!) (push! (if (truthy test) a b))))
(setf (gethash :NOT *builtins*) (apply-unary! (compose #'bool->int #'falsy)))
(setf (gethash :AND *builtins*) (apply-binary! (lambda (x y) (bool->int (and (truthy x) (truthy y))))))
(setf (gethash :OR *builtins*) (apply-binary! (lambda (x y) (bool->int (or (truthy x) (truthy y))))))
(setf (gethash :XOR *builtins*) (apply-binary! (lambda (x y) (logxor (bool->int (truthy x)) (bool->int (truthy y))))))
(setf (gethash :NAND *builtins*) (apply-binary! (lambda (x y) (bool->int (not (and (truthy x) (truthy y)))))))
(setf (gethash :NOR *builtins*) (apply-binary! (lambda (x y) (bool->int (not (or (truthy x) (truthy y)))))))
(setf (gethash :TRUEP *builtins*) (apply-unary! (compose #'bool->int #'truthy)))
(setf (gethash :FALSEP *builtins*) (apply-unary! (compose #'bool->int #'falsy)))
(setf (gethash :ZEROP *builtins*) (apply-unary! (compose #'bool->int #'zerop)))
(setf (gethash :ONEP *builtins*) (apply-unary! #'(lambda (x) (bool->int (zerop (1- x))))))
(setf (gethash :PLUSP *builtins*) (apply-unary! (compose #'bool->int #'plusp)))
(setf (gethash :MINUSP *builtins*) (apply-unary! (compose #'bool->int #'minusp)))
(setf (gethash :EVENP *builtins*) (apply-unary! (compose #'bool->int #'evenp)))
(setf (gethash :ODDP *builtins*) (apply-unary! (compose #'bool->int #'oddp)))
(setf (gethash :> *builtins*) (apply-binary! (compose #'bool->int #'>)))
(setf (gethash :>= *builtins*) (apply-binary! (compose #'bool->int #'>=)))
(setf (gethash :< *builtins*) (apply-binary! (compose #'bool->int #'<)))
(setf (gethash :<= *builtins*) (apply-binary! (compose #'bool->int #'<=)))
(setf (gethash := *builtins*) (apply-binary! (compose #'bool->int #'=)))

;; misc functions
(setf (gethash :INV *builtins*) (apply-unary! #'(lambda (x) (/ 1 x))))
(setf (gethash :MAX *builtins*) (apply-binary! #'max))
(setf (gethash :MIN *builtins*) (apply-binary! #'min))
(setf (gethash :GCD *builtins*) (apply-binary! #'gcd))
(setf (gethash :LCM *builtins*) (apply-binary! #'lcm))
(setf (gethash :ABS *builtins*) (apply-unary! #'abs))
(setf (gethash :SIGNUM *builtins*) (apply-unary! #'signum))
(setf (gethash :FLOOR *builtins*) (apply-unary! #'floor))
(setf (gethash :CEILING *builtins*) (apply-unary! #'ceiling))
(setf (gethash :TRUNCATE *builtins*) (apply-unary! #'truncate))
(setf (gethash :ROUND *builtins*) (apply-unary! #'round))
(setf (gethash :MOD *builtins*) (apply-binary! #'mod))
(setf (gethash :REM *builtins*) (apply-binary! #'rem))
(setf (gethash :RANDOM *builtins*) (apply-unary! #'random)) ;; random value between 0 and arg
(setf (gethash :RAND *builtins*) (push! (random (+ 1.0d0 double-float-epsilon)))) ;; random value between 0.0 and 1.0
(setf (gethash :SQUARE *builtins*) (apply-unary! #'square))
(setf (gethash :CUBE *builtins*) (apply-unary! (lambda (x) (* x x x))))
(setf (gethash :ISQRT *builtins*) (apply-unary! #'isqrt))

;; irrational operations that try to preserve exactness
(setf (gethash :POW *builtins*) (apply-binary! #'expt-exact))
(setf (gethash :SQRT *builtins*) (apply-unary! #'sqrt-exact))
(setf (gethash :LOG *builtins*) (apply-binary! #'log-exact))
(setf (gethash :LG *builtins*) (apply-unary! #'(lambda (x) (log-exact x 2))))
(setf (gethash :LOG10 *builtins*) (apply-unary! #'(lambda (x) (log-exact x 10))))

;; irrational & trig - all will result in a double-float
(setf (gethash :EXP *builtins*) (apply-unary! (compose #'exp #'->double)))
(setf (gethash :LN *builtins*) (apply-unary! (compose #'log #'->double)))
(setf (gethash :SIN *builtins*) (apply-unary! (compose #'sin #'->double)))
(setf (gethash :COS *builtins*) (apply-unary! (compose #'cos #'->double)))
(setf (gethash :TAN *builtins*) (apply-unary! (compose #'tan #'->double)))
(setf (gethash :ASIN *builtins*) (apply-unary! (compose #'asin #'->double)))
(setf (gethash :ACOS *builtins*) (apply-unary! (compose #'acos #'->double)))
(setf (gethash :ATAN *builtins*) (apply-unary! (compose #'atan #'->double)))
(setf (gethash :ATAN2 *builtins*) (apply-binary! (lambda (a b) (atan (->double a) (->double b)))))
(setf (gethash :CIS *builtins*) (apply-unary! (compose #'cis #'->double)))
(setf (gethash :SINH *builtins*) (apply-unary! (compose #'sinh #'->double)))
(setf (gethash :COSH *builtins*) (apply-unary! (compose #'cosh #'->double)))
(setf (gethash :TANH *builtins*) (apply-unary! (compose #'tanh #'->double)))
(setf (gethash :ASINH *builtins*) (apply-unary! (compose #'asinh #'->double)))
(setf (gethash :ACOSH *builtins*) (apply-unary! (compose #'acosh #'->double)))
(setf (gethash :ATANH *builtins*) (apply-unary! (compose #'atanh #'->double)))

;; type conversions/constructors
(setf (gethash :FLOAT *builtins*) (apply-unary! #'(lambda (x) (float x 1.0d0))))
(setf (gethash :RATIONAL *builtins*) (apply-unary! #'rational))
(setf (gethash :NUMERATOR *builtins*) (apply-unary! #'numerator))
(setf (gethash :DENOMINATOR *builtins*) (apply-unary! #'denominator))
(setf (gethash :COMPLEX *builtins*) (apply-binary! #'complex))
(setf (gethash :CONJUGATE *builtins*) (apply-unary! #'conjugate))
(setf (gethash :PHASE *builtins*) (apply-unary! #'phase))
(setf (gethash :REALPART *builtins*) (apply-unary! #'realpart))
(setf (gethash :IMAGPART *builtins*) (apply-unary! #'imagpart))

;; constants
(setf (gethash :PI *builtins*) (push! pi))
(setf (gethash :PHI *builtins*) (push! (/ 2 (+ 1 (sqrt 5.0d0)))))
(setf (gethash :E *builtins*) (push! (exp 1.0d0)))
(setf (gethash :I *builtins*) (push! #C(0 1)))
(setf (gethash :TRUE *builtins*) (push! 1))
(setf (gethash :FALSE *builtins*) (push! 0))

;; special functions
(setf (gethash :CLEAR *builtins*) (set! nil))
(setf (gethash :ID *builtins*) (return!)) ;; do nothing
(setf (gethash :ERROR *builtins*) (lambda (s) (declare (ignorable s)) (signal "oopsie")))

;; Initial packages
(setf (gethash :user *all-packages*) (make-hash-table))
(setf (gethash :builtins *all-packages*) *builtins*)
