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
  (loop for test in (reverse *tests*)
        do (progn
             (handler-case
               (progn (funcall (cdr test))
                      (format t "Test Passed: ~S~%" (car test)))
               (t (e) (format t "Test Failed: ~S ~%~4t~a~%" (car test) e)))))
  (format t "Finished testing.~%"))

;; we are comparing to the reversed stack since my mental model is (right=top of stack)
(defmacro test-stack (name interaction expected)
  `(deftest ,name
    (let ((state (make-instance 'Calc)))
      (calc-interact state ,interaction)
      (assert (equal ,expected (reverse (calc-stack state)))))))

;; TODO: property testing?

;; Basic operations
(test-stack push '(1 2 3 4 5) '(1 2 3 4 5))
(test-stack + '(2 3 +) '(5))
(test-stack - '(2 3 -) '(-1))
(test-stack * '(2 3 *) '(6))
(test-stack / '(5 8 /) '(5/8))
(test-stack // '(22 4 //) '(5))
(test-stack neg '(4 neg) '(-4))
(test-stack inc '(4 inc inc) '(6))
(test-stack dec '(4 dec dec) '(2))

;; Stack manip
(test-stack drop '(4 5 drop) '(4))
(test-stack pop '(4 5 pop) '(4))
(test-stack swap '(2 3 4 swap) '(2 4 3))
(test-stack dup '(5 2 dup) '(5 2 2))
(test-stack rot '(1 2 3 4 rot) '(1 4 2 3))
(test-stack -rot '(1 2 3 4 -rot) '(1 3 4 2))
(test-stack roll '(1 2 3 4 roll) '(4 1 2 3))
(test-stack -roll '(1 2 3 4 -roll) '(2 3 4 1))

;; Bitwise ops
(test-stack lnot '(9 lnot) '(-10))
(test-stack land '(#b1100 #b1010 land) '(#b1000))
(test-stack lor '(#b1100 #b1010 lor) '(#b1110))
(test-stack lxor '(#b1100 #b1010 lxor) '(#b0110))
(test-stack lnand '(#b1100 #b1010 lnand) '(-9))
(test-stack lnor '(#b1100 #b1010 lnor) '(-15))
(test-stack bit '(1 #b1010 bit 1 #b0101 bit) '(1 0))
(test-stack >> '(15 2 >>) '(3))
(test-stack << '(15 2 <<) '(60))

;; Conditionals
(test-stack switch-true '(1 2 true switch) '(2))
(test-stack switch-false '(1 2 false switch) '(1))
(test-stack not-true '(20 not) '(0))
(test-stack not-false '(0 not) '(1))
(test-stack and '(0 0 and 1 0 and 0 1 and 1 1 and) '(0 0 0 1))
(test-stack or '(0 0 or 1 0 or 0 1 or 1 1 or) '(0 1 1 1))
(test-stack xor '(0 0 xor 1 0 xor 0 1 xor 1 1 xor) '(0 1 1 0))
(test-stack nand '(0 0 nand 1 0 nand 0 1 nand 1 1 nand) '(1 1 1 0))
(test-stack nor '(0 0 nor 1 0 nor 0 1 nor 1 1 nor) '(1 0 0 0))
(test-stack falsep '(0 falsep 1 falsep) '(1 0))
(test-stack truep '(0 truep 1 truep) '(0 1))
(test-stack plusp '(-1 plusp 0 plusp 1 plusp) '(0 0 1))
(test-stack zerop '(0 zerop 1 zerop) '(1 0))
(test-stack minusp '(-1 minusp 0 minusp 1 minusp) '(1 0 0))
(test-stack evenp '(1 evenp 2 evenp) '(0 1))
(test-stack oddp '(1 oddp 2 oddp) '(1 0))
(test-stack > '(-2 2 > 2 -2 > 2 2 >) '(0 1 0))
(test-stack >= '(-2 2 >= 2 -2 >= 2 2 >=) '(0 1 1))
(test-stack < '(-2 2 < 2 -2 < 2 2 <) '(1 0 0))
(test-stack <= '(-2 2 <= 2 -2 <= 2 2 <=) '(1 0 1))
(test-stack = '(1 2 = 1 1 =) '(0 1))
(test-stack approx '(2 1 approx 2 2 approx 2 2.00000004 approx) '(0 1 1))

;; misc math
(test-stack inv '(1 inv 2 inv) '(1 1/2))
(test-stack max '(0 1 max 2 1 max) '(1 2))
(test-stack min '(0 1 min 2 1 min) '(0 1))
(test-stack gcd '(7 5 gcd 90 165 gcd) '(1 15))
(test-stack lcm '(7 5 lcm 90 165 lcm) '(35 990))
(test-stack abs '(-2 abs 2 abs) '(2 2))
(test-stack signum '(-10 signum 0 signum 10 signum) '(-1 0 1))
(test-stack floor '(1.1 floor -2.3 floor) '(1 -3))
(test-stack ceiling '(1.1 ceiling -2.7 ceiling) '(2 -2))
(test-stack truncate '(1.1 truncate -2.7 truncate) '(1 -2))
(test-stack round '(1.1 round -2.7 round) '(1 -3))
(test-stack mod '(9 4 mod -1 3 mod) '(1 2))
(test-stack rem '(9 4 rem -4 3 rem) '(1 -1))
;; Can't test random or rand lol
(test-stack square '(-2 square 4 square) '(4 16))
(test-stack cube '(-2 cube 4 cube) '(-8 64))
(test-stack isqrt '(4 isqrt 22 isqrt) '(2 4))
(test-stack fib '(1 fib 10 fib) '(1 55))
(test-stack fact '(10 fact) '(3628800))
(test-stack prime '(10 prime) '(29))
(test-stack totient '(3198 totient) '(960))
(test-stack choose '(10 2 choose) '(45))
(test-stack permute '(10 2 permute) '(90))

;; irrational, tries to preserve exactness
(test-stack pow '(2 3 pow 2.5 2.5 pow) `(8 ,(expt 2.5d0 2.5d0)))
(test-stack invpow '(8/27 3 invpow) `(2/3))
(test-stack sqrt '(16 sqrt 30 sqrt) `(4 ,(sqrt 30.0d0)))
(test-stack log '(100 10 log 22 5 log) `(2 ,(log 22.0d0 5.0d0)))
(test-stack lg '(1024 lg 22 lg) `(10 ,(log 22.0d0 2.0d0)))
(test-stack log10 '(1000 log10 22 log10) `(3 ,(log 22.0d0 10.0d0)))

;; Irrational functions
;; These all just call builtin common lisp functions, so I'm not gonna
;; bother testing them (since I'll just be calling the same functions myself)

;; Type conversions/constructors
(test-stack float '(2/3 float) '(0.6666666666666666d0))
(test-stack rational '(0.5 rational) '(1/2))
(test-stack numerator '(5/8 numerator) '(5))
(test-stack denominator '(5/8 denominator) '(8))
(test-stack complex '(1 2 complex) '(#C(1 2)))
(test-stack conjugate '(1 2 complex) '(#C(1 2)))
(test-stack phase '(#C(1 1) phase) `(,(/ pi 4)))
(test-stack realpart '(#C(1 2) realpart) '(1))
(test-stack imagpart '(#C(1 2) imagpart) '(2))

;; Misc
(test-stack clear '(1 2 3 clear) '())
(test-stack id '(1 2 3 id) '(1 2 3))
(test-stack sto/rcl '(1 2 sto 3 rcl) '(1 2 3 2))
(test-stack eval '(70 80 'gcd eval) '(10))

;; top level actions and special constructs
(deftest undo
  (let ((state (make-instance 'Calc)))
    (calc-interact state '(1 2 3))
    (calc-interact state '+)
    (calc-undo state)
    (assert (equal '(1 2 3) (reverse (calc-stack state))))))

(deftest redo
  (let ((state (make-instance 'Calc)))
    (calc-interact state '(1 2 3))
    (calc-interact state '+)
    (calc-undo state)
    (calc-redo state)
    (assert (equal '(1 5) (reverse (calc-stack state))))))

(deftest def-symbol
  (let ((state (make-instance 'Calc)))
    (calc-interact state '((def foo 20 +)))
    (calc-interact state '(-20 foo))
    (assert (equal '(0) (reverse (calc-stack state))))))

(deftest def-function
  (let ((state (make-instance 'Calc)))
    (calc-interact state '((def (foo x y) :x :x * :y *)))
    (calc-interact state '(2 3 foo))
    (assert (equal '(12) (reverse (calc-stack state))))))

(deftest store
  (let ((state (make-instance 'Calc)))
    (calc-interact state '(20 (store x) drop))
    (calc-interact state '(:x))
    (assert (equal '(20) (reverse (calc-stack state))))))

(deftest if-true
  (let ((state (make-instance 'Calc)))
    (calc-interact state '((if (2 3 <) (1) (2))))
    (assert (equal '(1) (reverse (calc-stack state))))))

(deftest if-false
  (let ((state (make-instance 'Calc)))
    (calc-interact state '((if (2 3 >) (1) (2))))
    (assert (equal '(2) (reverse (calc-stack state))))))

(deftest while
  (let ((state (make-instance 'Calc)))
    (calc-interact state '(1 sto (while (rcl 10 <=) rcl * rcl inc sto drop)))
    (assert (equal '(3628800) (reverse (calc-stack state))))))

(deftest in-package
  (let ((state (make-instance 'Calc)))
    (calc-interact state '((in-package baz)))
    (assert (equal :baz (calc-package state)))))

(deftest with-package
  (let ((state (make-instance 'Calc)))
    (calc-interact state '((with-package foo (def bar 11))))
    (calc-interact state '(foo.bar))
    (assert (equal '(11) (reverse (calc-stack state))))))

;; TODO: test example code

;; TODO: some more complex examples and edge cases

(run-tests)
