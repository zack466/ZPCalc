(in-package :cl-user)

(defpackage rpncalc
  (:use :cl :rpncalc/util :rpncalc/state :rpncalc/conditions)
  (:export #:main))
(in-package :rpncalc)

;; (defun main ()
;;   (multiple-value-bind (state ret)
;;       (run! nil (do! (push! 10)
;;                      (return! 20)
;;                   ))
;;     (print ret)
;;     (print state)))

;; environment:
;;   functions: symbol -> action
(defvar *functions* (make-hash-table :test #'equal))
;;   variables $symbol -> value to be pushed onto stack
(defvar *variables* (make-hash-table))
;; pi, e, phi, i
;; min/max fixnum, float epislon, etc
;; plus variations like 2pi, pi2, pi3, 2pi3, etc

;; def main:
;; loop until end of input:
;;   match
;;     number -> push number onto stack
;;     symbol -> if symbol has definition:
;;                 apply definition to stack, rollback if error
;;               else
;;                 error - undefined symbol
;;     quote symbol -> push symbol onto stack
;;     def -> match list
;;              (def word &rest body) -> word will now expand into each term in its body
;;              (def (fn-name &rest args) &rest body) -> fn-name will pop off args and then expand into body
;;                                                       args can be referred to by prepending "$"

(defun apply! (symbol)
  (lambda (s)
    (let ((body (gethash (symbol-name symbol) *functions*)))
      (if (null body)
          (error 'rpn-undefined-function :name symbol)
          (funcall body s)))))

(defun >>> (first &rest rest)
  (labels ((rec (acc rest)
             (if (null rest)
                 acc
                 (rec (>> acc (car rest)) (cdr rest)))))
    (rec first rest)))

(defun produce-action (input)
  (cond
    ;; number
    ((numberp input)
     (push! input))
    ;; boolean
    ((or (eq input t) (eq input nil))
     (push! input))
    ;; symbol
    ((symbolp input)
     (apply! input)) 
    ;; quoted symbol
    ((and (listp input)
          (eq (car input) 'quote)
          (symbolp (cadr input)))
     (push! (cadr input)))
    ;; symbol/function
    ((and (listp input)
          (eq (car input) 'def))
     (cond
       ;; symbol function - simply gets expanded
       ((symbolp (cadr input))
        (>>> (mapcar #'produce-action (cddr input))))
       ;; function function - take arguments into account
       ;; ((listp (cadr input))
       ;;  (destructuring-bind (fn-name &rest args)
       ;;      (cadr input)
       ;;    (create-environment
       ;;      (>>> (cddr input)))))
       )
     )
    (t (error 'rpn-unsupported-element :element input))
    ))

(defun apply-unary! (op)
  (do! (a <- pop!)
       (push! (funcall op a))))

(defun apply-binary! (op)
  (do! (a <- pop!)
       (b <- pop!)
       (push! (funcall op b a))))

(defun bool->int (b)
  (if b 1 0))

(defun init-builtin-functions ()
  ;; numerical operations
  (setf (gethash "+" *functions*) (apply-binary! #'+))
  (setf (gethash "-" *functions*) (apply-binary! #'-))
  (setf (gethash "*" *functions*) (apply-binary! #'*))
  (setf (gethash "/" *functions*) (apply-binary! #'/))
  (setf (gethash "_" *functions*) (apply-unary! #'-))

  ;; stack primitives
  (setf (gethash "DROP" *functions*) (drop!))
  (setf (gethash "SWAP" *functions*) (do! (a <- pop!) (b <- pop!) (push! a) (push! b)))
  (setf (gethash "DUP" *functions*) (dup!))

  ;; boolean operations
  (setf (gethash "NOT" *functions*) (apply-unary! #'(lambda (a) (if (zerop a) 1 0))))
  (setf (gethash "AND" *functions*) (apply-binary! #'logand))
  (setf (gethash "OR" *functions*) (apply-binary! #'logior))
  (setf (gethash "XOR" *functions*) (apply-binary! #'logxor))
  (setf (gethash "NAND" *functions*) (apply-binary! #'lognand))
  (setf (gethash "NOR" *functions*) (apply-binary! #'lognor))
  (setf (gethash "BIT" *functions*) (apply-binary! #'logbitp))
  (setf (gethash "<<" *functions*) (apply-binary! #'ash))
  (setf (gethash ">>" *functions*) (apply-binary! #'(lambda (integer count) (ash integer (- count)))))

  ;; conditonals - 0 is false, everything else is true (but 1 is preferred)
  (setf (gethash "IF" *functions*) (do! (test <- pop!)
                                       (a <- pop!)
                                       (b <- pop!)
                                       (push! (if (not (zerop (test))) a b))))
  (setf (gethash "ZEROP" *functions*) (apply-unary! (compose #'bool->int #'zerop )))
  (setf (gethash "ONEP" *functions*) (apply-unary! #'(lambda (x) (bool->int (zerop (1- x))))))
  (setf (gethash "PLUSP" *functions*) (apply-unary! (compose #'bool->int #'plusp)))
  (setf (gethash "MINUSP" *functions*) (apply-unary! (compose #'bool->int #'minusp)))
  (setf (gethash "EVENP" *functions*) (apply-unary! (compose #'bool->int #'evenp)))
  (setf (gethash "ODDP" *functions*) (apply-unary! (compose #'bool->int #'oddp)))
  (setf (gethash ">" *functions*) (apply-binary! (compose #'bool->int #'>)))
  (setf (gethash ">=" *functions*) (apply-binary! (compose #'bool->int #'>=)))
  (setf (gethash "<" *functions*) (apply-binary! (compose #'bool->int #'<)))
  (setf (gethash "<=" *functions*) (apply-binary! (compose #'bool->int #'<=)))
  (setf (gethash "=" *functions*) (apply-binary! (compose #'bool->int #'=)))

  ;; misc functions
  (setf (gethash "INV" *functions*) (apply-unary! #'(lambda (x) (/ 1 x))))
  (setf (gethash "MAX" *functions*) (apply-binary! #'max))
  (setf (gethash "MIN" *functions*) (apply-binary! #'min))
  (setf (gethash "GCD" *functions*) (apply-binary! #'gcd))
  (setf (gethash "LCM" *functions*) (apply-binary! #'lcm))
  (setf (gethash "ABS" *functions*) (apply-unary! #'abs))
  (setf (gethash "SIGNUM" *functions*) (apply-unary! #'signum))
  (setf (gethash "FLOOR" *functions*) (apply-unary! #'floor))
  (setf (gethash "CEILING" *functions*) (apply-unary! #'ceiling))
  (setf (gethash "TRUNCATE" *functions*) (apply-unary! #'truncate))
  (setf (gethash "ROUND" *functions*) (apply-unary! #'round))
  (setf (gethash "FFLOOR" *functions*) (apply-unary! #'ffloor))
  (setf (gethash "FCEILING" *functions*) (apply-unary! #'fceiling))
  (setf (gethash "FTRUNCATE" *functions*) (apply-unary! #'ftruncate))
  (setf (gethash "FROUND" *functions*) (apply-unary! #'fround))
  (setf (gethash "MOD" *functions*) (apply-binary! #'mod))
  (setf (gethash "REM" *functions*) (apply-binary! #'rem))
  (setf (gethash "RANDOM" *functions*) (apply-unary! #'random)) ;; random value between 0 and arg
  (setf (gethash "RAND" *functions*) (push! (random (+ 1.0f0 single-float-epsilon)))) ;; random value between 0.0 and 1.0

  ;; complex numbers
  (setf (gethash "CONJUGATE" *functions*) (apply-unary! #'conjugate))
  (setf (gethash "PHASE" *functions*) (apply-unary! #'phase))

  ;; irrational & trig
  (setf (gethash "EXP" *functions*) (apply-unary! #'exp))
  (setf (gethash "EXPT" *functions*) (apply-binary! #'expt))
  (setf (gethash "LN" *functions*) (apply-unary! #'(lambda (x) (log x))))
  (setf (gethash "LOG" *functions*) (apply-binary! #'log))
  (setf (gethash "SQRT" *functions*) (apply-unary! #'sqrt))
  (setf (gethash "ISQRT" *functions*) (apply-unary! #'isqrt))
  (setf (gethash "SIN" *functions*) (apply-unary! #'sin))
  (setf (gethash "COS" *functions*) (apply-unary! #'cos))
  (setf (gethash "TAN" *functions*) (apply-unary! #'tan))
  (setf (gethash "ASIN" *functions*) (apply-unary! #'asin))
  (setf (gethash "ACOS" *functions*) (apply-unary! #'acos))
  (setf (gethash "ATAN" *functions*) (apply-unary! #'atan))
  (setf (gethash "ATAN2" *functions*) (apply-binary! #'atan))
  (setf (gethash "CIS" *functions*) (apply-unary! #'cis))
  (setf (gethash "SINH" *functions*) (apply-unary! #'sinh))
  (setf (gethash "COSH" *functions*) (apply-unary! #'cosh))
  (setf (gethash "TANH" *functions*) (apply-unary! #'tanh))
  (setf (gethash "ASINH" *functions*) (apply-unary! #'asinh))
  (setf (gethash "ACOSH" *functions*) (apply-unary! #'acosh))
  (setf (gethash "ATANH" *functions*) (apply-unary! #'atanh))

  ;; type conversions/constructors
  (setf (gethash "FLOAT" *functions*) (apply-unary! #'float))
  (setf (gethash "RATIONAL" *functions*) (apply-unary! #'rational))
  (setf (gethash "NUMERATOR" *functions*) (apply-unary! #'numerator))
  (setf (gethash "DENOMINATOR" *functions*) (apply-unary! #'denominator))
  (setf (gethash "COMPLEX" *functions*) (apply-binary! #'complex))
  (setf (gethash "REALPART" *functions*) (apply-unary! #'realpart))
  (setf (gethash "IMAGPART" *functions*) (apply-unary! #'imagpart))
  )

(defun main ()
  (let ((stack nil))
    (init-builtin-functions)
    (handler-case
        (loop
          (handler-case
              (progn
                (format t "~S~%" stack)
                (let ((input (read)))
                  (let ((action (produce-action input)))
                    (setf stack (run! stack action)))))
            (rpn-stack-empty () (format t "Error: Not enough elements on stack~%"))
            (rpn-unsupported-element (e) (format t "Unsupported element: ~a~%" (rpn-element e)))
            (rpn-undefined-function (e) (format t "Undefined Function: ~a~%" (rpn-name e)))
            (type-error (e) (format t "Type Error: ~a~%" e))
            (end-of-file (e) (signal e)) ;; pass the condition 
            (error (e) (format t "Error: ~a~%" e))
            ))
      (end-of-file () nil))))
