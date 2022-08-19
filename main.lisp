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

;; is a Zipper - car contains undo history, cdr contains redo
;; cannot undo function definitions, only state of the stack
(defvar *history* (cons nil nil))

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

;; binds together actions using >>
;; can't use reduce bc >> is a macro...
(defun >>> (actions)
  (labels ((rec (acc rest)
             (if (null rest)
                 acc
                 (rec (>> acc (car rest)) (cdr rest)))))
    (rec (car actions) (cdr actions))))

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
          (symbol= (car input) 'def))
     (cond
       ;; symbol function - simply gets expanded
       ((symbolp (cadr input))
        (setf (gethash (symbol-name (cadr input)) *functions*) (>>> (mapcar #'produce-action (cddr input))))
        (return!))
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

(defun ->double (x)
  (coerce x 'double-float))

(defun init-builtin-functions ()
  ;; numerical operations
  (setf (gethash "+" *functions*) (apply-binary! #'+))
  (setf (gethash "-" *functions*) (apply-binary! #'-))
  (setf (gethash "*" *functions*) (apply-binary! #'*))
  (setf (gethash "/" *functions*) (apply-binary! #'/))
  (setf (gethash "//" *functions*) (apply-binary! (compose #'floor #'/))) ;; int division
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
                                       (push! (if (not (zerop test)) a b))))
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
  (setf (gethash "MOD" *functions*) (apply-binary! #'mod))
  (setf (gethash "REM" *functions*) (apply-binary! #'rem))
  (setf (gethash "RANDOM" *functions*) (apply-unary! #'random)) ;; random value between 0 and arg
  (setf (gethash "RAND" *functions*) (push! (random (+ 1.0d0 double-float-epsilon)))) ;; random value between 0.0 and 1.0

  ;; complex numbers
  (setf (gethash "CONJUGATE" *functions*) (apply-unary! #'conjugate))
  (setf (gethash "PHASE" *functions*) (apply-unary! #'phase))

  ;; irrational & trig
  ;; all will result in a double-float (except isqrt)
  (setf (gethash "EXP" *functions*) (apply-unary! (compose #'->double #'exp)))
  (setf (gethash "EXPT" *functions*) (apply-binary! (compose #'->double #'expt)))
  (setf (gethash "LN" *functions*) (apply-unary! #'(lambda (x) (log (->double x)))))
  (setf (gethash "LOG" *functions*) (apply-binary! (compose #'->double #'log)))
  (setf (gethash "SQRT" *functions*) (apply-unary! (compose #'->double #'sqrt)))
  (setf (gethash "ISQRT" *functions*) (apply-unary! #'isqrt))
  (setf (gethash "SIN" *functions*) (apply-unary! (compose #'->double #'sin)))
  (setf (gethash "COS" *functions*) (apply-unary! (compose #'->double #'cos)))
  (setf (gethash "TAN" *functions*) (apply-unary! (compose #'->double #'tan)))
  (setf (gethash "ASIN" *functions*) (apply-unary! (compose #'->double #'asin)))
  (setf (gethash "ACOS" *functions*) (apply-unary! (compose #'->double #'acos)))
  (setf (gethash "ATAN" *functions*) (apply-unary! (compose #'->double #'atan)))
  (setf (gethash "ATAN2" *functions*) (apply-binary! (compose #'->double #'atan)))
  (setf (gethash "CIS" *functions*) (apply-unary! (compose #'->double #'cis)))
  (setf (gethash "SINH" *functions*) (apply-unary! (compose #'->double #'sinh)))
  (setf (gethash "COSH" *functions*) (apply-unary! (compose #'->double #'cosh)))
  (setf (gethash "TANH" *functions*) (apply-unary! (compose #'->double #'tanh)))
  (setf (gethash "ASINH" *functions*) (apply-unary! (compose #'->double #'asinh)))
  (setf (gethash "ACOSH" *functions*) (apply-unary! (compose #'->double #'acosh)))
  (setf (gethash "ATANH" *functions*) (apply-unary! (compose #'->double #'atanh)))

  ;; type conversions/constructors
  (setf (gethash "FLOAT" *functions*) (apply-unary! #'(lambda (x) (float x 1.0d0))))
  (setf (gethash "RATIONAL" *functions*) (apply-unary! #'rational))
  (setf (gethash "NUMERATOR" *functions*) (apply-unary! #'numerator))
  (setf (gethash "DENOMINATOR" *functions*) (apply-unary! #'denominator))
  (setf (gethash "COMPLEX" *functions*) (apply-binary! #'complex))
  (setf (gethash "REALPART" *functions*) (apply-unary! #'realpart))
  (setf (gethash "IMAGPART" *functions*) (apply-unary! #'imagpart))

  ;; constants
  (setf (gethash "PI" *functions*) (push! pi))
  (setf (gethash "E" *functions*) (push! (exp 1.0d0)))
  (setf (gethash "I" *functions*) (push! #C(0 1)))
  )

(defun print-stack (stack)
  (format t "~{-~*~}~%" (loop for i from 0 to 30 collect i))
  (mapcar #'(lambda (x)
              (typecase x
                (fixnum (format t "~30:<~D~>~%" x))
                (complex (format t "~30:<~a + ~ai~>~%" (realpart x) (imagpart x)))
                (number (format t "~30:<~a~>~%" x))
                ;; (number (format t "~30:<~30,10E~>~%" x))
                (t (format t "~20:<~a~>~%" x))))
          (reverse stack))
  (when (null stack)
    (format t "Empty Stack~%"))
  (format t "~{-~*~}~%" (loop for i from 0 to 30 collect i)))

(defun main ()
  (let ((stack nil)
        (*read-default-float-format* 'double-float))
    (init-builtin-functions)
    (handler-case
        (loop
          (handler-case
              (progn
                (print-stack stack)
                (format t "history: ~S~%" *history*)
                (format t "> ") (finish-output)
                (let ((input (read)))
                  (cond
                    ;; UNDO
                    ((symbol= input 'undo)
                     (when (null (car *history*))
                       (error 'rpn-cannot-undo))
                     (push stack (cdr *history*))
                     (setf stack (pop (car *history*))))
                    ;; REDO
                    ((symbol= input 'redo)
                     (when (null (cdr *history*))
                       (error 'rpn-cannot-redo))
                     (push stack (car *history*))
                     (setf stack (pop (cdr *history*))))
                    ;; Otherwise
                    (t (let ((action (produce-action input)))
                         (push stack (car *history*))
                         (setf (cdr *history*) nil)
                         (setf stack (run! stack action)))))))
            (rpn-cannot-undo () (format t "Error: Nothing to undo~%"))
            (rpn-cannot-redo () (format t "Error: Nothing to redo~%"))
            (rpn-stack-empty () (format t "Error: Not enough elements on stack~%"))
            (rpn-unsupported-element (e) (format t "Unsupported element: ~a~%" (rpn-element e)))
            (rpn-undefined-function (e) (format t "Undefined Function: ~a~%" (rpn-name e)))
            (type-error (e) (format t "Type Error: ~a~%" e))
            (end-of-file (e) (signal e)) ;; pass the condition 
            (error (e) (format t "Error: ~a~%" e))
            ))
      (end-of-file () nil))))
