(in-package :cl-user)

(defpackage rpncalc
  (:use :cl :rpncalc/util :rpncalc/state :rpncalc/conditions)
  (:export #:main))
(in-package :rpncalc)

;; environment :: symbol -> action
(defvar *functions* (make-hash-table :test #'equal))
(defvar *user-functions* (make-hash-table :test #'equal))

;; pi, e, phi, i
;; min/max fixnum, float epislon, etc
;; plus variations like 2pi, pi2, pi3, 2pi3, etc

;; is a Zipper - car contains undo history, cdr contains redo
;; cannot undo function definitions, only state of the stack
(defvar *history* (cons nil nil))

(defun apply! (symbol)
  (lambda (s)
    (let ((body (gethash (symbol-name symbol) *functions*)))
      (let ((user-fn (gethash (symbol-name symbol) *user-functions*)))
        (if (null body)
          (if (null user-fn)
            (error 'rpn-undefined-function :name symbol)
            (funcall user-fn s))
          (funcall body s))))))

(defun var-get! (keyword env)
  (lambda (s)
    (let ((lookup (env-get env keyword)))
      (if lookup
        (funcall (push! lookup) s)
        (error 'rpn-undefined-variable :name keyword)))))

(defun var-set! (input value env)
  (lambda (s)
    (env-set env (make-keyword (symbol-name input)) value)
      (funcall (return!) s)))

(defun eval! ()
  (do! (top <- pop!)
       (let action (cond
                     ((symbolp top)
                      (apply! top))
                     ((listp top)
                      (>>> (mapcar (produce-action *variables*) top)))
                     (t (error 'rpn-cannot-eval :element top))))
       action))

(defun make-environment (&optional parent)
  (cons parent (make-hash-table)))

(defun env-set (env name value)
  (setf (gethash name (cdr env)) value))

(defun env-get (env name)
  (let ((result (gethash name (cdr env))))
    (if result
      result
      (if (null (car env))
        nil
        (env-get (car env) name)))))

(defvar *variables* (make-environment))

(defvar *reg* 0)

(defun sto! ()
  (do! (top <- top!)
       (let x (setf *reg* top))
       (return! x)))

(defun rcl! ()
  (push! *reg*))

;; binds together actions using >>
;; can't use reduce bc >> is a macro...
(defun >>> (actions)
  (labels ((rec (acc rest)
             (if (null rest)
                 acc
                 (rec (>> acc (car rest)) (cdr rest)))))
    (rec (car actions) (cdr actions))))

(defun produce-action (env)
  (lambda (input)
    (cond
      ;; number
      ((numberp input)
       (push! input))
      ;; variable
      ((keywordp input)
       (var-get! input env))
      ;; symbol
      ((symbolp input)
       (apply! input)) 
      ;; quoted symbol
      ((and (listp input)
            (eq (car input) 'quote)
            (symbolp (cadr input)))
       (push! (cadr input)))
      ;; quoted list - when evaluated, everything gets pushed onto stack
      ((and (listp input)
            (eq (car input) 'quote)
            (listp (cadr input)))
       (push! (cadr input)))
      ;; store variable
      ((and (listp input)
            (symbol= (car input) 'store)
            (symbolp (cadr input)))
       (do! (x <- top!) (var-set! (cadr input) x env)))
      ;; symbol/function
      ((and (listp input)
            (symbol= (car input) 'def))
       (cond
         ;; symbol function - simply gets expanded
         ((symbolp (cadr input))
          (setf (gethash (symbol-name (cadr input)) *user-functions*)
                (>>> (mapcar (produce-action env) (cddr input))))
          (return!))
         ;; function function - makes arguments available as lexical variables
         ((listp (cadr input))
          (destructuring-bind (fn-name &rest args)
            (cadr input)
            (let ((new-env (make-environment env)))
              (setf (gethash (symbol-name fn-name) *user-functions*)
                    (>>>
                      (append
                        ;; pop arguments off the stack, store them into variables
                        (mapcar #'(lambda (var) (do! (x <- pop!) (var-set! var x new-env))) (reverse args))
                        ;; the body of the function
                        (mapcar (produce-action new-env) (cddr input)))))
              (return!))))))
      (t (error 'rpn-unsupported-element :element input))
      )))

(defun apply-unary! (op)
  (do! (a <- pop!)
       (push! (funcall op a))))

(defun apply-binary! (op)
  (do! (a <- pop!)
       (b <- pop!)
       (push! (funcall op b a))))

(defun init-builtin-functions ()
  ;; numerical operations
  (setf (gethash "+" *functions*) (apply-binary! #'+))
  (setf (gethash "-" *functions*) (apply-binary! #'-))
  (setf (gethash "*" *functions*) (apply-binary! #'*))
  (setf (gethash "/" *functions*) (apply-binary! #'/))
  (setf (gethash "//" *functions*) (apply-binary! (compose #'floor #'/))) ;; int division
  (setf (gethash "_" *functions*) (apply-unary! #'-))
  (setf (gethash "NEG" *functions*) (apply-unary! #'-))

  ;; stack primitives
  (setf (gethash "DROP" *functions*) (drop!))
  (setf (gethash "POP" *functions*) (drop!))
  (setf (gethash "SWAP" *functions*) (do! (a <- pop!) (b <- pop!) (push! a) (push! b)))
  (setf (gethash "DUP" *functions*) (dup!))
  (setf (gethash "ROT" *functions*) (do! (a <- pop!) (b <- pop!) (c <- pop!)
                                         (push! a) (push! c) (push! b)))

  ;; boolean operations
  (setf (gethash "NOT" *functions*) (apply-unary! #'lognot))
  (setf (gethash "AND" *functions*) (apply-binary! #'logand))
  (setf (gethash "OR" *functions*) (apply-binary! #'logior))
  (setf (gethash "XOR" *functions*) (apply-binary! #'logxor))
  (setf (gethash "NAND" *functions*) (apply-binary! #'lognand))
  (setf (gethash "NOR" *functions*) (apply-binary! #'lognor))
  (setf (gethash "BIT" *functions*) (apply-binary! #'logbitp))
  (setf (gethash "<<" *functions*) (apply-binary! #'ash))
  (setf (gethash ">>" *functions*) (apply-binary! #'(lambda (integer count) (ash integer (- count)))))

  ;; conditonals - 0 is false, everything else is true (but 1 is preferred)
  (setf (gethash "IF" *functions*) (do! (test <- pop!) (a <- pop!) (b <- pop!) (push! (if (not (zerop test)) a b))))
  (setf (gethash "ZEROP" *functions*) (apply-unary! (compose #'bool->int #'zerop)))
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
  (setf (gethash "SQUARE" *functions*) (apply-unary! #'square))
  (setf (gethash "CUBE" *functions*) (apply-unary! (lambda (x) (* x x x))))
  (setf (gethash "ISQRT" *functions*) (apply-unary! #'isqrt))

  ;; irrational operations that try to preserve exactness
  (setf (gethash "POW" *functions*) (apply-binary! #'expt-exact))
  (setf (gethash "SQRT" *functions*) (apply-unary! #'sqrt-exact))
  (setf (gethash "LOG" *functions*) (apply-binary! #'log-exact))
  (setf (gethash "LG" *functions*) (apply-unary! #'(lambda (x) (log-exact x 2))))
  (setf (gethash "LOG10" *functions*) (apply-unary! #'(lambda (x) (log-exact x 10))))

  ;; irrational & trig - all will result in a double-float
  (setf (gethash "EXP" *functions*) (apply-unary! (compose #'exp #'->double)))
  (setf (gethash "LN" *functions*) (apply-unary! (compose #'log #'->double)))
  (setf (gethash "SIN" *functions*) (apply-unary! (compose #'sin #'->double)))
  (setf (gethash "COS" *functions*) (apply-unary! (compose #'cos #'->double)))
  (setf (gethash "TAN" *functions*) (apply-unary! (compose #'tan #'->double)))
  (setf (gethash "ASIN" *functions*) (apply-unary! (compose #'asin #'->double)))
  (setf (gethash "ACOS" *functions*) (apply-unary! (compose #'acos #'->double)))
  (setf (gethash "ATAN" *functions*) (apply-unary! (compose #'atan #'->double)))
  (setf (gethash "ATAN2" *functions*) (apply-binary! (compose #'atan #'->double)))
  (setf (gethash "CIS" *functions*) (apply-unary! (compose #'cis #'->double)))
  (setf (gethash "SINH" *functions*) (apply-unary! (compose #'sinh #'->double)))
  (setf (gethash "COSH" *functions*) (apply-unary! (compose #'cosh #'->double)))
  (setf (gethash "TANH" *functions*) (apply-unary! (compose #'tanh #'->double)))
  (setf (gethash "ASINH" *functions*) (apply-unary! (compose #'asinh #'->double)))
  (setf (gethash "ACOSH" *functions*) (apply-unary! (compose #'acosh #'->double)))
  (setf (gethash "ATANH" *functions*) (apply-unary! (compose #'atanh #'->double)))

  ;; type conversions/constructors
  (setf (gethash "FLOAT" *functions*) (apply-unary! #'(lambda (x) (float x 1.0d0))))
  (setf (gethash "RATIONAL" *functions*) (apply-unary! #'rational))
  (setf (gethash "NUMERATOR" *functions*) (apply-unary! #'numerator))
  (setf (gethash "DENOMINATOR" *functions*) (apply-unary! #'denominator))
  (setf (gethash "COMPLEX" *functions*) (apply-binary! #'complex))
  (setf (gethash "CONJUGATE" *functions*) (apply-unary! #'conjugate))
  (setf (gethash "PHASE" *functions*) (apply-unary! #'phase))
  (setf (gethash "REALPART" *functions*) (apply-unary! #'realpart))
  (setf (gethash "IMAGPART" *functions*) (apply-unary! #'imagpart))

  ;; constants
  (setf (gethash "PI" *functions*) (push! pi))
  (setf (gethash "PHI" *functions*) (push! (/ 2 (+ 1 (sqrt 5.0d0)))))
  (setf (gethash "E" *functions*) (push! (exp 1.0d0)))
  (setf (gethash "I" *functions*) (push! #C(0 1)))
  (setf (gethash "TRUE" *functions*) (push! 1))
  (setf (gethash "FALSE" *functions*) (push! 0))

  ;; special functions
  (setf (gethash "CLEAR" *functions*) (set! nil))
  (setf (gethash "EVAL" *functions*) (eval!))
  (setf (gethash "STO" *functions*) (sto!)) ;; store
  (setf (gethash "RCL" *functions*) (rcl!)) ;; recall
  )

(defun print-stack (stack)
  (format t "~{-~*~}~%" (loop for i from 0 to 30 collect i))
  (mapcar #'(lambda (x)
              (typecase x
                (fixnum (format t "~30:<~D~>~%" x))
                (complex (format t "~30:<~a + ~ai~>~%" (realpart x) (imagpart x)))
                (number (format t "~30:<~a~>~%" x))
                ;; (number (format t "~30:<~30,10E~>~%" x))
                (t (format t "~30:<~S~>~%" x))))
          (reverse stack))
  (when (null stack)
    (format t "~30:<Stack is empty~>~%"))
  (format t "~{-~*~}~%" (loop for i from 0 to 30 collect i)))

(defun read-prompt (prompt)
  (format t "~a" prompt)
  (finish-output)
  (let ((ret (read)))
    (format t "~%")
    ret))

;; TODO: add looping construct (woo turing completeness!)
;; TODO: package/module construct?
;; TODO: add read from file (and a command line interface)
;; TODO: more complex tui using ncurses/cl-charms (but try to avoid 100% cpu usage)
(defun main ()
  (let ((stack nil)
        (*read-default-float-format* 'double-float)
        ;; safe io (hopefully)
        (*read-eval* nil)
        (*print-readably* nil))
    (init-builtin-functions)
    (handler-case
      (loop
        (handler-case
          (progn
            (print-stack stack)
             (let ((input (read-prompt "> ")))
              (cond
                ((symbol= input 'quit)
                 (return))
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
                (t (let ((action (funcall (produce-action *variables*) input)))
                     (let ((new-stack (run! stack action)))
                       (push stack (car *history*))
                       (setf (cdr *history*) nil)
                       (setf stack new-stack)))))))
          (rpn-cannot-undo () (format t "Error: Nothing to undo~%"))
          (rpn-cannot-redo () (format t "Error: Nothing to redo~%"))
          (rpn-stack-empty () (format t "Error: Not enough elements on stack~%"))
          (rpn-unsupported-element (e) (format t "Unsupported element: ~S~%" (rpn-element e)))
          (rpn-cannot-eval (e) (format t "Cannot evaluate element: ~S~%" (rpn-element e)))
          (rpn-undefined-function (e) (format t "Undefined function: ~S~%" (rpn-name e)))
          (rpn-undefined-variable (e) (format t "Undefined variable ~S~%" (rpn-name e)))
          (reader-error () (format t "Reader error: invalid element~%"))
          (type-error (e) (format t "Type Error: ~a~%" e))
          (end-of-file (e) (signal e)) ;; pass the condition 
          (error (e) (format t "Error: ~a~%" e))))
      (end-of-file () nil))))
