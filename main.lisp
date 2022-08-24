(in-package :cl-user)

(defpackage rpncalc
  (:use
    :cl
    :rpncalc/util
    :rpncalc/state
    :rpncalc/conditions
    :rpncalc/env
    :rpncalc/history)
  (:export #:main))
(in-package :rpncalc)

;; need to package up all special variables into some sort of state object

;; Builtin Functions are the only "actually global" variable

;; State:
;;   fields:
;;    - topmost variable environment
;;    - user-defined functions
;;    - history
;;    - sto/rcl register
;;   methods:
;;    - run-stack (runs action on inner stack, may raise an error)
;;    - interact (takes a Lisp object as input, may raise an error)
;;    - some getters for the inner state
;;    - print-stack

(defvar *state* nil)

;; an instance of an RPN calculator/stack
(defclass RPN ()
  ;; Inner stack data
  ((stack
     :initarg stack
     :initform nil
     :accessor rpn-stack
     :type list)
   ;; Top-level lexical environment
   (env
     :initarg env
     :initform (make-env)
     :accessor rpn-env)
   ;; User-defined functions
   (functions
     :initarg functions
     :initform (make-hash-table :test #'equal)
     :accessor rpn-functions)
   ;; History of stack data
   (history
     :initarg history
     :initform (make-history)
     :accessor rpn-history)
   ;; unnamed register
   (reg
     :initarg reg
     :initform 0
     :accessor rpn-reg)))

(defmethod rpn-undo ((state RPN))
  (let ((new-stack (undo (rpn-stack state) (rpn-history state))))
    (setf (rpn-stack state) new-stack)))

(defmethod rpn-redo ((state RPN))
  (let ((new-stack (redo (rpn-stack state) (rpn-history state))))
    (setf (rpn-stack state) new-stack)))

(defmethod rpn-interact ((state RPN) input)
  (let ((*state* state))
    (cond
      ;; Quit
      ((symbol= input 'quit)
       (signal 'rpn-quit))
      ;; Undo
      ((symbol= input 'undo)
       (rpn-undo state))
      ;; Redo
      ((symbol= input 'redo)
       (rpn-redo state))
      ;; Otherwise
      (t (let ((action (produce-action input (rpn-env state)))
               (stack (rpn-stack state)))
           (let ((new-stack (run! stack action)))
             ;; if stack is unchanged, don't record in history
             (unless (eq stack new-stack)
               (record stack (rpn-history state))
               (setf (rpn-stack state) new-stack))))))))

;; environment :: symbol -> action
(defvar *functions* (make-hash-table :test #'equal))

;; pi, e, phi, i
;; min/max fixnum, float epislon, etc
;; plus variations like 2pi, pi2, pi3, 2pi3, etc

(defmacro apply! (symbol)
  (with-gensyms (s)
    `(lambda (,s)
       (acond
         ((gethash (symbol-name ,symbol) *functions*)
          (funcall it ,s))
         ((gethash (symbol-name ,symbol) (rpn-functions *state*))
          (funcall it ,s))
         (t (error 'rpn-undefined-function :name ,symbol))))))

(defmacro var-get! (keyword env)
  (with-gensyms (s lookup)
    `(lambda (,s)
      (let ((,lookup (get-env ,keyword ,env)))
        (if ,lookup
          (funcall (push! ,lookup) ,s)
          (error 'rpn-undefined-variable :name ,keyword))))))

(defmacro var-set! (input value env)
  (with-gensyms (s)
    `(lambda (,s)
       (setf (get-env (make-keyword (symbol-name ,input)) ,env) ,value)
       (funcall (return!) ,s))))

;; simply delays "produce-action"
(defun eval! (env)
  (do! (top <- pop!)
       (let action (produce-action top env))
       action))

(defun sto! ()
  (do! (top <- top!)
       (let x (setf *reg* top))
       (return! x)))

(defun rcl! ()
  (push! *reg*))

(defmacro while! (test-action body-action)
  (with-gensyms (s s% s%% s%%% result top)
    `(lambda (,s)
       (loop
         (let ((,s% (cdr (funcall ,test-action ,s))))
           (let* ((,result (funcall (pop!) ,s%))
                  (,top (car ,result))
                  (,s%% (cdr ,result)))
             (if (truthy ,top)
               (let ((,s%%% (cdr (funcall ,body-action ,s%%))))
                 (setf ,s ,s%%%))
               (return (cons nil ,s%%)))))))))

;; binds together actions using >>
;; can't use reduce bc >> is a macro...
(defun >>> (actions)
  (if (null actions)
    (return!)
    (labels ((rec (acc rest)
                  (if (null rest)
                    acc
                    (rec (>> acc (car rest)) (cdr rest)))))
      (rec (car actions) (cdr actions)))))

(defun produce-action (input env)
  (cond
    ;; number
    ((numberp input)
     (push! input))
    ;; variable
    ((keywordp input)
     (var-get! input env))
    ;; eval
    ((and (symbolp input)
          (symbol= input 'eval))
     (eval! env)) 
    ;; symbol
    ((symbolp input)
     (apply! input)) 
    ;; quote - technically, this lets you push arbitrary Common Lisp objects
    ;; onto the stack, even if they can't be evaluated, but I don't think
    ;; that will be a problem
    ((and (listp input)
          (eq (car input) 'quote))
     (push! (cadr input)))
    ;; store variable
    ;; should this pop the variable?
    ((and (listp input)
          (symbol= (car input) 'store)
          (symbolp (cadr input)))
     (do! (x <- top!) (var-set! (cadr input) x env)))
    ;; (if (<condition>) (<then>) [(<else>)])
    ((and (listp input)
          (symbol= (car input) 'if))
     (do! (let test (>>> (mapcar #'(lambda (i) (produce-action i env)) (cadr input))))
          (let then (>>> (mapcar #'(lambda (i) (produce-action i env)) (caddr input))))
          (let else (>>> (mapcar #'(lambda (i) (produce-action i env)) (cadddr input))))
          test
          (condition <- pop!)
          (if (truthy condition) then else)))
    ;; (while (<condition>) (<body>))
    ((and (listp input)
          (symbol= (car input) 'while))
     (let ((test (>>> (mapcar #'(lambda (i) (produce-action i env)) (cadr input))))
           (body (>>> (mapcar #'(lambda (i) (produce-action i env)) (caddr input)))))
       (while! test body)))

    ;; define function or constant
    ((and (listp input)
          (symbol= (car input) 'def))
     (cond
       ;; symbol function - simply gets expanded
       ((symbolp (cadr input))
        (setf (gethash (symbol-name (cadr input)) (rpn-functions *state*))
              (>>> (mapcar #'(lambda (i) (produce-action i env)) (cddr input))))
        (return!))
       ;; function function - makes arguments available as lexical variables
       ((listp (cadr input))
        (destructuring-bind (fn-name &rest args)
          (cadr input)
          (let ((new-env (make-env env)))
            (setf (gethash (symbol-name fn-name) (rpn-functions *state*))
                  (>>>
                    (append
                      ;; pop arguments off the stack, store them into variables
                      (mapcar #'(lambda (var) (do! (x <- pop!) (var-set! var x new-env))) (reverse args))
                      ;; the body of the function
                      (mapcar #'(lambda (i) (produce-action i new-env)) (cddr input)))))
            (return!))))))
    ((and (listp input))
     (>>> (mapcar #'(lambda (i) (produce-action i env)) input)))
    (t (error 'rpn-cannot-evaluate :element input))))

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
  (setf (gethash "INC" *functions*) (apply-unary! #'1+))
  (setf (gethash "DEC" *functions*) (apply-unary! #'1-))

  ;; stack primitives
  (setf (gethash "DROP" *functions*) (drop!))
  (setf (gethash "POP" *functions*) (drop!))
  (setf (gethash "SWAP" *functions*) (do! (a <- pop!) (b <- pop!) (push! a) (push! b)))
  (setf (gethash "DUP" *functions*) (dup!))
  (setf (gethash "ROT" *functions*) (do! (a <- pop!) (b <- pop!) (c <- pop!)
                                         (push! a) (push! c) (push! b)))
  (setf (gethash "ROLL" *functions*) (roll!))
  (setf (gethash "UNROLL" *functions*) (unroll!))

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
  (setf (gethash "SWITCH" *functions*) (do! (test <- pop!) (a <- pop!) (b <- pop!) (push! (if (truthy test) a b))))
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
  (setf (gethash "ATAN2" *functions*) (apply-binary! (lambda (a b) (atan (->double a) (->double b)))))
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
  (setf (gethash "STO" *functions*) (sto!)) ;; store
  (setf (gethash "RCL" *functions*) (rcl!)) ;; recall
  (setf (gethash "ID" *functions*) (return!)) ;; do nothing
  (setf (gethash "ERROR" *functions*) (lambda (s) (declare (ignorable s)) (signal "oopsie")))
  )

(defun print-stack (stack)
  (format t "~{-~*~}~%" (loop for i from 0 to 30 collect i))
  (mapcar #'(lambda (x)
              (typecase x
                (integer (format t "~30:<~D~>~%" x))
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

;; TODO: add vectors (can be used as general purpose lists?)
;; TODO: symbolic computation?
;; TODO: add basic structs/objects?
;; TODO: ranges
;; TODO: package/module construct
;; TODO: add read from file (and a command line interface)
;; TODO: more complex tui using ncurses/cl-charms (or maybe croaton)
;; TODO: graphing
;; TODO: refactor main so it can be easily tested 
(defun main ()
  (let ((*state* (make-instance 'RPN))
        (*read-default-float-format* 'double-float)
        ;; safe io (hopefully)
        (*read-eval* nil)
        (*print-readably* nil))
    (init-builtin-functions)

    (handler-case
      (loop
        (handler-case
          (progn
            (print-stack (rpn-stack *state*))
             (let ((input (read-prompt "> ")))
               (rpn-interact *state* input)))
          (rpn-cannot-undo () (format t "Error: Nothing to undo~%"))
          (rpn-cannot-redo () (format t "Error: Nothing to redo~%"))
          (rpn-stack-empty () (format t "Error: Not enough elements on stack~%"))
          (rpn-cannot-evaluate (e) (format t "Cannot evaluate element: ~S~%" (rpn-element e)))
          (rpn-cannot-evaluate (e) (format t "Syntax error in element: ~S~%" (rpn-element e)))
          (rpn-undefined-function (e) (format t "Undefined function: ~S~%" (rpn-name e)))
          (rpn-undefined-variable (e) (format t "Undefined variable ~S~%" (rpn-name e)))
          (reader-error () (format t "Reader error: invalid element~%"))
          (end-of-file (e) (signal e)) ;; pass the condition 
          ;; handle ctrl-c aka sigint (interactive-interrupt)
          ;; I know trivial-signals is an option, but I want to avoid dependencies (for now, at least)
          (#+sbcl sb-sys:interactive-interrupt
            #+ccl  ccl:interrupt-signal-condition
            #+clisp system::simple-interrupt-condition
            #+ecl ext:interactive-interrupt
            #+allegro excl:interrupt-signal
            ()
            (format t "~%Received Console Interrupt. Quit with ctrl-d or 'quit'~%"))
          (error (e) (format t "Error: ~a~%" e))
          ))
      (end-of-file () nil)
      ;; catch-all for unexcepted errors/conditions
      (t (e) (format t "Unexpected error: ~a~%Quitting...~%" e))
      )))
