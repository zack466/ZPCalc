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

(defvar *all-packages* (make-hash-table))

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
   ;; The name of the currently active package
   (current-package
     :initarg current-package
     :initform :user
     :accessor rpn-package)
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

(defmethod rpn-load ((state RPN) path)
  (with-open-file (fin path :direction :input
                       :if-does-not-exist nil)
    (if (null fin)
      (format t "Load error: file ~a does not exist~%" path)
      (loop
        (handler-case
          (rpn-interact state (read fin))
          (end-of-file () (progn
                            (format t "Load Success!~%")
                            (return))))))))

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
      ;; Load
      ((and (listp input)
            (symbol= (car input) 'load))
       (rpn-load state (string (cadr input))))
      ;; Otherwise
      (t (let ((action (produce-action input (rpn-env state)))
               (stack (rpn-stack state)))
           (let ((new-stack (run! stack action)))
             ;; if stack is unchanged, don't record in history
             (unless (eq stack new-stack)
               (record stack (rpn-history state))
               (setf (rpn-stack state) new-stack))))))))

;; environment :: symbol -> action
(defvar *builtins* (make-hash-table))

;; pi, e, phi, i
;; min/max fixnum, float epislon, etc
;; plus variations like 2pi, pi2, pi3, 2pi3, etc

(defun apply! (symbol package)
  (lambda (s)
    (multiple-value-bind (pkg sym)
      (package-designator symbol)
      (acond
        ;; package exists
        ((and pkg (gethash pkg *all-packages*))
         (aif (gethash (make-keyword sym) it)
              (funcall it s)
              (error 'rpn-undefined-function :name sym :package pkg)))
        ;; package is not nil, but doesn't exist
        ((not (null pkg)) (error 'rpn-undefined-package :name pkg))
        ;; look into current package
        ((gethash package *all-packages*)
         (aif (gethash (make-keyword sym) it)
              (funcall it s)
              ;; builtins
              (aif (gethash (make-keyword sym) *builtins*)
                   (funcall it s)
                   (error 'rpn-undefined-function :name sym :package package))))
        ;; The current package should always be defined, see rpn-enter-package
        ;; Every case should be covered, so this error should never happen
        (t (error 'rpn-unreachable :message "Current package not defined."))))))

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
       (setf (get-env (make-keyword ,input) ,env) ,value)
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

(defun in-package! (input)
  (let ((name (make-keyword input)))
    (cond
      ((find #\. (string name))
       (warn 'rpn-invalid-package-name))
      ((equal name :builtins)
       (warn 'rpn-cannot-enter-builtins))
      (t (when (null (gethash name *all-packages*))
           (setf (gethash name *all-packages*) (make-hash-table)))
         (setf (rpn-package *state*) name)))
    (return!)))

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
     (apply! input (rpn-package *state*))) 
    ;; quote - technically, this lets you push arbitrary Common Lisp objects
    ;; onto the stack, even if they can't be evaluated, but I don't think
    ;; that will be a problem
    ((and (listp input)
          (eq (car input) 'quote))
     (push! (cadr input)))
    ;; Package
    ((and (listp input)
          (symbol= 'in-package (car input)))
     (in-package! (cadr input)))
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
        (cond
          ((find #\. (string (cadr input)))
           (warn 'rpn-invalid-function-name))
          (t (setf (gethash (make-keyword (cadr input)) (gethash (rpn-package *state*) *all-packages*))
                   (>>> (mapcar #'(lambda (i) (produce-action i env)) (cddr input))))))
        (return!))
       ;; function function - makes arguments available as lexical variables
       ((listp (cadr input))
        (destructuring-bind (fn-name &rest args)
          (cadr input)
          (cond 
            ((find #\. (string fn-name))
             (warn 'rpn-invalid-function-name))
            (t (let ((new-env (make-env env)))
                 (setf (gethash (make-keyword fn-name) (gethash (rpn-package *state*) *all-packages*))
                       (>>>
                         (append
                           ;; pop arguments off the stack, store them into variables
                           (mapcar #'(lambda (var) (do! (x <- pop!) (var-set! var x new-env))) (reverse args))
                           ;; the body of the function
                           (mapcar #'(lambda (i) (produce-action i new-env)) (cddr input))))))))
          (return!)))))
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
  (setf (gethash :DROP *builtins*) (drop!))
  (setf (gethash :POP *builtins*) (drop!))
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
  (setf (gethash :STO *builtins*) (sto!)) ;; store
  (setf (gethash :RCL *builtins*) (rcl!)) ;; recall
  (setf (gethash :ID *builtins*) (return!)) ;; do nothing
  (setf (gethash :ERROR *builtins*) (lambda (s) (declare (ignorable s)) (signal "oopsie")))
  (setf (gethash :PACKAGE *builtins*) (push! (rpn-package *state*)))
  (setf (gethash :PACKAGE-ENTER *builtins*) (do! (x <- pop!) (in-package! x)))
  (setf (gethash :PACKAGE-EXISTS *builtins*) (do! (x <- pop!) (push! (bool->int (gethash x *all-packages*))))))

(defmethod print-stack ((state RPN))
  (format t "~30,,,'-A~%" (rpn-package state))
  (aif (rpn-stack state)
       (mapcar #'(lambda (x)
                   (typecase x
                     (integer (format t "~30:<~D~>~%" x))
                     (complex (format t "~30:<~a + ~ai~>~%" (realpart x) (imagpart x)))
                     (number (format t "~30:<~a~>~%" x))
                     ;; (number (format t "~30:<~30,10E~>~%" x))
                     (t (format t "~30:<~S~>~%" x))))
               (reverse it))
       (format t "~30:<Stack is empty~>~%"))
  (format t "~30,,,'-A~%" "-"))

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
;; TODO: standard library of packages (phys, nt, etc)
;; TODO: add read from file (and a command line interface)
;; TODO: more complex tui using ncurses/cl-charms (or maybe croaton)
;; TODO: graphing
(defun main ()
  (let ((*state* (make-instance 'RPN))
        (*read-default-float-format* 'double-float)
        ;; safe io (hopefully)
        (*read-eval* nil)
        (*print-readably* nil))
    (init-builtin-functions)
    (setf (gethash :user *all-packages*) (make-hash-table))
    (setf (gethash :builtins *all-packages*) *builtins*)
    (handler-case
      (loop
        (handler-case
          (progn
            (print-stack *state*)
            (let ((input (read-prompt "> ")))
              (rpn-interact *state* input)))
          (rpn-cannot-undo () (format t "Error: Nothing to undo~%"))
          (rpn-cannot-redo () (format t "Error: Nothing to redo~%"))
          (rpn-stack-empty () (format t "Error: Not enough elements on stack~%"))
          (rpn-cannot-evaluate (e) (format t "Cannot evaluate element: ~S~%" (rpn-element e)))
          (rpn-cannot-evaluate (e) (format t "Syntax error in element: ~S~%" (rpn-element e)))
          (rpn-undefined-function (e) (format t "Function ~a not defined in package ~a~%" (rpn-name e) (rpn-package e)))
          (rpn-undefined-variable (e) (format t "Undefined variable ~S~%" (rpn-name e)))
          (rpn-undefined-package (e) (format t "Package ~a is not defined~%" (rpn-name e)))
          (rpn-invalid-package-name () (format t "Warning: package names cannot include \".\"~%"))
          (rpn-invalid-function-name () (format t "Warning: function names cannot include \".\"~%"))
          (rpn-cannot-enter-builtins () (format t "Warning: cannot enter package BUILTINS~%"))
          (rpn-unreachable (e) (format t "Something has gone terrible wrong...~a~%" (rpn-message e)))
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
