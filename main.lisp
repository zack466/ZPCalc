(in-package :cl-user)

(defpackage zpcalc
  (:use :cl
    #:zpcalc/conditions
    #:zpcalc/actions
    #:zpcalc/util
    #:zpcalc/env
    #:zpcalc/history
    #:zpcalc/builtins)
  (:export
    #:Calc
    #:calc-stack
    #:calc-env
    #:calc-package
    #:calc-packages
    #:calc-history
    #:calc-reg
    #:calc-undo
    #:calc-redo
    #:calc-load
    #:calc-interact
    #:calc-enter-package
    #:calc-print
    #:main))
(in-package :zpcalc)

;; (declaim (optimize (debug 0) (safety 0) (speed 3)))

;; Currently active calculator (not to be confused with the State monad)
(defvar *state* nil)

;; an instance of an calc calculator/stack
(defclass Calc ()
  ;; Inner stack data
  ((stack
     :initarg stack
     :initform nil
     :accessor calc-stack
     :type list)
   ;; Top-level lexical environment
   (env
     :initarg env
     :initform (make-env)
     :accessor calc-env)
   ;; all loaded packages 
   (packages
     :initarg packages
     :initform (let ((h (make-hash-table)))
                 (setf (gethash :user h) (make-hash-table))
                 (setf (gethash :builtins h) *builtins*)
                 h)
     :accessor calc-packages)
   ;; The name of the currently active package
   (current-package
     :initarg current-package
     :initform :user
     :accessor calc-package)
   ;; History of stack data
   (history
     :initarg history
     :initform (make-history)
     :accessor calc-history)
   ;; unnamed register
   (reg
     :initarg reg
     :initform 0
     :accessor calc-reg)))

(defmethod calc-undo ((state Calc))
  (let ((new-stack (undo (calc-stack state) (calc-history state))))
    (setf (calc-stack state) new-stack)))

(defmethod calc-redo ((state Calc))
  (let ((new-stack (redo (calc-stack state) (calc-history state))))
    (setf (calc-stack state) new-stack)))

(defmethod calc-load ((state Calc) path)
  (with-open-file (fin path :direction :input
                       :if-does-not-exist nil)
    (if (null fin)
      (format t "Load error: file ~a does not exist~%" path)
      (loop
        (handler-case
          (calc-interact state (read fin))
          (end-of-file () (progn
                            (format t "Finished loading.~%")
                            (return))))))))

(defmethod calc-enter-package ((state Calc) input)
  (let ((name (make-keyword input)))
    (cond
      ((find #\. (string name))
       (warn 'calc-invalid-package-name))
      ((equal name :builtins)
       (warn 'calc-cannot-enter-builtins))
      (t (when (null (gethash name (calc-packages state)))
           (setf (gethash name (calc-packages state)) (make-hash-table)))
         (setf (calc-package *state*) name)))))

(defmethod calc-print ((state Calc))
  (format t "~50,,,'-A~%" (calc-package state))
  (aif (calc-stack state)
       (mapcar #'(lambda (x)
                   (typecase x
                     (integer (format t "~50:<~D~>~%" x))
                     (complex (format t "~50:<~a + ~ai~>~%" (realpart x) (imagpart x)))
                     (number (format t "~50:<~a~>~%" x))
                     ;; (number (format t "~50:<~50,10E~>~%" x))
                     (t (format t "~50:<~S~>~%" x))))
               (reverse it))
       (format t "~50:<Stack is empty~>~%"))
  (format t "~50,,,'-A~%" "-"))

(defmethod calc-interact ((state Calc) input)
  (let ((*state* state))
    (cond
      ;; Quit
      ((symbol= input 'quit)
       (signal 'calc-quit))
      ;; Undo
      ((symbol= input 'undo)
       (calc-undo state))
      ;; Redo
      ((symbol= input 'redo)
       (calc-redo state))
      ;; Load
      ((and (listp input)
            (symbol= (car input) 'load))
       (calc-load state (string (cadr input))))
      ;; Otherwise
      (t (let ((action (compile-action input (calc-env state)))
               (stack (calc-stack state)))
           (let ((new-stack (run! stack action)))
             ;; if stack is unchanged, don't record in history
             (unless (eq stack new-stack)
               (record stack (calc-history state))
               (setf (calc-stack state) new-stack))))))))

;; grabs the compiled action associated with a symbol
(defun get-action (input)
  (multiple-value-bind (pkg sym)
    (package-designator input)
    (acond
      ;; package exists
      ((and pkg (gethash pkg (calc-packages *state*)))
       (aif (gethash (make-keyword sym) it)
            it
            (error 'calc-undefined-function :name sym :package pkg)))
      ;; package is not nil, but doesn't exist
      ((not (null pkg)) (error 'calc-undefined-package :name pkg))
      ;; look into current package
      ((gethash (calc-package *state*) (calc-packages *state*))
       (aif (gethash (make-keyword sym) it)
            it
            ;; builtins
            (aif (gethash (make-keyword sym) *builtins*)
                 it
                 (error 'calc-undefined-function :name sym :package (calc-package *state*)))))
      ;; The current package should always be defined, see calc-enter-package
      ;; Every case should be covered, so this error should never happen
      (t (error 'calc-unreachable :message "Current package not defined.")))))

(defun dispatch! (input)
  (lambda (s)
    (funcall (get-action input) s)))

;; This "compiles" a symbolic input into an action
;; Special functions and constructs are implemented here
(defun compile-action (input env)
  (cond
    ;; number
    ((numberp input)
     (push! input))
    ;; variable
    ((keywordp input)
     (recall! input env))
    ;; some debugging tools
    ;; disassemble
    ((and (symbolp input)
          (symbol= input 'disassemble))
     (do! (top <- pop!)
          (let action (get-action top))
          (side-effect! (disassemble action))
           (return!)))
    ;; show top-level registers
    ((and (symbolp input)
          (symbol= input 'show-registers))
     (format t "sto <= ~s~%" (calc-reg *state*))
     (loop for k being the hash-keys in (car (calc-env *state*)) using (hash-value v)
           do (format t "~s <= ~s~%" k v))
     (return!))
    ;; show defined packages
    ((and (symbolp input)
          (symbol= input 'show-packages))
     (loop for k being the hash-keys in (calc-packages *state*)
           do (format t "~S~%" k))
     (return!))
    ;; show functions defined in current package
    ((and (symbolp input)
          (symbol= input 'show-functions))
     (loop for k being the hash-keys in (gethash (calc-package *state*) (calc-packages *state*))
           do (format t "~a~%" k))
     (return!))
    ;; show functions defined in a package
    ((and (symbolp input)
          (symbol= input 'show-functions-in))
     (do! (x <- pop!)
          (side-effect!
            (loop for k being the hash-keys in (gethash x (calc-packages *state*))
                  do (format t "~a~%" k)))
          (return!)))
    ;; A few operations that require context from the calculator
    ;; eval
    ((and (symbolp input)
          (symbol= input 'eval))
     (do! (top <- pop!)
          (compile-action top env)))
    ;; sto
    ((and (symbolp input)
          (symbol= input 'sto))
     (do! (top <- top!)
          (let x (setf (calc-reg *state*) top))
          (return! x)))
    ;; rcl
    ((and (symbolp input)
          (symbol= input 'rcl))
     (push! (calc-reg *state*)))
    ;; all other symbols
    ((symbolp input)
     (dispatch! input))
    ;; quote - technically, this lets you push arbitrary Common Lisp objects
    ;; onto the stack, even if they can't be evaluated, but I don't think
    ;; that will be a problem
    ((and (listp input)
          (eq (car input) 'quote))
     (push! (cadr input)))
    ;; in-package
    ((and (listp input)
          (symbol= 'in-package (car input)))
     (calc-enter-package *state* (cadr input))
     (return!))
    ;; with-package
    ((and (listp input)
          (symbol= (car input) 'with-package))
     (do! (let curr-package (calc-package *state*))
          (side-effect! (calc-enter-package *state* (cadr input)))
          (let body (>>> (mapcar #'(lambda (i) (compile-action i env)) (cddr input))))
          body
          (side-effect! (calc-enter-package *state* curr-package))))
    ;; store variable without pop
    ((and (listp input)
          (= 2 (length input))
          (symbol= (car input) 'store)
          (symbolp (cadr input)))
     (do! (x <- top!) (store! (cadr input) x env)))
    ;; store variable w instructions
    ((and (listp input)
          (symbol= (car input) 'store)
          (symbolp (cadr input)))
     (do! (let pre-action (>>> (mapcar #'(lambda (i) (compile-action i env)) (cddr input))))
          pre-action
          (x <- pop!)
          (store! (cadr input) x env)))
    ;; (if (<condition>) (<then>) [(<else>)])
    ((and (listp input)
          (symbol= (car input) 'if))
     (do! (let test (>>> (mapcar #'(lambda (i) (compile-action i env)) (cadr input))))
          (let then (>>> (mapcar #'(lambda (i) (compile-action i env)) (caddr input))))
          (let else (>>> (mapcar #'(lambda (i) (compile-action i env)) (cadddr input))))
          test
          (condition <- pop!)
          (if (truthy condition) then else)))
    ;; (while (<condition>) (<body>))
    ((and (listp input)
          (symbol= (car input) 'while))
     (let ((test (>>> (mapcar #'(lambda (i) (compile-action i env)) (cadr input))))
           (body (>>> (mapcar #'(lambda (i) (compile-action i env)) (cddr input)))))
       (while! test body)))

    ;; define function or constant
    ((and (listp input)
          (symbol= (car input) 'def))
     (cond
       ;; symbol function - simply gets expanded
       ((symbolp (cadr input))
        (cond
          ((find #\. (string (cadr input)))
           (warn 'calc-invalid-function-name))
          (t (setf (gethash (make-keyword (cadr input))
                            (gethash (calc-package *state*) (calc-packages *state*)))
                   (>>> (mapcar #'(lambda (i) (compile-action i env)) (cddr input))))))
        (return!))
       ;; function function - makes arguments available as lexical variables
       ((listp (cadr input))
        (destructuring-bind (fn-name &rest args)
          (cadr input)
          (cond 
            ((find #\. (string fn-name))
             (warn 'calc-invalid-function-name))
            (t (let ((new-env (make-env env)))
                 (setf (gethash (make-keyword fn-name)
                                (gethash (calc-package *state*) (calc-packages *state*)))
                       (>>>
                         (append
                           ;; pop arguments off the stack, store them into variables
                           (mapcar #'(lambda (var) (do! (x <- pop!) (store! var x new-env))) (reverse args))
                           ;; the body of the function
                           (mapcar #'(lambda (i) (compile-action i new-env)) (cddr input))))))))
          (return!)))))
    ((and (listp input))
     (>>> (mapcar #'(lambda (i) (compile-action i env)) input)))
    (t (error 'calc-cannot-evaluate :element input))))

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
  (let ((calc (make-instance 'calc))
        (*read-default-float-format* 'double-float)
        ;; safe io (hopefully)
        (*read-eval* nil)
        (*print-readably* nil))
    (handler-case
      (loop
        (handler-case
          (progn
            (calc-print calc)
            (let ((input (read-prompt "> ")))
              (calc-interact calc input)))
          (calc-cannot-undo () (format t "Error: Nothing to undo~%"))
          (calc-cannot-redo () (format t "Error: Nothing to redo~%"))
          (calc-stack-empty () (format t "Error: Not enough elements on stack~%"))
          (calc-cannot-evaluate (e) (format t "Cannot evaluate element: ~S~%" (calc-element e)))
          (calc-cannot-evaluate (e) (format t "Syntax error in element: ~S~%" (calc-element e)))
          (calc-undefined-function (e) (format t "Function ~a not defined in package ~a~%"
                                               (calc-name e) (calc-package e)))
          (calc-undefined-variable (e) (format t "Undefined variable ~S~%" (calc-name e)))
          (calc-undefined-package (e) (format t "Package ~a is not defined~%" (calc-name e)))
          (calc-invalid-package-name () (format t "Warning: package names cannot include \".\"~%"))
          (calc-invalid-function-name () (format t "Warning: function names cannot include \".\"~%"))
          (calc-cannot-enter-builtins () (format t "Warning: cannot enter package BUILTINS~%"))
          (calc-unreachable (e) (format t "Something has gone terrible wrong...~a~%" (calc-message e)))
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
      (t (e) (format t "Unexpected error: ~a~%Quitting...~%" e)))))
