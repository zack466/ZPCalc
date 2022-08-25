(in-package :cl-user)

(defpackage zpcalc/actions
  (:use :cl :zpcalc/state)
  (:import-from
    #:zpcalc/env
    #:get-env)
  (:import-from
    #:zpcalc/conditions
    #:calc-stack-empty
    #:calc-undefined-variable)
  (:import-from
    #:zpcalc/util
    #:with-gensyms
    #:make-keyword)
  (:export
    #:run!
    #:push!
    #:top!
    #:pop!
    #:drop!
    #:dup!
    #:roll!
    #:unroll!
    #:store!
    #:recall!
    #:while!
    #:apply-unary!
    #:apply-binary!
    #:do!
    #:return!
    #:set!
    #:side-effect!))
(in-package :zpcalc/actions)

;; stack-related state actions that don't require any context

(defmacro push! (element)
  (with-gensyms (state)
    `(lambda (,state)
       (cons nil (cons ,element ,state)))))

(defmacro top! ()
  (with-gensyms (s)
    `(lambda (,s) (if (null ,s) (error 'calc-stack-empty) (cons (car ,s) ,s)))))

(defmacro pop! ()
  (with-gensyms (s)
    `(lambda (,s) (if (null ,s) (error 'calc-stack-empty) (cons (car ,s) (cdr ,s))))))

(defmacro dup! ()
  (with-gensyms (s)
    `(lambda (,s) (if (null ,s) (error 'calc-stack-empty) (cons nil (cons (car ,s) ,s))))))

(defmacro roll! ()
  (with-gensyms (s top rest)
    `(lambda (,s)
       (if (null ,s)
         ,s
         (let* ((,top (car ,s))
                (,rest (cdr ,s)))
           (cons nil (append ,rest (list ,top))))))))

(defmacro unroll! ()
  (with-gensyms (s top last)
    `(lambda (,s)
       (if (null ,s)
         ,s
         (let* ((,top (butlast ,s))
                (,last (last ,s)))
           (cons nil (append ,last ,top)))))))

(defmacro recall! (keyword env)
  (with-gensyms (s lookup)
    `(lambda (,s)
      (let ((,lookup (get-env ,keyword ,env)))
        (if ,lookup
          (funcall (push! ,lookup) ,s)
          (error 'calc-undefined-variable :name ,keyword))))))

(defmacro store! (input value env)
  (with-gensyms (s)
    `(lambda (,s)
       (setf (get-env (make-keyword ,input) ,env) ,value)
       (funcall (return!) ,s))))

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

(defun apply-unary! (op)
  (do! (a <- pop!)
       (push! (funcall op a))))

(defun apply-binary! (op)
  (do! (a <- pop!)
       (b <- pop!)
       (push! (funcall op b a))))
