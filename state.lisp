(in-package :cl-user)

(defpackage zpcalc/state
  (:use :cl :zpcalc/util :zpcalc/conditions)
  (:export
    #:>>=
    #:>>
    #:run!
    #:get!
    #:set!
    #:modify!
    #:return!
    #:push!
    #:top!
    #:pop!
    #:drop!
    #:dup!
    #:roll!
    #:unroll!
    #:id!
    #:do!))
(in-package :zpcalc/state)

;; state monad: contains a stateful computation
;; State = (lambda (old-state) (cons return-value new-state))
;; bind :: State a -> (a -> State b) -> State b
(defmacro >>= (state action)
  (with-gensyms (s res return-value new-state next-state)
    `(lambda (,s)
       (let* ((,res (funcall ,state ,s)) ;; runs the previous stateful computation
              ;; runs the bound function to get the next stateful computation
              (,return-value (car ,res))
              (,new-state (cdr ,res))
              ;; threads the computation through the next one
              (,next-state (funcall ,action ,return-value)))
         (funcall ,next-state ,new-state)))))

;; (>>) :: State a -> State a -> State a
;; does the same as bind except ignores the previous return value
(defmacro >> (action1 action2)
  (with-gensyms (ret)
    `(>>= ,action1 (lambda (,ret)
                     (declare (ignorable ,ret))
                     ,action2))))

;; runs an action on a state, then unpacks the resulting
;; return value and new state
(defmacro run! (initial-state action)
  (with-gensyms (result)
    `(let ((,result (funcall ,action ,initial-state)))
       (values (cdr ,result) (car ,result)))))

;; set return value to state, keep state the same
(defmacro get! ()
  (with-gensyms (s)
    `(lambda (,s) (cons ,s ,s))))

;; no return value, set state to given value
(defmacro set! (x)
  (with-gensyms (s)
    `(lambda (,s) (declare (ignorable ,s)) (cons nil ,x))))

;; return
(defmacro return! (&optional x)
  (with-gensyms (s)
    `(lambda (,s) (cons ,x ,s))))

;; modifies the state value given a function
(defmacro modify! (f)
  (with-gensyms (s)
    `(lambda (,s) (cons nil (funcall ,f ,s)))))

;; stack-related state actions
(defmacro push! (element)
  (with-gensyms (state)
    `(lambda (,state)
       (cons nil (cons ,element ,state)))))

(defmacro top! ()
  (with-gensyms (s)
    `(lambda (,s) (if (null ,s) (error 'rpn-stack-empty) (cons (car ,s) ,s)))))

(defmacro pop! ()
  (with-gensyms (s)
    `(lambda (,s) (if (null ,s) (error 'rpn-stack-empty) (cons (car ,s) (cdr ,s))))))

(defmacro drop! ()
  (with-gensyms (s)
    `(lambda (,s) (cons nil (cdr ,s)))))

(defmacro dup! ()
  (with-gensyms (s)
    `(lambda (,s) (if (null ,s) (error 'rpn-stack-empty) (cons nil (cons (car ,s) ,s))))))

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

(defmacro do! (&rest lines)
  (do!% lines))

(defun do!% (lines)
  (if (= 1 (length lines))
    (car lines)
    (let ((line (car lines)))
      (cond
        ((and (listp line)
              (< 2 (length line))
              (symbol= '<- (cadr line)))
         `(>>= ,(cddr line) (lambda (,(car line))
                              ,(do!% (cdr lines)))))
        ((and (listp line)
              (symbol= 'let (car line)))
         `(let ((,(cadr line) ,(caddr line)))
            ,(do!% (cdr lines))))
        (t `(>> ,line ,(do!% (cdr lines))))))))
