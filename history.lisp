(in-package :cl-user)

(defpackage zpcalc/history
  (:use :cl)
  (:import-from
    #:zpcalc/conditions
    #:calc-cannot-undo
    #:calc-cannot-redo)
  (:export
    #:make-history
    #:record
    #:undo
    #:redo))
(in-package :zpcalc/history)

;; History implemented as a "zipper" made from two stacks: the undo history and the redo history
;; Assumes the user has access to the current state

;; history :: (stack * stack)

;; My mental model:
;;     (undo history) : current : (redo history)
;;          /\                         /\
;;          ||                         ||
;;           ---------------------------
;;                stored in history

;; Initialize an empty history
(defun make-history ()
  (cons nil nil))

;; Records a new state to the history, overwriting the current redo history
;; In the future, may implement tree-based history to prevent losing state
(defun record (state history)
  (push state (car history))
  (setf (cdr history) nil))

;; Adds the current state to the redo history
;; Returns the most recently recorded "undo" state
(defun undo (state history)
  (when (null (car history))
    (error 'calc-cannot-undo))
  (push state (cdr history))
  (pop (car history)))

;; Adds the current state to the undo-history
;; Returns the most recently recorded "redo" state
(defun redo (state history)
  (when (null (cdr history))
    (error 'calc-cannot-redo))
  (push state (car history))
  (pop (cdr history)))
