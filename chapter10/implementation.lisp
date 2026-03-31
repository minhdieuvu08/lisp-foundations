;;; =========================================================================
;;; Chapter 10 - Rules for Good Programming and Tools for Debugging
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

;;; Rule 1. Comment before definitions and staccato notes inside them.
;; PUT-ON aranges for one object to be placed on another
;; PUT-ON succeeds only if all its subprocedures succeed.
(defun put-on (arg1 arg2)
    (and (action1 arg1) ;; Gasp the object
        (action 2 arg1 arg2) ;; Move it to the support
        (action3 arg1))) ;; Ungrasp it.

;;; Rule 2. Procedure and variable names should be clear and descriptive.
(defun put-on (object support)
    (and (grasp object)
        (move object support)
        (ungrasp object)))

;;; Other rules:
;;; Procedure should be short.
;;; Procedure should have only a few arguments.
;;; Procedure should be built around goals.

;; PUT-ON aranges for one object to be placed on another
;; If there is sufficient room. PUT-ON succeeds only if all its subprocedures suceed
(defun put-on (object support)
    (when (get-space object support) ; See if there is space
        (and (grasp object) ;If there is, grasp the object,
            (move object support) ; move it to the support
            (ungrasp object)))) ; and ungrasp it.

(defun put-on (object support)
    (if (get-space object support)
        (and (grasp object)
            (move object support)
            (ungrasp object))
        (format t "~&Sorry, there is no room for ~a on ~a" object support)))

(defun count-atoms (expression)
    (if (atom expression)
        1
        (+ (count-atoms (first expression))
            (count-atoms (rest expression)))))
(format t "~A~%" (count-atoms '((this is) (a test))))
(format t "~A~%" (trace count-atoms))

(defun minus-one (n)
    (current (- n 1)))

(defun minus-two (n)
    (current (- n 2)))

(defun current (n)
    (+ (minus-one n) (minus-two n)))

(format t "~A~%" (current 6))
(format t "~A~%" (trace current minus-one minus-two))
(format t "~A~%" (untrace minus-one minus-two))

(defun count-atoms (expression)
    (cond ((atom expression) 1)
        ((null expression) 0)
        (t (+ (count-atoms (first expression))
            (count-atoms (rest expression))))))
(format t "~A~%" (count-atoms '((this is) (a test))))

(defun current (n)
    (if (= n 1)
        1
        (+ (minus-one n) (minus-two n))))

(defun minus-one (n)
    (when (< (- n 1) 1) (break "N is too small in MINUS-ONE"))
    (current (- n 1)))
(defun minus-two (n)
    (when (< (- n 2) 1) (break "N is too small in MINUS-TWO"))
    (current (- n 2)))
