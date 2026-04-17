; ;;; ============================================================
; ;;; Chapter 13 - Structures
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;;;; ============================================================
;;;; DEFSTRUCT
;;;; ============================================================
;; Notes:
;; defstruct is the primitive that creates new structure types.

;; Template:
; (defstruct <structure name> 
;     (<field name 1> <default value 1>)
;     (<field name 2> <default value 2>)
;     ...
;     (<field name n> <default value n>))

(defstruct person 
    (sex nil) ;; Default value is NIL
    (personality 'nice)
    (surname nil)) ;; Default value is NICE

(defparameter *person-instance-1* (make-person))
(defparameter *person-instance-2* (make-person :sex 'female))
(format t "~A~%" (person-sex *person-instance-2*))
(format t "~A~%" (person-personality *person-instance-2*))
(setf (person-sex *person-instance-1*) 'female)
(format t "~A~%" (person-sex *person-instance-1*))

(format t "~A~%" (person-p *person-instance-1*))
(format t "~A~%" '(this is a list -- not a person instance))
(setf (person-surname *person-instance-1*) 'winston)

(defstruct employee
    (length-of-service 0)
    (payment 'salary))

(defstruct (hacker (:include employee))
    (preferred-language 'lisp))

(defparameter *employee-example* (make-employee))
(defparameter *hacker-example* (make-hacker))
(format t "~A~%" (employee-length-of-service *employee-example*))
(format t "~A~%" (hacker-length-of-service *hacker-example*))
(format t "~A~%" (employee-length-of-service *hacker-example*))
(format t "~A~%" (employee-payment *hacker-example*))
(format t "~A~%" (hacker-preferred-language *hacker-example*))

(defstruct (salesperson (:include employee (payment 'commission)))
    (preferred-car 'mercedes))

(defparameter *salesperson-example* (make-salesperson))
(format t "~A~%"(employee-payment *hacker-example*))
(format t "~A~%" (employee-payment *salesperson-example*))

;;;; ============================================================
;;;; DESCRIBE
;;;; ============================================================
(describe *person-instance-1*)
(describe *person-instance-2*)