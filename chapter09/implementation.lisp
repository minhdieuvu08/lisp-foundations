;;; =========================================================================
;;; Chapter 9 - Printing and Reading
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================

;;;; =========================================================================
;;;; PRINT and READ
;;;;==========================================================================
(defparameter temperature 100)
(print temperature)

(format t "~A~%" (if (< -1 (print (- temperature 98.6)) +1) 'normal 'abnormal))

(defparameter *name* 'kirsh)
(defparameter *symptoms* '(fever rash nausea))
(print (list 'patient *name* 'presented (length *symptoms*) 'symptoms *symptoms*))

;; ((read)kirsh)
(defparameter *text* (read))
(print *text*)

(let ((p nil))
    (print '(please type a name))
    (setf p (read))
    (print (append '(ok the name is) (list p))) p)

;;;; =========================================================================
;;;; FORMAT
;;;;==========================================================================
;; Replace "print" and "append" with "format"
(let ((p nil))
    (format t "~% Please type a name: ")
    (setf p (read))
    (format t "ok the name is ~A.~%" p) p)

(format t "Hello!")
; Print a character string on a fresh line, use "~%"
(format t "~%Hello!")
(format t "~%Hello!~%I'm ready to start now.")

;; Note: Use "&" when we want to be sure we start off on a fresh line,
;; but don't know if somw other format form has finished with a fresh-line directive.
(progn (format t "~%Line followed by % sign directive.~%")
        (format t "~%Line preceded by % sign directive.~%")
        (format t "~&Line preceded by & sign directive."))

(format t "~%The next patient is ~a." *name*)
(format t "~%Patient ~a presented ~a symptoms ~a." *name* (length *symptoms*) *symptoms*)

;;;; =========================================================================
;;;; EVAL
;;;;==========================================================================

(defparameter form-to-evaluate '(+ 2 2))
(print (eval form-to-evaluate))
(print (read)) (+ 2 2)
(print (eval (read))) (+ 2 2)

;;;; =========================================================================
;;;; Special Primitives Manipulate Strings and Characters
;;;;==========================================================================

(format t "~A~%" (length '(a b c)))
(format t "~A~%" (length "abc"))
(format t "~A~%" (reverse '(a b c)))
(format t "~A~%" (reverse "a b c"))
(format t "~A~%" (elt '(a b c) 0))
(format t "~A~%" (elt '(a b c) 2))
(format t "~A~%" (elt "abc" 0))
(format t "~A~%" (elt "abc" 2))
(format t "~A~%" (string= "abc" "xyz"))
(format t "~A~%" (string= "abc" "abc"))
(format t "~A~%" (string= "abc" "ABC"))