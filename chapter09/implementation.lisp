;;; =========================================================================
;;; Chapter 9 - Printing and Reading
;;; Book: LISP - 3rd Edition
;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;; =========================================================================
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

