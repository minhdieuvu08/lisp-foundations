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
; Notes: EVAL takes a single argument and evaluate it.
(defparameter form-to-evaluate '(+ 2 2))
(print (eval form-to-evaluate))
(print (read)) (+ 2 2)

; READ returns a data structure like a list
; which EVAL processes as code.
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

(format t "~A~%" (char= #\a #\b))
(format t "~A~%" (char= #\a #\a))
(format t "~A~%" (char= #\a #\A))

(format t "~A~%" (char-equal #\a #\b))
(format t "~A~%" (char-equal #\a #\a))
(format t "~A~%" (char-equal #\a #\A))

(format t "~A~%" (search "Katz" "Katz, Boris"))
(format t "~A~%" (search "Boris" "Katz, Boris"))
(format t "~A~%" (search "Pushkin" "Katz, Boris"))
(format t "~A~%" (search "BORIS" "Katz, Boris"))
(format t "~A~%" (search "BORIS" "Katz, Boris" :test #'char-equal))
(format t "~A~%" (search '(katz) '(katz boris)))
(format t "~A~%" (search '(boris) '(katz boris)))
(format t "~A~%" (search '(pushkin) '(katz boris)))
(format t "~A~%" ((read-line)This is a READ-LINE test))
(format t "~A~%" ((read-char)x))
(format t "~A~%" ((read-char)X))