; ;;; ============================================================
; ;;; Chapter 12 - Macros and Backquote
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;;;; ============================================================
;;;; Macros
;;;; ============================================================
;Example
(defun when-plusp-with-bug (number result)
    (when (plusp number) result))

(setf pressure -3)
(format t "~A~%" (when-plusp-with-bug pressure (print 'alarm)))
;; ALARM -> incorrect value
;; NIL -> correct value

(setf pressure 3)
(format t "~A~%" (when-plusp-with-bug pressure (print 'alarm)))

(defmacro when-plusp-macro (number result) ;; Macro name and parameter.
    (list 'when ;; Body.
            (list 'plusp number)
            result))

(setf pressure 2)
(format t "~A~%" (when-plusp-macro pressure (print 'alarm)))
(setf pressure -5)
(format t "~A~%" (when-plusp-macro pressure (print 'alarm)))

;;;; ============================================================
;;;; Backquote mechanism
;;;; ============================================================
(setf custom-variable 'test)
(format t "~A~%" `(THIS IS A ,CUSTOM-VARIABLE))

(setf demo-variable '(more difficult example))
(format t "~A~%" `(this is a ,demo-variable))
(format t "~A~%" `(this is a ,@demo-variable))
