; ;;; ============================================================
; ;;; Chapter 24 - Symbolic Pattern Matching
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(color apple red)
(color orange orange)
(color (? x) red)
(color apple (? y))
(color (? x) (? y))
(color (? x) (? x))

(format t "~a~%" (match '(color (? x) red) '(color apple red)))
(format t "~a~%" (match '(color apple (? y)) '(color apple red)))
(format t "~a~%" (match '(color (? x) (? y)) '(color apple red)))
(format t "~a~%" (match '(color apple red) '(color apple red)))
(format t "~a~%" (match '(color (? _) red) '(color apple red)))
(format t "~a~%" (match '(color (? _) (? _)) '(color apple red)))

(format t "~a~%" (match '(color apple orange) '(color apple red)))
(format t "~a~%" (match '(color (? x) (? x)) '(color apple red)))

(setf pattern-variable-expression '(? x))
(setf datum 'apple)
(setf bindings '((y red)))

(format t "~a~%" (second pattern-variable-expression))
(list (second pattern-variable-expression) datum)
(cons (list (second pattern-variable-expression) datum) bindings)

(defun add-binding (pattern-variable-expression datum bindings)
    (if (eq '- (extract-variable pattern-variable-expression))
        bindings
        (cons (make-binding
                (extract-variable pattern-variable-expression) datum) bindings)))

(defun extract-variable (pattern-variable-expression)
    (second pattern-variable-expression))

(defun make-binding (variable datum)
    (list variable datum))

(defun find-binding (pattern-variable-expression binding)
    (unless (eq '_ (extract-variable pattern-variable-expression))
        (assoc (extract-variable pattern-variable-expression) binding)))

(format t "~a~%" (add-binding '(? x) 'apple '((y red))))
(format t "~a~%" (add-binding '(? _) 'apple '((y red))))
(format t "Example of find-binding (? x): ~a~%" (find-binding '(? x) '((x apple) (y red))))
(format t "Example of find-binding (? y): ~a~%" (find-binding '(? y) '((x apple) (y red))))