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

(defun  extract-key (binding) (first binding))

(defun extract-value (bindng) (second bindng))

(defun match (p d &optional bindings)
    (cond 
        ((and (atom p) (atom d)) ;; check if the pattern and the datum are both atoms.
         (if (eql p d) bindings 'fail)) ;; If they are the same then return the current value of bindings else retun fail.

        ((and (listp p) (eq '? (first p))) ;; check if the pattern is a variable
         (let ((binding (find-binding p bindings))) ;; find binding
            (if binding ;; if it is a biding
                (match (extract-value binding) d bindings) ;; then substitute
                (add-binding p d bindings))))

        ((and (listp p) (listp d)) ;; check if both pattern and datum are lists.
         (let ((result (match (first p) (first d) bindings))) ;; match first element of pattern with first element of datum using current bindings.
            (if (eq 'fail result) ;; if first element fail to match
                'fail ;; then return fail
                (match (rest p) (rest d) result)))) ;; else, match the rest of the elements using new bindings.
        (t 'fail)))

(defun match-atoms (p d bindings)
    (if (eql p d)
        bindings 
        'fail))

(format t "~a~%" (add-binding '(? x) 'apple '((y red))))
(format t "~a~%" (add-binding '(? _) 'apple '((y red))))
(format t "Example of find-binding (? x): ~a~%" (find-binding '(? x) '((x apple) (y red))))
(format t "Example of find-binding (? y): ~a~%" (find-binding '(? y) '((x apple) (y red))))
(format t "Example of find-binding (? _): ~a~%" (find-binding '(? _) '((x apple) (y red))))