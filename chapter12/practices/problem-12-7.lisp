;;;; ============================================================
;;;; Chapter 12 - Problem 12-7
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================

;; Notes:
;; 1. Without quote: Lisp treat it as a variable/function call.
;; 2. With quote ('): Lisp treat it as data/symbol.

(defmacro punctuate-macro (lst &rest symbols)
    `(append ,lst ',symbols))

(format t "~A~%" (punctuate-macro '(this is an example) so to speak))