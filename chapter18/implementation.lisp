; ;;; ============================================================
; ;;; Chapter 18 - Lisp in Lisp
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;;;; ============================================================
;;;; MICRO-EVAL and MICRO-APPLY
;;;; ============================================================
(defun micro-eval (form &optional environment)
    (if (atom form)
        (cond ((numberp form) form)
            ((eq t form) t)
            ((eq nil form) nil)
            (t (let ((binding (assoc form environment)))
                (if binding 
                    (second binding)
                    (error "I could find no variable binding for ~a." form)))))
        (case (first form)
            (m-quote (second form))
            (m-if (if (micro-eval (second form) environment)
                        (micro-eval (third form) environment)
                        (micro-eval (fourth form) environment)))
            (t (micro-apply
                    (first form)
                    (mapcar #'(lambda (x) (micro-eval x environment))
                            (rest form)))))))

(defun micro-apply (procedure argument-values)
    (if (symbolp procedure)
        (case procedure
            (m-first (first (first argument-values)))
            (m-rest (rest (first argument-values)))
            (m-cons (cons (first argument-values)
                            (second argument-values)))
            (m-endp (endp (first argument-values)))
            (m-not (not (first argument-values)))
            (m-eq (eq (first argument-values)
                        (second argument-values)))
            (t (let ((procedure-description (get procedure 'm-lambda)))
                (if procedure-description
                    (micro-apply procedure-description argument-values)
                    (error "I could find no procedure description for ~a." procedure)))))
        (micro-eval (third procedure)
                    (bind-variables (second procedure) argument-values))))