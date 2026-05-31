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

;(format t "~a~%"(error "There has been a fatal error."))

;; Example
(format t "~a~%" (micro-eval '(m-first (m-quote (a b c)))))
(trace micro-eval micro-apply)
(format t "~a~%" (micro-eval '(m-first (m-quote (a b c)))))

;; Example with lambda-style
(setf (get 'm-second 'm-lambda)
    '(m-lambda (l) (m-first (m-rest l))))

(format t "~A~%" (micro-eval '(m-second (m-quote (a b c)))))
(format t "~A~%" (micro-eval '((M-LAMBDA (L) (M-FIRST (M-REST L)))
                                (M-QUOTE (A B C)))))
(format t "~A~%" (micro-eval '(m-second (m-quote (a b c)))))

(setf (get 'm-append 'm-lambda)
        '(m-lambda (l1 l2)
            (m-if (m-endp l1)
                l2
                (m-cons (m-first l1)
                        (m-append (m-rest l1) l2)))))

(format t "~A~%" (micro-eval '(m-append (m-quote (a b c)) (m-quote (x y z)))))

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
            (m -apply 
                (micro-apply 
                    (micro-eval (second form) environment)
                    (micro-eval (third form) environment)))
            (t (micro-apply
                    (first form)
                    (mapcar #'(lambda (x) (micro-eval x environment))
                            (rest form)))))))

(format t "~A~%" (micro-eval '(m-apply 
                                (m-quote (m-lambda (x y)
                                                    (m-cons x (m-cons y nil))))
                                (m-quote (a b)))))

(micro-eval '(m-apply 
                (m-quote
                    (m-lambda (x y)
                        (m-apply 
                            (m-quote
                                (m-lambda (l m)
                                    (m-cons l (m-cons m nil))))
                            (m-cons x (m-cons y nil)))))
                (m-quote (a b))))

(micro-eval '(m-apply 
                (m-quote 
                    (m-lambda (x y)
                        (m-apply
                            (m-quote
                                (m-lambda ()
                                    (m-cons x (m-cons y nil))))
                                (m-quote ()))))
                (m-quote (a b))))

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
                    (second argument-values)
            (t (let ((procedure-description (get procedure 'm-lambda)))
                    (if procedure-description
                        (micro-apply procedure-description argument-values)
                        (error 
                            "I could find no procedure description for ~a." 
                            procedure)))))
        (case (first procedure)
            (m-lambda
                (micro-eval (third procedure)
                            (bind-variables (second procedure)
                                            argument-values)))
            (m-closure
                (micro-eval (third procedure)
                            (bind-varibles (second procedure)
                                            argument-values
                                            (fourth procedure)))))))))

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
            (m-quote (second form)))))