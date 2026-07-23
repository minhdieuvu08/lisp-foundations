;;;; ============================================================
;;;; Chapter 18 - Problem 18-3
;;;; Source: LISP - Patrick Henry Winston and Berthold Klaus Paul Horn
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
        
        ;; I add m-setq to solve this problem.
        (m-setq 
            (let ((variable (second form))
                (value (micro-eval (third form) environment)))
            value))

        (m-quote (second form))
        (m-if (if (micro-eval (second form) environment)
                  (micro-eval (third form) environment)
                  (micro-eval (fourth form) environment)))
        (m-apply 
         (micro-apply 
          (micro-eval (second form) environment)
          (micro-eval (third form) environment)))
        (m-function 
         (list 'm-closure (second form) environment))
        (t (micro-apply
            (first form)
            (mapcar #'(lambda (x) (micro-eval x environment))
                    (rest form)))))))