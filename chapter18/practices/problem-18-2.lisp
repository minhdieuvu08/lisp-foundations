;;;; ============================================================
;;;; Chapter 18 - Problem 18-2
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
        (m-defun 
          (let ((name (second form))
                (params (third form))
                (body (fourth form)))
            (setf (get name 'm-lambda)
                    (list 'm-lambda params body))
            name))

        (m-quote (second form))
        (m-if (if (micro-eval (second form) environment)
                  (micro-eval (third form) environment)
                  (micro-eval (fourth form) environment)))
        (m-apply 
         (micro-apply 
          (micro-eval (second form) environment)
          (micro-eval (third form) environment)))
        (m-function 
         ;; Captures the current lexical environment into a closure bundle
         (list 'm-closure (second form) environment))
        (t (micro-apply
            (first form)
            (mapcar #'(lambda (x) (micro-eval x environment))
                    (rest form)))))))