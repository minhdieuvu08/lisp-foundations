; ;;; ============================================================
; ;;; Chapter 18 - Lisp in Lisp
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;;; ------------------------------------------------------------
;;; 1. Helper Functions
;;; ------------------------------------------------------------

(defun bind-variables (variables values &optional environment)
  "Binds a list of variables to their corresponding values and prepends to the environment."
  (append (mapcar #'list variables values) environment))

;;; ------------------------------------------------------------
;;; 2. Meta-Circular Evaluator (MICRO-EVAL)
;;; ------------------------------------------------------------

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

;;; ------------------------------------------------------------
;;; 3. Meta-Circular Applicator (MICRO-APPLY)
;;; ------------------------------------------------------------

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
      (case (first procedure)
        (m-lambda
         (micro-eval (third procedure)
                     (bind-variables (second procedure)
                                     argument-values)))
        (m-closure 
         ;; Destructures: (m-closure (m-lambda (params) (body)) saved-env)
         (let ((lambda-form (second procedure))
               (closure-env (third procedure)))
           (micro-eval (third lambda-form)
                       (bind-variables (second lambda-form)
                                       argument-values
                                       closure-env))))
        (t (error "Unknown procedure type: ~a" procedure)))))

;;; ------------------------------------------------------------
;;; 4. Execution & Automated Test Cases
;;; ------------------------------------------------------------

;; Enable trace to monitor micro-eval and micro-apply call frames
(trace micro-eval micro-apply)

(format t "=== Running Test Suite ===~%~%")

;; Test 1: Basic Evaluation
(format t "--- Test 1: Basic Primitive Evaluation ---~%")
(format t "Output: ~A~%~%" (micro-eval '(m-first (m-quote (a b c)))))

;; Test 2: Symbol-Bound Lambda (via property list)
(format t "--- Test 2: Symbol-Bound Lambda Property ---~%")
(setf (get 'm-second 'm-lambda) '(m-lambda (l) (m-first (m-rest l))))
(format t "Output: ~A~%~%" (micro-eval '(m-second (m-quote (a b c)))))

;; Test 3: Inline Lambda Execution
(format t "--- Test 3: Direct Compound Lambda Execution ---~%")
(format t "Output: ~A~%~%" (micro-eval '((m-lambda (l) (m-first (m-rest l)))
                                         (m-quote (a b c)))))

;; Test 4: Recursive Execution (m-append)
(format t "--- Test 4: Recursive Function Definition ---~%")
(setf (get 'm-append 'm-lambda)
      '(m-lambda (l1 l2)
         (m-if (m-endp l1)
               l2
               (m-cons (m-first l1)
                       (m-append (m-rest l1) l2)))))
(format t "Output: ~A~%~%" (micro-eval '(m-append (m-quote (a b c)) (m-quote (x y z)))))

;; Test 5: Explicit m-apply Evaluation
(format t "--- Test 5: Meta-Apply of Explicit Lambda ---~%")
(format t "Output: ~A~%~%" (micro-eval '(m-apply 
                                         (m-quote (m-lambda (x y) (m-cons x (m-cons y nil))))
                                         (m-quote (a b)))))

;; Test 6: Nested Functional Application (Dynamic Scoping)
(format t "--- Test 6: Nested Functional Scoping ---~%")
(format t "Output: ~A~%~%" (micro-eval '(m-apply 
                                         (m-quote
                                          (m-lambda (x y)
                                            (m-apply 
                                             (m-quote (m-lambda (l m) (m-cons l (m-cons m nil))))
                                             (m-cons x (m-cons y nil)))))
                                         (m-quote (a b)))))

;; Test 7: Lexical Scoping with Closures (The failing test case!)
(format t "--- Test 7: Lexical Closure Isolation (m-function) ---~%")
(format t "Output: ~A~%~%" (micro-eval '(m-apply 
                                         (m-function
                                          (m-lambda (x y)
                                            (m-apply
                                             (m-function 
                                              (m-lambda () (m-cons x (m-cons y nil))))
                                             (m-quote ()))))
                                         (m-quote (a b)))))