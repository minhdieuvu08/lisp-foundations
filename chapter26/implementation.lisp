; ;;; ============================================================
; ;;; Chapter 26 - Rule-Based Expert Systems and Forward Chaining
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;;; The following helpers are reimplemented from Chapter 24 and Chapter 25.
;;; Chapter 24 matching is reused without changing its data model.
;;; Chapter 25 later demonstrates delayed streams, but Chapter 26 follows
;;; the book's eager-stream examples such as '(nil empty-stream). Therefore
;;; these functions store the tail as an ordinary list, not a closure.
(defun stream-endp (stream) (eq stream 'empty-stream))
(defun stream-first (stream) (first stream))
(defun stream-cons (object stream) (list object stream))
(defun stream-rest (stream) (second stream))
(defun stream-append (stream1 stream2)
    (if (stream-endp stream1)
        stream2
        (stream-cons (stream-first stream1)
                     (stream-append (stream-rest stream1) stream2))))
(defun stream-transform (procedure stream)
    (if (stream-endp stream)
        'empty-stream
        (stream-cons (funcall procedure (stream-first stream))
                     (stream-transform procedure (stream-rest stream)))))
(defun stream-concatenate (streams)
    (if (stream-endp streams)
        'empty-stream
        (stream-append (stream-first streams)
                       (stream-concatenate (stream-rest streams)))))
(defun stream-member (object stream)
    (cond ((stream-endp stream) nil)
          ((equal object (stream-first stream)) t)
          (t (stream-member object (stream-rest stream)))))
(defmacro stream-remember (object variable)
    (let ((value (gensym "OBJECT")))
        `(let ((,value ,object))
            (unless (stream-member ,value ,variable)
                (setf ,variable
                      (stream-append ,variable
                                     (stream-cons ,value 'empty-stream)))
                ,value))))

;;; Reimplemented from Chapter 24: binding lookup and pattern matching.
(defun extract-value (binding) (second binding))
(defun find-binding (pattern-variable-expression bindings)
    (assoc (second pattern-variable-expression) bindings))
(defun match (pattern datum &optional bindings)
    (cond ((and (atom pattern) (atom datum))
           (if (eql pattern datum) bindings 'fail))
          ((and (consp pattern) (eq '? (first pattern)))
           (let ((binding (find-binding pattern bindings)))
               (if binding
                   (match (extract-value binding) datum bindings)
                   (if (eq '_ (second pattern))
                       bindings
                       (cons (list (second pattern) datum) bindings)))))
          ((and (consp pattern) (consp datum))
           (let ((result (match (first pattern) (first datum) bindings)))
               (if (eq 'fail result)
                   'fail
                   (match (rest pattern) (rest datum) result))))
          (t 'fail)))

(defun remember-assertion (assertion)
    (stream-remember assertion *assertions*))

(defparameter *assertions* 'empty-stream)
(remember-assertion '(bozo is a dog))
(remember-assertion '(deedee is a horse))
(remember-assertion '(deedee is a parent of sugar))
(remember-assertion '(deedee is a parent of brassy))
(format t "~A~%" *assertions*)

(defun remember-rule (rule)
    (stream-remember rule *rules*))

(defparameter *rules* 'empty-stream)
(remember-rule
    `(identify
            ((? animal) is a (? species))
            ((? animal) is a parent of (? child))
            ((? animal) is a (? species))))

(format t "~A~%" *rules*)

(defun try-assertion (pattern assertion bindings)
    (let ((result (match pattern assertion bindings)))
        (if (eq 'fail result)
            'empty-stream
            (stream-cons result 'empty-stream))))

(try-assertion '((? animal) is a (? species))
               '(bozo is a dog)
               nil)

(try-assertion '((? animal) is a parent of (? child))
               '(deedee is a parent of sugar)
               '((species dog) (animal bozo)))

(try-assertion '((? animal) is a (? species))
               '(deedee is a horse)
               nil)

(try-assertion '((? animal) is a parent of (? child))
               '(deedee is a parent of sugar)
               '((species horse) (animal deedee)))

(defun match-pattern-to-assertions (pattern bindings)
    (stream-concatenate
        (stream-transform
            #'(lambda (assertion) (try-assertion pattern assertion bindings))
            *assertions*)))

(format t "~A~%" (match-pattern-to-assertions
                    '((? animal) is a (? species)) nil))

(format t "~A~%" (match-pattern-to-assertions
                    '((? animal) is a parent of (? child))
                    '((species dog) (animal bozo))))

(format t "~A~%" (match-pattern-to-assertions 
                    '((? animal) is a parent of (? child))
                    '((species horse) (animal deedee))))

(defun filter-binding-stream (pattern stream)
    (stream-concatenate
        (stream-transform
            #'(lambda (bindings)
                (match-pattern-to-assertions pattern bindings))
            stream)))

(format t "~A~%" (filter-binding-stream '((? animal) is a (? species))
                       '(nil empty-stream)))

(format t "~A~%" (filter-binding-stream '((? animal) is a parent of (? child))
                       '(((species dog) (animal bozo))
                         (((species horse) (animal deedee))
                         empty-stream))))

(format t "~A~%" (filter-binding-stream 
    '((? animal) is a parent of (? child))
    (filter-binding-stream '((? animal) is a (? species))
                           '(nil empty-stream))))   

(defun apply-filters (pattern initial-input-stream)
    (if (endp patterns)
        initial-input-stream
        (apply-filters (rest patterns)
                       (filter-binding-stream (first patterns)
                                              initial-input-stream))))

(format t "~A~%" (apply-filters '(((? animal) is a parent of (? child))
                 ((? animal) is a (? species)))
               '(nil empty-stream)))

(defun instantiate-variables (pattern a-list)
    (cond
        ((atom pattern) pattern)
        ((eq '? (first pattern))
         (extract-value (find-binding pattern a-list)))
        (t (cons (instantiate-variables (first pattern) a-list)
                 (instantiate-variables (rest pattern) a-list)))))

(format t "~A~%" (instantiate-variables
                    '((? child) is a (? species))
                    '((child sugar) (species horse) (animal deedee))))                          

(format t "~A~%" (instantiate-variables
                    '((? child) is a (? species))
                    '((child brassy) (species horse) (animal deedee))))                          

(defun use-rule (rule)
    (let ((binding-stream
            (apply-filters (rule-ifs rule) 
                           (stream-cons nil 'empty-stream))))
        (do ((binding-stream binding-stream
                             (stream-rest binding-stream))
            (success-switch nil))
            ((stream-endp binding-stream) success-switch)
            (let ((result (instantiate-variables
                            (rule-then rule)
                            (stream-first binding-stream))))
                (when (remember-assertion result)
                    (format t "~%Rule ~a indicates ~a." (rule-name rule) result)
                    (setf success-switch t))))))

(format t "~A~%" (use-rule '(identify 
                                ((? animal) is a (? species))
                                ((? animal) is a parent of (? child))
                                ((? child) is a (? species)))))