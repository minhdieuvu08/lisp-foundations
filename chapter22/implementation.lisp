; ;;; ============================================================
; ;;; Chapter 22 - Answering Questions about Goals
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defclass node ()
    ((parent :accessor node-parent :initform nil)
    (children :accessor node-children :initform nil)
    (action :accessor node-action :initform nil)))

(defmethod get-rid-of ((object movable-block))
    (put-on object table))

(defvar *current-node* (make-instance 'node))

(defmethod get-rid-of :around ((object movable-block))
    (let *((parent *current-node*)
            (*current-node* (make-instance 'node))
            (primary-method-value (call-next-method)))
        primary-method-value))

(defmethod attach-parent ((child node) (parent node))
    (setf (node-parent child) parent)
    (setf (node-children parent)
            (append (node-children parent) (list child))))

(defmethod attach-action ((node node) action)
    (setf (node-action node) action))

(defmethod get-rid-of :around ((object movable-block))
    (let *((parent *current-node*)
            (*current-node* (make-instance 'node))
            (primary-method-value (call-next-method)))
        (when primary-method-value 
            (attach-parent *current-node* parent)
            (attach-action *current-node* (list 'get-rid-of object)))
        primary-method-value))

(defmacro define-history-method (name parameters &rest body)
    `(defmethod ,name :around ,parameters
        (let* ((parent *current-node*)
                (*current-node* (make-instance 'node))
                (primary-method-value (call-next-method)))
    (when primary-method-value
        (attach-parent *current-node* parent)
        (attach-action *current-node* (list ',name ,@(remove-specializers parameters))))
    primary-method-value)))

(defun remove-specializers (paramerer-list)
    (mapcar #'(lambda (element)
                (if (listp element)
                    (first element)
                    element))
            paramerer-list))

(define-history-method put-on ((object movable-block)
                                (support basic-block)))
(define-history-method get-rid-of ((object movable-block)))
(define-history-method make-space ((object movable-block) (support basic-block)))
(define-history-method clear-top ((support load-bearing-block)))
(define-history-method move ((object movable-block) (support basic-block)))

(defun show-simple-tree (node &optional (identation 0))
    (format t "~&~vt~a"
            indentation
            (or (node-action node) 'top-of-tree))
    (dolist (node (node-children node))
        (show-simple-tree node (+ 2 indentation))))

(format t "~%~a" (show-simple-tree *current-node*))

(defun find-action (given-form &optional (node *current-node*))
    (let ((node-form (node-action node)))
        (if (equal given-form node-form)
            node
            (dolist (child (node-children node))
                (let ((result (find-action given-form child)))
                    (when result (return result)))))))

(defmacro tell-why (name &rest parameters)
    `(tell-why-aux (list ',name ,@parameters)))

(defun tell-why-aux (given-action)
    (let ((node (find-action given-action)))
        (if (not (null node))
            (cond ((node-action (node-parent node))
                    (format t "~&I did ~a because I wanted to ~a."
                            given-action
                            (node-action (node-parent node))))
                    (t (format t "~&I did ~a because you told me to." given-action)))
            (format t "~&I did not ~a." given-action)) 
        'done))
(concatenate 'string "abc" "xyz")

(tell-why get-rid-of w7)
(tell-why make-space b1 b2)
(tell-why put-on b1 b2)

(defclass assertion ()
    ((name :accessor assertion-name :initarg :name)
    (lower-bound :accessor assertion-lower-bound :initform 0)
    (upper-bound :accessor assertion-upper-bound :initform 1)
    (constraints :accessor assertion-constraints :initform nil)))