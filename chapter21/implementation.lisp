; ;;; ============================================================
; ;;; Chapter 21 - The Blocks Worlds with Classes and Methods
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(put-on b1 b2)

(defclass basic-block ()
  ((name :accessor block-name :initarg :name)
   (width :accessor block-width :initarg :width)
   (height :accessor block-height :initarg :height)
   (position :accessor block-position :initarg :position)
   (supported-by :accessor block-supported-by :initform :initform nil)))

(defclass movable-block (basic-block) ())

(defclass load-bearing-block (basic-block) 
    ((support-for :accessor block-support-for :initform nil)))

(defclass brick (movable-block load-bearing-block) ())
(defclass wedge (movable-block) ())
(defclass ball (movable-block) ())

(defclass hand()
    ((name :accessor hand-name :initarg :name)
     (position :accessor hand-position :initarg :position)
     (grasping :accessor hand-grasping :initform nil)))

(defvar *block* 
    (list 
        (make-instance 'table :name 'table :width 20 :height 0 :position '(0 0))
        (make-instance 'brick :name 'b1 :width 2 :height 2 :position '(0 0))
        (make-instance 'brick :name 'b2 :width 2 :height 2 :position '(2 0))
        (make-instance 'brick :name 'b3 :width 4 :height 4 :position '(4 0))
        (make-instance 'brick :name 'b4 :width 2 :height 2 :position '(8 0))
        (make-instance 'wedge :name 'w5 :width 2 :height 4 :position '(10 0))
        (make-instance 'brick :name 'w6 :width 4 :height 2 :position '(12 0))
        (make-instance 'wedge :name 'w7 :width 2 :height 2 :position '(16 0))
        (make-instance 'ball :name 'l8 :width 2 :height 2 :position '(18 0))))

(dolist (l *block*) (set (block-name l) l))

(dolist (l (remove table *block*)) 
    (push l (block-support-for table))
    (setf (block-supported-by l) table))

(defvar *hand* (make-instance 'hand :name 'h1 :position '(0 0)))

(defmethod put-on ((object movable-block) (support load-bearing-block))
    (if (get-space object support)
        (and (grasp object)
             (move object support)
             (ungrasp object))
    (format t "~&Sorry, there is no room for ~a on ~a."
             (block-name object)
             (block-name support))))

(defmethod get-space ((object movable-block) (support basic-block))
    (or (find-space object support)
        (make-space object support)))

(defmethod grasp ((object movable-block))
    (unless (eq (hand-grasping *hand*) object)
        (when (block-support-for object) (clear-top object))
        (when (hand-grasping *hand*)
            (get-rid-of (hand-grasping *hand*)))
        (format t "~&Move hand to pick up ~a at location ~a."
                (block-name object)
                (top-location object))
        (setf (hand-position *hand*) (top-location object))
        (format t "~&Grasp ~a." (block-name object))
        (setf (hand-grasping *hand*) object)) 
    t)