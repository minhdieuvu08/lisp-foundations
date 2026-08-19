; ;;; ============================================================
; ;;; Chapter 21 - The Blocks Worlds with Classes and Methods
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================
;;;
;;; This file models a robot hand that moves blocks around on a table.
;;;
;;; The main idea of the chapter is that classes can carry the rules
;;; that the earlier procedural version had to check by hand:
;;;   - only a movable block can be picked up,
;;;   - only a load-bearing block can hold other blocks on top of it.
;;; Because the methods are written for these classes, Lisp chooses the
;;; correct method automatically, and we do not need extra tests.
;;;
;;; The world keeps the support relation in two directions at once:
;;;   (block-supported-by x)  the single block that x sits on
;;;   (block-support-for x)   the list of blocks that sit on x

;;; ------------------------------------------------------------
;;; Classes of blocks
;;; ------------------------------------------------------------

;; The parent class of every block. It stores the size and the place of
;; a block, and remembers what the block is standing on.
(defclass basic-block ()
  ((name :accessor block-name :initarg :name)
   (width :accessor block-width :initarg :width)
   (height :accessor block-height :initarg :height)
   (position :accessor block-position :initarg :position)
   (supported-by :accessor block-supported-by :initform nil)))

;; A block that the hand is allowed to lift. Methods that grasp or move
;; a block are written for this class, so a block outside this class can
;; never be picked up.
(defclass movable-block (basic-block) ())

;; A block that is flat enough to hold other blocks. The support-for
;; slot is the opposite view of supported-by: it lists everything that
;; is resting on this block.
(defclass load-bearing-block (basic-block)
    ((support-for :accessor block-support-for :initform nil)))

;; A brick can be lifted and can also carry other blocks, so it belongs
;; to both classes. A wedge and a ball can be lifted, but their shape is
;; too pointed or too round for anything to stand on them.
(defclass brick (movable-block load-bearing-block) ())
(defclass wedge (movable-block) ())
(defclass ball (movable-block) ())
(defclass table (load-bearing-block) ())

;; The robot hand. The grasping slot holds the block that is in the hand
;; at the moment, and is nil when the hand is empty.
(defclass hand()
    ((name :accessor hand-name :initarg :name)
     (position :accessor hand-position :initarg :position)
     (grasping :accessor hand-grasping :initform nil)))

;;; ------------------------------------------------------------
;;; The world at the start
;;; ------------------------------------------------------------

;; All the blocks in the world, with the table first. At the beginning
;; they all stand in one row directly on the table.
(defvar *block*
    (list
        (make-instance 'table :name 'table :width 20 :height 0 :position '(0 0))
        (make-instance 'brick :name 'b1 :width 2 :height 2 :position '(0 0))
        (make-instance 'brick :name 'b2 :width 2 :height 2 :position '(2 0))
        (make-instance 'brick :name 'b3 :width 4 :height 4 :position '(4 0))
        (make-instance 'brick :name 'b4 :width 2 :height 2 :position '(8 0))
        (make-instance 'wedge :name 'w5 :width 2 :height 4 :position '(10 0))
        (make-instance 'brick :name 'b6 :width 4 :height 2 :position '(12 0))
        (make-instance 'wedge :name 'w7 :width 2 :height 2 :position '(16 0))
        (make-instance 'ball :name 'l8 :width 2 :height 2 :position '(18 0))))

;; Give each block's name a value at the top level, so that the symbol
;; b1 stands for the brick object itself. This lets the examples be
;; written in a short form, such as (put-on b1 b2).
(dolist (l *block*) (set (block-name l) l))

;; Record the starting support relation in both directions: every block
;; rests on the table, and the table holds every block.
(dolist (l (remove table *block*))
    (push l (block-support-for table))
    (setf (block-supported-by l) table))

;; The one hand that acts on the world.
(defvar *hand* (make-instance 'hand :name 'h1 :position '(0 0)))

;;; ------------------------------------------------------------
;;; Moving the blocks
;;; ------------------------------------------------------------

(defmethod put-on ((object movable-block) (support load-bearing-block))
    "Place OBJECT on top of SUPPORT.

     A free space is prepared first. If there is enough room, the hand
     takes the object, carries it across and lets it go. If no room can
     be found, even after other blocks are moved away, the method simply
     prints a message and does nothing else."
    (if (get-space object support)
        (and (grasp object)
             (move object support)
             (ungrasp object))
    (format t "~&Sorry, there is no room for ~a on ~a."
             (block-name object)
             (block-name support))))

(defmethod get-space ((object movable-block) (support basic-block))
    "Find a place on SUPPORT that is wide enough for OBJECT.

     The easy way is tried first, which is looking for a gap that is
     already free. Only when this fails are other blocks moved away to
     create room."
    (or (find-space object support)
        (make-space object support)))

(defmethod grasp ((object movable-block))
    "Take OBJECT into the hand and return t.

     Nothing happens if the hand is already holding the object. In every
     other case the top of the object is cleared first, any block that
     is still in the hand is put away, and only then does the hand move
     to the object and close on it."
    (unless (eq (hand-grasping *hand*) object)
        ;; Only a load-bearing block can have anything on top of it, so
        ;; the slot is read only for that kind of block. A wedge or a
        ;; ball has no support-for slot at all.
        (when (and (typep object 'load-bearing-block)
                   (block-support-for object))
            (clear-top object))
        (when (hand-grasping *hand*)
            (get-rid-of (hand-grasping *hand*)))
        (format t "~&Move hand to pick up ~a at location ~a."
                (block-name object)
                (top-location object))
        (setf (hand-position *hand*) (top-location object))
        (format t "~&Grasp ~a." (block-name object))
        (setf (hand-grasping *hand*) object))
    t)

(defmethod ungrasp ((object movable-block))
    "Open the hand and release OBJECT.

     The hand only opens when something is already supporting the
     object, so a block is never dropped in the air."
    (when (block-supported-by object)
        (format t "~&Ungrasp ~a." (block-name object))
        (setf (hand-grasping *hand*) nil)
        t))

(defmethod get-rid-of ((object movable-block))
    "Move OBJECT out of the way by putting it back on the table."
    (put-on object table))

(defmethod make-space ((object movable-block) (support basic-block))
    "Make room on SUPPORT for OBJECT and return the space that is found.

     The blocks standing on the support are removed one by one, and the
     space is checked again after each one. In this way the stack is
     disturbed as little as possible."
    (dolist (obstruction (block-support-for support))
        (get-rid-of obstruction)
        (let ((space (find-space object support)))
            (when space (return space)))))

(defmethod clear-top ((support load-bearing-block))
    "Take every block off the top of SUPPORT and return t."
    (dolist (obstacle (block-support-for support) t)
        (get-rid-of obstacle)))

;;; ------------------------------------------------------------
;;; Positions and geometry
;;; ------------------------------------------------------------

(defmethod top-location ((object basic-block))
    "Return the point in the middle of the top face of OBJECT.

     This is the place the hand has to reach in order to take hold of
     the block."
    (list (+ (first (block-position object))
             (/ (block-width object) 2))
          (+ (second (block-position object))
             (block-height object))))

(defmethod find-space ((object support)
    "Return a free place on top of SUPPORT that is wide enough for OBJECT.

     The top of the support is examined from left to right, one unit at
     a time. A place is free when it does not overlap any block that is
     already standing there. The result is the position the object would
     take, and it is nil when no such place exists."
    (dotimes (offset (+ 1 (- (block-width support) (block-width object))))
            (unless (find-if
                      #'(lambda (obstacle)
                          (let ((obstacle-left (first (block-position obstacle))))
                            (and (< left (+ obstacle-left (block-width obstacle)))
                                 (< obstacle-left (+ left (block-width object))))))
                      (block-support-for support))
                (return (list left
                              (+ (second (block-position support))
                                 (block-height support))))))))

(defmethod move ((object movable-block) (support load-bearing-block))
    "Carry OBJECT across to SUPPORT and set it down there.

     The block is first taken off whatever was holding it, so that the
     old support no longer lists it. It is then given its new position,
     the hand follows it, and the two support slots are written again in
     both directions."
    (let ((space (get-space object support))
          (old-support (block-supported-by object)))
        (when old-support
            (setf (block-support-for old-support)
                  (remove object (block-support-for old-support))))
        (format t "~&Move ~a to top of ~a at location ~a."
                (block-name object)
                (block-name support)
                space)
        (setf (block-position object) space)
        (setf (hand-position *hand*) (top-location object))
        (push object (block-support-for support))
        (setf (block-supported-by object) support))
    t)

;; A short example of the whole system: put brick b1 on top of brick b2.
(put-on b1 b2)