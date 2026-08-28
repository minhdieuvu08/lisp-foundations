; ;;; ============================================================
; ;;; Chapter 23 - Constraint Propagation
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(defclass assertion ()
    ((name :accessor assertion-name :initarg :name)
    (lower-bound :accessor assertion-lower-bound :initform 0)
    (upper-bound :accessor assertion-upper-bound :initform 1)
    (constraints :accessor assertion-constraints :initform nil)))

(defclass binary-constraint (constraint)
    ((input :accessor constraint-input)))

(defclass ternary-constraint (constraint)
    ((input-a :accessor constraint-input-a)
    (input-b :accessor constraint-input-b)))

(defclass not-box (binary-constraint) ())

(defclass or-box (binary-constraint) ())

(defclass and-box (binary-constraint) ())

(defmethod connect2 ((c constraint) (i assertion) (o assertion))
    (setf (constraint-input c) i)
    (setf (constraint-output c) o)
    (push c (assertion-constraints i))
    (push c (assertion-constraints o)))

(defmethod connect3 ((c constraint) (a assertion) (b assertion) (o assertion))
    (setf (constraint-input-a c) a)
    (setf (constraint-input-b c) b)
    (setf (constraint-output c) o)
    (push c (assertion-constraints a))
    (push c (assertion-constraints b))
    (push c (assertion-constraints o)))

(let ((assertions 
        (list (make-instance 'assertion :name 'broker1)
            (make-instance 'assertion :name 'broker2)
            (make-instance 'assertion :name 'broker-opinion)
            (make-instance 'assertion :name 'mystic1)
            (make-instance 'assertion :name 'mystic2)
            (make-instance 'assertion :name 'mystic-opinion)
            (make-instance 'assertion :name 'your-opinion)))
     (constraints
        (list (make-instance 'or-box :name 'broker-constraint)
            (make-instance 'or-box :name 'mystic-constraint)
            (make-instance 'and-box :name 'your-constraint))))
    (dolist (l constraints) (set (constraint-name l) l))
    (dolist (l assertion) (set (assertion-name l) l))
    (connect3 broker-constraint broker1 broker2 broker-opinion)
    (connect3 mystic-constraint mystic1 mystic2 mystic-opinion)
    (connect3 your-constraint
              broker-opinion
              mystic-opinion
              your-opinion))

(defmethod propagate-via-box ((constraint or-box))
    (let* ((a (constraint-input-a constraint))
           (b (constraint-input-b constraint))
           (o (constraint-output constraint))
           (la (assertion-lower-bound a))
           (ua (assertion-upper-bound a))
           (lb (assertion-lower-bound b))
           (ub (assertion-upper-bound b))
           (lo (assertion-lower-bound o))
           (uo (assertion-upper-bound o)))
        (propagate-via-assertion o constraint (max la lb) (+ ua ub))
        (propagate-via-assertion a constraint (- lo ub) uo)
        (propagate-via-assertion b constraint (- lo ua) uo)))

(defmethod propagate-via-box ((constraint and-box))
    (let* ((a (constraint-input-a constraint))
           (b (constraint-input-b constraint))
           (o (constraint-output constraint))
           (la (assertion-lower-bound a))
           (ua (assertion-upper-bound a))
           (lb (assertion-lower-bound b))
           (ub (assertion-upper-bound b))
           (lo (assertion-lower-bound o))
           (uo (assertion-upper-bound o)))
        (propagate-via-assertion o constraint (+ la lb -1) (min ua ub))
        (propagate-via-assertion a constraint 0 (+ 1 (- uo lb)))
        (propagate-via-assertion b constraint 0 (+ 1 (- uo la)))))

(defmethod propagate-via-assertion ((assertion assertion)
                                    (source constraint)
                                    lower upper)
    (let* ((old-upper (assertion-upper-bound assertion))
            (old-lower (assertion-lower-bound assertion))
            (new-upper (max 0 (min old-upper upper)))
            (new-lower (min 1 (max old-lower lower))))
        (unless (= old-upper new-upper)
            (setf (assertion-upper-bound assertion) new-upper))
        (unless (= old-lower new-lower)
            (setf (assertion-lower-bound assertion) new-lower))
        (when (or (/= old-lower new-lower) (/= old-upper new-upper))
            (format t "~%Constraint ~a has modified ~a's values:")
            (format t "~%[~4,2f, ~4,2f] --> [~4,2f, ~4,2f]"
                    (constraint-name source)
                    (assertion-name assertion)
                    old-lower old-upper
                    new-lower new-upper)
            (dolist (constraint (assertion-constraints assertion))
                (propagate-via-box constraint)))))

(defmethod initiate-propagation ((assertion assertion) lower upper)
    (setf (assertion-upper-bound assertion) upper)
    (setf (assertion-lower-bound assertion) lower)
    (format t "~%You have started propagation from ~a with values:" 
            (assertion-name assertion))
    (format t "~%[~4,2f, ~4,2f]" lower upper)
        (dolist (constraint (assertion-constraints assertion))
            (propagate-via-box constraint)))

(format t "~%~a" (initiate-propagation broker1 .25 .75))