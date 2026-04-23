; ;;; ============================================================
; ;;; Chapter 14 - Classes and Generic Functions
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

;;;; ============================================================
;;;; Data Driven Approach with Structures
;;;; ============================================================
(defstruct triangle
    (base 0)
    (altitude 0))

(defstruct rectangle
    (width 0)
    (height 0))

(defstruct circle
    (radius 0))

; (defun area (figure)
;     (cond ((triangle-p figure)
;             (* 1/2 
;                 (triangle-base figure)
;                 (triangle-altitude figure)))
;             ((rectangle-p figure)
;             (* (rectangle-width figure)
;                 (rectangle-height figure)))
;             ((circle-p figure)
;             (* pi (expt (circle-radius figure) 2)))))

(defun triangle-area (figure)
    (* 1/2 
        (triangle-base figure)
        (triangle-altitude figure)))

(defun rectangle-area (figure)
    (* (rectangle-width figure)
        (rectangle-height figure)))

(defun circle-area (figure)
    (* pi (expt (circle-radius figure) 2)))

;;;; ============================================================
;;;; Using DEFMETHOD with Structures
;;;; ============================================================
; (defun area (figure)
;     (cond ((triangle-p figure) (triangle-area figure))
;             ((rectangle-p figure) (rectangle-area figure))
;             ((circle-p figure) (circle-area figure))))

(defmethod area ((figure triangle))
    (* 1/2 
        (triangle-base figure)
        (triangle-altitude figure)))

(defmethod area ((figure rectangle))
    (* (rectangle-width figure)
        (rectangle-height figure)))

(defmethod area ((figure circle))
    (* pi (expt (circle-radius figure) 2)))

(defparameter *triangle* (make-triangle :base 2 :altitude 3))
(defparameter *rectangle* (make-rectangle :width 5 :height 7))
(defparameter *circle* (make-circle :radius 11))
(format t "Triangle area: ~A~%" (area *triangle*))
(format t "Rectangle are: ~A~%" (area *rectangle*))
(format t "Circle area: ~A~%" (area *circle*))

;;;; ============================================================
;;;; Classes Resemble Structure Types
;;;; ============================================================
(defclass article ()
    ((title :accessor article-title :initarg :title)
    (author :accessor article-author :initarg :author)))

(defclass computer-article (article) ())
(defclass business-article (article) ())
(defclass political-article (article) ())

(defclass friend ()
    ((name :accessor friend-name :initarg :name)))
(defclass hacker-friend (friend) ())
(defclass entrepreneur-friend (friend) ())
(defclass philosopher-friend (friend) ())

(defparameter *articles*
    (list (make-instance 'business-article
                            :title "Memory Prices Down")
            (make-instance 'computer-article 
                            :title "Memory Speed Up")
            (make-instance 'political-article
                            :title "Memory Impugned")))

(defparameter *friends* 
    (list (make-instance 'hacker-friend :name 'Dan)
            (make-instance 'hacker-friend :name 'Gerry)
            (make-instance 'entrepreneur-friend :name 'Philip)
            (make-instance 'philosopher-friend :name 'David)))

;;;; ============================================================
;;;; Classes Enable Method Inheritance
;;;; ============================================================
(defun print-notification (article friend)
    (format t "~%Tell ~a about \"~a.\""
            (friend-name friend) 
            (article-title article)) t)

(defmethod process ((friend hacker-friend)
                    (article computer-article))
    (print-notification article friend))

(defmethod process ((friend entrepreneur-friend)
                    (article business-article))
    (print-notification article friend))

(defmethod process ((friend philosopher-friend)
                    (article article))
    (print-notification article friend))

(defmethod process ((friend friend) (article article)))
(defmethod process ((friend t) (article t)))
(defmethod process (friend article))

(dolist (friend *friends*) 
    (dolist (article *articles*) 
        (print-notification article friend)))

(dolist (friend *friends*) 
    (dolist (article *articles*) 
        (process friend article)))

;;;; ============================================================
;;;; The Most Specific Method Takes Precedence
;;;; (Specializing Subclasses)
;;;; ============================================================
(defclass stocks-article (business-article) ())
(defclass :new-stocks-article (stocks-article) ())

(defmethod process ((friend entrepreneur-friend)
                    (article stocks-article)))
(defmethod process ((friend entrepreneur-friend)
                    (article :new-stocks-article))
    (print-notification article friend))

(process (make-instance 'entrepreneur-friend :name 'jack)
        (make-instance 'stocks-article :title "Stock Prices Up"))

(process (make-instance 'entrepreneur-friend :name 'jill)
            (make-instance ':new-stocks-article :title "New-Stock Prices Up"))

(defclass retired-hacker-friend (hacker-friend) ())

(defmethod process ((friend retired-hacker-friend)
                    (article business-article))
    (print-notification article friend))

(defmethod process ((friend hacker-friend)
                    (article stocks-article)))

(process (make-instance 'retired-hacker-friend :name 'test-friend)
        (make-instance 'stocks-article :title "Test Article"))

;;;; ============================================================
;;;; Simple Rules Approximate the Complicated Class Precedence Algorithm
;;;; ============================================================
(defclass computer-political-article 
    (computer-article political-article) ())

(defmethod process ((friend hacker-friend)
                    (article computer-article))
    (print-notification article friend))

(defmethod process ((friend hacker-friend)
                    (article political-article)))

(process
    (make-instance 'hacker-friend :name 'test-friend)
    (make-instance 'computer-political-article 
                    :title "Computer Politics Article Test"))

(process
    (make-instance 'hacker-friend :name 'test-friend)
    (make-instance 'computer-political-article
                    :title "Computer Politics Article Test"))

;;;; ============================================================
;;;; Methods Can Be Specialized to Individual Instances
;;;; ============================================================
(defparameter philip (third *friends*))
(defmethod process ((friend (eql philip))
                    (article article)))