; ;;; ============================================================
; ;;; Chapter 14 - Classes and Generic Functions
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

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