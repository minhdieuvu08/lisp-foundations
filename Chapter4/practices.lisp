; ;;; ============================================================
; ;;; Chapter 4 - Predicates and Conditionals
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

(format t "Compare (+ 2 12) with 4: ~A~%" (equal (+ 2 12) 4))
(format t "Compare (+ 2 1) with 3: ~A~%" (equal (+ 2 1) 3))

; Notes: 'equal' only compares 2 arguments
(format t "Compare (+ 2 3) with (-8 4) and 4: ~A~%" (equal (+ 2 3) (- 8 4) 4)) 

;;;; ============================================================
;;;; List Membership - MEMBER
;;;; ============================================================
;; declare variable
(defvar sentence nil)
;; assign value
(setf sentence '(tell me more about your mother please))
;; print
(format t "Sentence: ~A~%" sentence)

;; The "member" function returns the sublist starting from the matching symbol to the end of the list.
;; This function is used to check if an element exists within a list.
(format t "Member: ~A~%" (member 'mother sentence))
(format t "Member: ~A~%" (member 'more sentence))

(defvar pairs nil)
(setf pairs '((father son) (mother daughter)))
(format t "Pairs: ~A~%" pairs)
(format t "Member: ~A~%" (member 'mother pairs))

(defvar pairs nil)
(setf pairs '((maple shade) (apple fruit)))
(format t "Pairs: ~A~%" pairs)

;; Lenh ':test' la tuy chon cho ham member duoc dung de so sanh hai ptu khi tim kiem
;; Check if the list (maple shade) exists in pairs
(format t "Member1: ~A~%" (member '(maple shade) pairs :test #'equal))
(format t "Member2: ~A~%" (second (first pairs)))

(defvar predicate nil)
(setf predicate #'equal)
(format t "Member: ~A~%" (member '(maple shade) '((maple shade) (apple fruit)) :test predicate))

(defvar pairs nil)
(setf pairs '((maple shade) (apple fruit)))
(format t "Member: ~A~%" (member '(maple shade) pairs :test-not #'equal))
(format t "Member: ~A~%" (member '(maple shade) '((maple shade) (maple shade)) :test-not #'equal))

;;;; ============================================================
;;;; Data-Type Predicates - LISTP, ATOM, NUMBERP, SYMBOLP
;;;; ============================================================
(format t "atom1: ~A~%" (atom 'pi))
(format t "atom2: ~A~%" (atom pi))
(format t "numberp1: ~A~%" (numberp 'pi))
(format t "numberp2: ~A~%" (numberp pi))
(format t "symbolp1: ~A~%" (symbolp 'pi))
(format t "symbolp2: ~A~%" (symbolp pi))
(format t "listp: ~A~%" (listp 'pi))
(format t "listp: ~A~%" (listp pi))
(format t "listp: ~A~%" '(this is a list with pi in it))
(format t "eq: ~A~%" (eq nil '()))
(format t "eql: ~A~%" (eql nil '()))
(format t "equal: ~A~%" (equal nil ))

;;;; ============================================================
;;;; Empty-List Predicates - NULL, ENDP
;;;; ============================================================
; The 'null' function checks if the argument is NIL
(format t "null: ~A~%" (null '(this is not empty)))
(format t "endp: ~A~%" (endp '(this is not empty)))
(format t "null: ~A~%" (null ()))
(format t "endp: ~A~%" (endp ()))
(format t "null: ~A~%" (null 'this-is-a-symbol))

; 'endp' is used to check if a LIST is empty (has reached its end).
; If the argument provided is not a list,
; the program will error.

(format t "endp: ~A~%" (endp 'this-is-a-symbol))

;;;; ============================================================
;;;; Number Predicates 
;;;; ============================================================
(defparameter zero 0) 
(defparameter one 1)
(defparameter two 2)
(defparameter three 3)
(defparameter four 4)
(defun digits (lst) lst)

(setf zero 0 one 1 two 2 three 3 four 4)
(format t "digits: ~A~%" (digits (list zero one two three four)))

(format t "numberp: ~A~%" (numberp 4))
(format t "numberp: ~A~%" (numberp four))
(format t "numberp: ~A~%" (numberp 'four))
(format t "numberp: ~A~%" (numberp digits))
(format t "numberp: ~A~%" (numberp 'digits))
(functionp #'digits)
(symbolp 'digits)

(format t "zerop: ~A~%" (zerop zero))
(format t "zerop: ~A~%" (zerop 'zero))
(format t "zerop: ~A~%" (zerop four))
(format t "plusp: ~A~%" (plusp one))
(format t "plusp: ~A~%" (plusp (- one)))
(format t "plusp: ~A~%" (plusp zero))
(format t "evenp: ~A~%" (evenp (* 9 7 5 3 1)))
(format t "evenp: ~A~%" (evenp (* 10 8 6 4 2)))

(> four two)
(> two four)
(> three two one)
(> three one two)

;;;; ============================================================
;;;; Predicates - AND, OR
;;;; ============================================================
(defparameter pets '(dog cat))
; S1: Check for 'dog -> returns (dog cat) which is not NIL -> S2
; S2: Check for 'tiger -> returns NIL
(format t "Check if tiger is in the pets list: ~A~%" (and (member 'dog pets) (member 'tiger pets)))

; Similarly:
; AND returns NIL if any of its arguments evaluate to NIL.
; OR returns NIL only if all of its arguments evaluate to NIL.
(format t "Check if dingo or tiger is in the pets list: ~A~%" (or (member 'dingo pets) (member 'tiger pets)))
(format t "Check if cat and dog are in the pets list: ~A~%" (and (member 'dog pets) (member 'cat pets)))
(format t "Check if dog or tiger is in the pets list: ~A~%" (or (member 'dog pets) (member 'tiger pets)))

(format t "~A~%" (not nil))
(format t "~A~%"(not t))
(format t "~A~%" (not 'dog))
(format t "~A~%" (member 'dog pets))
(format t "~A~%" (not (member 'dog pets)))
(format t "~A~%" (member 'dingo pets))
(format t "~A~%" (not (member 'dingo pets)))
(format t "~A~%" (and (member 'dog pets) (member 'tiger pets)))
(format t "~A~%" (and (member 'dog pets) (not (member 'tiger pets))))

;;;; ============================================================
;;;; Predicates - IF, WHEN, UNLESS
;;;; ============================================================
(defparameter *day-or-date* 'monday)
(if (symbolp *day-or-date*) 'day 'date)
(defparameter *day-or-date* 9)
(if (symbolp *day-or-date*) 'day 'date)

; (defparameter high 98 temperature 102)
(defparameter *high* 98)
(defparameter *temperature* 102)

(when (> *temperature* *high*) 
    (defparameter *high* *temperature*)
    'new-record)
(format t "~A~%" *high*)

;;;; ============================================================
;;;; Predicates - COND
;;;; ============================================================
; (setf thing 'sphere r 1) -> ERROR
(defparameter *thing* 'sphere)
(defparameter *r* 1)

(format t "~A~%" (cond ((eq *thing* 'circle) (* pi *r* *r*))
    ((eq *thing* 'sphere) (* 4 pi *r* *r*))))

(setf *thing* 'sphere *r* 1)

;; Notes: 'setf' can update multiple variables simultaneously.
;; 'defparameter' can only declare one variable at a time.

(format t "A~%" (cond ((eq *thing* 'circle) (* pi *r* *r*))
    ((eq *thing* 'sphere) (* 4 pi *r* *r*))))
(cond ((eq *thing* 'circle) (* pi *r* *r*))
    (t (* 4 pi *r* *r*)))

(setf *thing* 'sphere *r* 1)

(cond ((eq *thing* 'circle) (* pi *r* *r*))
    ((* 4 pi *r* *r*)))

(defparameter p .6)
(format t "~A~%" (cond ((> p .75) 'very-likely)
    ((> p .5) 'likely)
    ((> p .25) 'unlikely)
    (t 'very-unlikely)))

(defparameter breakfast '(eggs bacon toast tea))
(format t "~A~%" (cond ((> (length breakfast) 10) 'glutton)
    ((not (endp breakfast)) 'normal)
    (t 'anorexic)))
; NORMAL
(format t "~A~%" (cond ((> (length breakfast) 10) 'glutton)
    (breakfast 'normal)
    (t 'anorexic)))
; NORMAL

;;;; ============================================================
;;;; CASE
;;;; ============================================================
; ; Example
; (cond ((eq thing 'circle) (* pi r r))
;     ((eq thing 'sphere) (* 4 pi r r)))

; Rewrite
(defparameter *thing* 'sphere) ; shape to compute
(defparameter *r* 1) ; radius
(format t "~A~%" 
    (case *thing* 
        (circle (* pi *r* *r*)) ; if it is a circle 
        (sphere (* 4 pi *r* *r*)))) ; if it is a phere

;; Notes:
;;; Uses CASE instead of COND for cleaner branching on a single variable.
;;; CASE compares the evaluated key form (*thing*) against the unevaluated key.

; (defparameter *thing* 'point)
; (defparameter *r* 1)
; (case *thing* (circle (* pi *r* *r*)) (sphere (* 4 pi *r* *r*)))

; (setf *thing* 'point *r* 1)
; (case *thing* 
;     (circle (* pi *r* *r*))
;     (sphere (* 4 pi *r* *r*))
; )
; (setf *thing* 'point *r* 1)
; (case *thing* 
;     (circle (* pi *r* *r*))
;     (sphere (* 4 pi *r* *r*))
;     (otherwise 0))