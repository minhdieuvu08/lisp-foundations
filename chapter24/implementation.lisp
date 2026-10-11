; ;;; ============================================================
; ;;; Chapter 24 - Symbolic Pattern Matching
; ;;; Book: LISP - 3rd Edition
; ;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
; ;;; ============================================================

; (color apple red)
; (color orange orange)
; (color (? x) red)
; (color apple (? y))
; (color (? x) (? y))
; (color (? x) (? x))

; (format t "~a~%" (match '(color (? x) red) '(color apple red)))
; (format t "~a~%" (match '(color apple (? y)) '(color apple red)))
; (format t "~a~%" (match '(color (? x) (? y)) '(color apple red)))
; (format t "~a~%" (match '(color apple red) '(color apple red)))
; (format t "~a~%" (match '(color (? _) red) '(color apple red)))
; (format t "~a~%" (match '(color (? _) (? _)) '(color apple red)))

; (format t "~a~%" (match '(color apple orange) '(color apple red)))
; (format t "~a~%" (match '(color (? x) (? x)) '(color apple red)))

(defparameter *pattern-variable-expression* '(? x))
(defparameter *datum* 'apple)
(defparameter *bindings* '((y red)))

(format t "~%--- Building a binding by hand ---~%")
(format t "Variable extracted from (? x):            ~a~%"
        (second *pattern-variable-expression*))
(format t "Binding of that variable to 'apple:       ~a~%"
        (list (second *pattern-variable-expression*) *datum*))
(format t "That binding consed onto ((y red)):       ~a~%"
        (cons (list (second *pattern-variable-expression*) *datum*) *bindings*))

(defun add-binding (pattern-variable-expression datum bindings)
    "Bind the variable of PATTERN-VARIABLE-EXPRESSION to DATUM in BINDINGS.

    Returns a new BINDINGS alist with the binding pushed onto the front.
    The anonymous variable (? _) matches anything without being recorded,
    so BINDINGS is returned unchanged for it."
    (if (eq '_ (extract-variable pattern-variable-expression))
        bindings
        (cons (make-binding
                (extract-variable pattern-variable-expression) datum) bindings)))

(defun extract-variable (pattern-variable-expression)
    "Return the variable name held by PATTERN-VARIABLE-EXPRESSION.
    Returns X for the pattern variable (? X)."
    (second pattern-variable-expression))

(defun make-binding (variable datum)
    "Return a binding pairing VARIABLE with DATUM.
    The binding is the two-element list (VARIABLE DATUM)."
    (list variable datum))

(defun find-binding (pattern-variable-expression binding)
    "Look up the variable of PATTERN-VARIABLE-EXPRESSION in the alist BINDING.

    Returns the matching binding, or NIL when the variable is unbound.
    ADD-BINDING never records the anonymous variable (? _), so NIL is
    returned for it without consulting BINDING."
    (unless (eq '_ (extract-variable pattern-variable-expression))
        (assoc (extract-variable pattern-variable-expression) binding)))

(defun  extract-key (binding)
    "Return the variable named by BINDING."
    (first binding))

(defun extract-value (binding)
    "Return the datum stored in BINDING."
    (second binding))

(format t "~%--- Taking a pattern variable and a binding apart ---~%")
(format t "extract-variable (? x):                   ~a~%"
        (extract-variable '(? x)))
(format t "make-binding x apple:                     ~a~%"
        (make-binding 'x 'apple))
(format t "extract-key (x apple):                    ~a~%"
        (extract-key (make-binding 'x 'apple)))
(format t "extract-value (x apple):                  ~a~%"
        (extract-value (make-binding 'x 'apple)))

(defun match (p d &optional bindings)
    "Match pattern P against datum D, binding pattern variables as needed.

    Matching is one-way: only P may contain pattern variables of the form
    (? VARIABLE). BINDINGS is the alist of bindings established so far,
    which constrains every occurrence of a variable repeated in P to the
    same datum.

    The cases are tried in order:

      * both atoms        -- succeed when P and D are EQL
      * P is (? VARIABLE) -- follow the existing binding if there is one,
                             otherwise bind VARIABLE to D
      * both lists        -- match the first elements, then the rest under
                             the bindings the first match produced
      * otherwise         -- an atom against a list, which cannot match

    Returns the extended BINDINGS alist on success, or the symbol FAIL when
    P and D cannot be made to agree."
    (cond
        ((and (atom p) (atom d)) ;; check if the pattern and the datum are both atoms.
         (if (eql p d) bindings 'fail)) ;; If they are the same then return the current value of bindings else retun fail.

        ((and (listp p) (eq '? (first p))) ;; check if the pattern is a variable
         (let ((binding (find-binding p bindings))) ;; find binding
            (if binding ;; if it is a biding
                (match (extract-value binding) d bindings) ;; then substitute
                (add-binding p d bindings))))

        ((and (listp p) (listp d)) ;; check if both pattern and datum are lists.
         (let ((result (match (first p) (first d) bindings))) ;; match first element of pattern with first element of datum using current bindings.
            (if (eq 'fail result) ;; if first element fail to match
                'fail ;; then return fail
                (match (rest p) (rest d) result)))) ;; else, match the rest of the elements using new bindings.
        (t 'fail)))

(defun match-atoms (p d bindings)
    "Match atom P against atom D.

    Returns BINDINGS unchanged when P and D are EQL, otherwise the symbol
    FAIL."
    (if (eql p d)
        bindings
        'fail))

(defun match-variable (p d bindings)
    "Match the pattern variable P against datum D.

    When P is already bound in BINDINGS, its value is matched against D so
    that every occurrence of the variable agrees. Otherwise P is bound to D.

    Returns the extended BINDINGS alist, or the symbol FAIL when an existing
    binding contradicts D."
    (let ((binding (find-binding p bindings)))
        (if binding
            (match (extract-value binding) d bindings)
            (add-binding p d bindings))))

(defun match-pieces (p d bindings)
    "Match the lists P and D element by element.

    The first elements are matched under BINDINGS, and the bindings that
    match produces are threaded into the match of the remaining elements.

    Returns the extended BINDINGS alist, or the symbol FAIL as soon as any
    pair of elements fails to match."
    (let ((result (match (first p) (first d) bindings)))
        (if (eq 'fail result)
            'fail
            (match (rest p) (rest d) result))))

(defun match-simple (p d &optional bindings)
    "See MATCH. Refactored to use delegated helper functions: the same case
    analysis, with each arm's work moved into MATCH-ATOMS, MATCH-VARIABLE
    and MATCH-PIECES rather than written inline."
    (cond ((and (atom p) (atom d))
           (match-atoms p d bindings))
          ((and (listp p) (eq '? (first p)))
           (match-variable p d bindings))
          ((and (listp p) (listp d))
           (match-pieces p d bindings))
        (t 'fail)))

(defun elements-p (p d)
    "Return T when both P and D are atoms, NIL otherwise."
    (and (atom p) (atom d)))

(defun variable-p (p)
    "Return T when P is a pattern variable of the form (? VARIABLE).
    Returns NIL for atoms and for lists that do not start with the symbol ?."
    (and (listp p) (eq '? (first p))))

(defun recursive-p (p d)
    "Return T when both P and D are lists, NIL otherwise.
    Two lists are compared element by element, so this is the case that
    recurses."
    (and (listp p) (listp d)))

(format t "~%--- add-binding: named variables bind, (? _) is ignored ---~%")
(format t "add-binding (? x) apple to ((y red)):     ~a~%"
        (add-binding '(? x) 'apple '((y red))))
(format t "add-binding (? _) apple to ((y red)):     ~a~%"
        (add-binding '(? _) 'apple '((y red))))

(format t "~%--- find-binding: lookup in ((x apple) (y red)) ---~%")
(format t "find-binding (? x):                       ~a~%"
        (find-binding '(? x) '((x apple) (y red))))
(format t "find-binding (? y):                       ~a~%"
        (find-binding '(? y) '((x apple) (y red))))
(format t "find-binding (? _):                       ~a~%"
        (find-binding '(? _) '((x apple) (y red))))

(defun unify-atoms (p1 p2 bindings)
    "Unify atom P1 with atom P2.

    Returns BINDINGS unchanged when P1 and P2 are EQL, otherwise the symbol
    FAIL."
    (if (eql p1 p2) bindings 'fail))

(defun unify-pieces (p1 p2 bindings)
    "Unify the lists P1 and P2 element by element.

    The first elements are unified under BINDINGS, and the bindings that
    unification produces are threaded into the remaining elements.

    Returns the extended BINDINGS alist, or the symbol FAIL as soon as any
    pair of elements fails to unify."
    (let ((result (unify (first p1) (first p2) bindings)))
        (if (eq 'fail result)
            'fail
            (unify (rest p1) (rest p2) result))))

(defun inside-or-equal-p (variable expression bindings)
    "Return T when VARIABLE is EQUAL to EXPRESSION or occurs inside it.

    EXPRESSION is searched recursively. A pattern variable met along the way
    is replaced by its value in BINDINGS, if it has one, so that bindings
    established earlier are followed instead of treated as opaque.

    Returns NIL when VARIABLE does not occur in EXPRESSION."
    (cond ((equal variable expression) t)
          ((atom expression) nil)
          ((eq '? (first expression))
            (let ((binding (find-binding expression bindings)))
                (when binding
                    (inside-or-equal-p variable
                                        (extract-value binding) bindings))))
          (t (or (inside-or-equal-p variable (first expression) bindings)
                 (inside-or-equal-p variable (rest expression) bindings)))))

(defun insidep (variable expression bindings)
    "Return T when VARIABLE occurs strictly inside EXPRESSION, NIL otherwise.

    This is the occurs check that stops unification from building a circular
    binding such as (? X) bound to (f (? X)). A VARIABLE EQUAL to EXPRESSION
    does not count as being inside it, so unifying a variable with itself
    remains legal."
    (if (equal variable expression)
        nil
        (inside-or-equal-p variable expression bindings)))

(defun unify-variable (p1 p2 bindings)
    "Unify the pattern variable P1 with the expression P2.

    When P1 is already bound in BINDINGS, its value is unified with P2.
    Otherwise P1 is bound to P2, provided INSIDEP reports no circularity.

    Returns the extended BINDINGS alist, or the symbol FAIL when an existing
    binding contradicts P2 or when binding P1 to P2 would be circular."
    (let ((binding (find-binding p1 bindings)))
        (if binding
            (unify (extract-value binding) p2 bindings)
            (if (insidep p1 p2 bindings)
                'fail
                (add-binding p1 p2 bindings)))))

(defun unify (p1 p2 &optional bindings)
    "Unify patterns P1 and P2, binding pattern variables on either side.

    Unification is two-way: unlike MATCH, both P1 and P2 may contain pattern
    variables, and a variable may be bound to another variable or to a whole
    subexpression.

    The cases are tried in order:

      * both atoms         -- succeed when P1 and P2 are EQL
      * P1 is a variable   -- follow or bind P1, subject to the occurs check
      * P2 is a variable   -- the same, with the arguments reversed
      * both lists         -- unify the first elements, then the rest under
                              the bindings the first unification produced
      * otherwise          -- an atom against a list, which cannot unify

    Returns the extended BINDINGS alist on success, or the symbol FAIL when
    P1 and P2 cannot be made to agree."
    (cond ((elements-p p1 p2) (unify-atoms p1 p2 bindings))
          ((variable-p p1) (unify-variable p1 p2 bindings))
          ((variable-p p2) (unify-variable p2 p1 bindings))
          ((recursive-p p1 p2) (unify-pieces p1 p2 bindings))
          (t 'fail)))

(format t "~%--- unify ---~%")
(format t "Variables on the left bind to constants:  ~a~%"
        (unify '(color (? x) (? y))
               '(color apple red)))
(format t "Variables on the right bind too: ~a~%"
        (unify '(color apple red)
               '(color (? l) (? m))))
(format t "Variable bound to another variable: ~a~%"
        (unify '(color (? x) (? y))
               '(color (? l) (? m))))
(format t "A pattern unified with itself: ~a~%"
        (unify '(color (? x) (? y))
               '(color (? x) (? y))))
(format t "A variable bound to a whole sublist ~a~%"
        (unify '((? x) with (hair blond))
               '((patrick is-a person) with (hair blond))))
(format t "~a~%"
        (unify '((? x) with (hair blond))
               '((patrick is-a (? y)) with (hair blond))))
(format t "~a~%"
        (unify '((? x) with (hair blond))
               '((patrick is-a (? x)) with (hair blond))))

(format t "Unify two lists containing variables: ~a~%"
        (unify '(likes (? x) ice-cream)
               '(likes john (? y))))
(format t "Chaining variables together: ~a~%"
        (unify '(equal (? x) (? y))
               '(equal john (? z))))
(format t "Unification rejected due to infinite loop (Occurs Check): ~a~%"
        (unify '(? x) '(loves john (? x))))

(format t "~%--- insidep: the occurs check ---~%")
(format t "Check (? x) inside (f (? x)): ~a~%"
        (inside-or-equal-p '(? x) '(f (? x)) nil))
(format t "Check (? x) against itself: ~a~%"
        (inside-or-equal-p '(? x) '(? x) nil))
(format t "Check insidep (? x) in (f (? x)): ~a~%"
        (insidep '(? x) '(f (? x)) nil))
(format t "Check insidep (? x) against itself: ~a~%"
        (insidep '(? x) '(? x) nil))
        
(format t "~%-- Examples for unify-variable --~%")
(format t "Bind (? x) to the symbol 'apple': ~a~%"
        (unify-variable '(? x) 'apple nil))
(format t "Bind (? x) to 'apple' when it is already bound to 'red': ~a~%"
        (unify-variable '(? x) 'apple '((x red))))