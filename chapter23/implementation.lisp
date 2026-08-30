;;;; ============================================================
;;;; Chapter 23 - Constraint Propagation
;;;; Book: LISP - 3rd Edition
;;;; Author: Patrick Henry Winston and Berthold Klaus Paul Horn
;;;; ============================================================
;;;;
;;;; OVERVIEW
;;;; --------
;;;; This file implements a *constraint net*: a bipartite network whose
;;;; nodes are ASSERTIONs (propositions carrying a degree of belief) and
;;;; whose edges are mediated by CONSTRAINT boxes (logical gates).  Rather
;;;; than committing each assertion to a single truth value, I track an
;;;; *interval* [lower, upper] subset of [0, 1] bounding how true the
;;;; assertion may be.  Evidence entering the net at one node is then
;;;; propagated outward, narrowing the intervals of its neighbours, their
;;;; neighbours, and so on until the net quiesces.
;;;;
;;;; This is the local-propagation paradigm Winston and Horn develop in
;;;; Chapter 23: no global solver and no search: each box knows only how
;;;; to relate its own inputs to its own output, and global consistency
;;;; emerges from purely local rules firing repeatedly.  The same idea
;;;; underlies Waltz line-labelling, interval arithmetic, and the
;;;; propagator networks of later constraint literature.
;;;;
;;;; WORKED EXAMPLE (bottom of file)
;;;; -------------------------------
;;;; Two stockbrokers and two mystics each offer an opinion.  We believe
;;;; the brokers collectively if *either* broker is right (an OR box), and
;;;; likewise for the mystics; our own final opinion demands that *both*
;;;; camps agree (an AND box):
;;;;
;;;;     broker1 --\
;;;;                >--[OR]--> broker-opinion --\
;;;;     broker2 --/                             \
;;;;                                              >--[AND]--> your-opinion
;;;;     mystic1 --\                             /
;;;;                >--[OR]--> mystic-opinion --/
;;;;     mystic2 --/
;;;;
;;;; Pinning broker1 to [0.25, 0.75] injects evidence at the left edge and
;;;; lets it flow rightward to your-opinion, and back again.
;;;;
;;;; LISP PARADIGMS EXERCISED HERE
;;;; -----------------------------
;;;;   * CLOS class hierarchies with slot :accessor / :initarg / :initform
;;;;     options (introduced in Chapter 14, applied in Chapters 21-22).
;;;;   * Generic functions specialised on class, so that PROPAGATE-VIA-BOX
;;;;     dispatches on gate type instead of branching on a type tag.  This
;;;;     is the data-driven programming style Chapter 14 contrasts with
;;;;     the DEFSTRUCT-plus-CASE approach of Chapter 13.
;;;;   * Mutual recursion between PROPAGATE-VIA-BOX and
;;;;     PROPAGATE-VIA-ASSERTION as the propagation mechanism.
;;;;   * LET* for sequential binding, PUSH for destructive list accumulation,
;;;;     DOLIST for iteration (Chapter 7), and SET to install objects as the
;;;;     symbol-values of their own names (Chapter 16).
;;;; ============================================================

;;; ------------------------------------------------------------
;;; 1. The assertion node
;;; ------------------------------------------------------------
;;; An assertion is a proposition whose truth we know only approximately.
;;; The pair (lower-bound, upper-bound) brackets its degree of belief:
;;;
;;;   [0, 1]      total ignorance -- the initial state (see :initform)
;;;   [1, 1]      known true
;;;   [0, 0]      known false
;;;   [0.25, .75] partial evidence, the interval we inject below
;;;
;;; Propagation may only ever *narrow* this interval, never widen it.
;;; That monotonicity is what guarantees the algorithm terminates; see
;;; the fixpoint argument in PROPAGATE-VIA-ASSERTION.

(defclass assertion ()
    ((name :accessor assertion-name :initarg :name)
    (lower-bound :accessor assertion-lower-bound :initform 0)
    (upper-bound :accessor assertion-upper-bound :initform 1)
    ;; Back-pointers to every constraint box this assertion participates
    ;; in, whether as an input or as an output.  The net is therefore
    ;; navigable in both directions, which is precisely what lets evidence
    ;; travel backwards from a conclusion to its premises.
    (constraints :accessor assertion-constraints :initform nil))
  (:documentation
   "A proposition whose truth is bracketed by the interval
[LOWER-BOUND, UPPER-BOUND] within [0, 1].  CONSTRAINTS holds every box
incident on this node, giving the undirected adjacency the propagation
walks over."))

;;; ------------------------------------------------------------
;;; 2. The constraint boxes
;;; ------------------------------------------------------------
;;; Boxes are the logical gates of the net.  The abstract superclass
;;; CONSTRAINT supplies the slots common to every box -- a NAME (accessor
;;; CONSTRAINT-NAME, initarg :NAME) and an OUTPUT assertion (accessor
;;; CONSTRAINT-OUTPUT) -- while the two intermediate classes add the input
;;; side according to arity.  The concrete gates below add no state at
;;; all: they exist purely as classes for PROPAGATE-VIA-BOX to dispatch on.
;;;
;;;     constraint              (name output)
;;;       +-- binary-constraint   (input)           <-- not-box
;;;       +-- ternary-constraint  (input-a input-b) <-- or-box, and-box
;;;
;;; FIXED (was: CONSTRAINT never defined).  Both accessors were already
;;; used by CONNECT2, CONNECT3 and PROPAGATE-VIA-ASSERTION, and :NAME was
;;; passed to MAKE-INSTANCE for every box, so the class is defined here.

(defclass constraint ()
    ((name :accessor constraint-name :initarg :name)
    ;; The assertion on the box's conclusion side.  "Output" names the
    ;; forward direction only -- evidence travels backward across this
    ;; same slot whenever a conclusion constrains its premises.
    (output :accessor constraint-output))
  (:documentation
   "Abstract superclass of every constraint box.  NAME labels the box in
the propagation trace and is the symbol the box installs itself under;
OUTPUT is the assertion the box computes.  Input slots are contributed by
the arity-specific subclasses below."))

(defclass binary-constraint (constraint)
    ((input :accessor constraint-input))
  (:documentation
   "A one-input, one-output box -- the arity of a logical negation.
Instances are wired with CONNECT2."))

(defclass ternary-constraint (constraint)
    ((input-a :accessor constraint-input-a)
    (input-b :accessor constraint-input-b))
  (:documentation
   "A two-input, one-output box -- the arity of conjunction and
disjunction.  Instances are wired with CONNECT3."))

(defclass not-box (binary-constraint) ()
  (:documentation
   "Logical negation: OUTPUT is true exactly where INPUT is false.  Its
propagation rule is the interval reflection
[lower, upper] --> [1 - upper, 1 - lower], which is its own inverse and
so reads the same forwards and backwards.  See the PROPAGATE-VIA-BOX
method in section 5."))

;;; FIXED (was: OR-BOX and AND-BOX inherited from BINARY-CONSTRAINT).
;;; Both are genuinely two-input gates -- they are wired with CONNECT3 and
;;; their propagation methods read CONSTRAINT-INPUT-A and
;;; CONSTRAINT-INPUT-B -- so they inherit from TERNARY-CONSTRAINT.  As
;;; written before, they inherited a single INPUT slot they never used and
;;; lacked the two input slots they do use.

(defclass or-box (ternary-constraint) ()
  (:documentation
   "Logical disjunction over two input assertions."))

(defclass and-box (ternary-constraint) ()
  (:documentation
   "Logical conjunction over two input assertions."))

;;; ------------------------------------------------------------
;;; 3. Wiring the net
;;; ------------------------------------------------------------
;;; Connection is deliberately symmetric: as well as pointing the box at
;;; its operands, we PUSH the box onto the CONSTRAINTS list of *every*
;;; assertion it touches.  Each edge is thus recorded twice, once per
;;; direction.  Without those back-pointers the net would only support
;;; forward inference; with them, learning something about a conclusion
;;; can revise our belief in its premises.

(defmethod connect2 ((c constraint) (i assertion) (o assertion))
  "Wire single-input box C to read from assertion I and write to O,
registering C in the constraint list of both."
    (setf (constraint-input c) i)
    (setf (constraint-output c) o)
    (push c (assertion-constraints i))
    (push c (assertion-constraints o)))

(defmethod connect3 ((c constraint) (a assertion) (b assertion) (o assertion))
  "Wire two-input box C to read from assertions A and B and write to O,
registering C in the constraint list of all three."
    (setf (constraint-input-a c) a)
    (setf (constraint-input-b c) b)
    (setf (constraint-output c) o)
    (push c (assertion-constraints a))
    (push c (assertion-constraints b))
    (push c (assertion-constraints o)))

;;; ------------------------------------------------------------
;;; 4. Building the broker / mystic net
;;; ------------------------------------------------------------
;;; The two DOLIST forms use SET to install each object as the
;;; symbol-value of its own name slot, so that after this form runs the
;;; symbol BROKER1 evaluates to the corresponding assertion object.  This
;;; is the idiom Winston and Horn use to keep example nets readable: the
;;; wiring below then reads like the diagram in the file header rather
;;; than like a chain of NTH calls.
;;;
;;; Note that SET assigns the *dynamic* (special) value of a symbol, which
;;; is why the names can be referenced as free variables in the CONNECT3
;;; calls even though they are nowhere lexically bound.  The compiler
;;; cannot know that and emits an undefined-variable warning for each --
;;; the price of the idiom.  Chapter 16 draws exactly this distinction
;;; between lexical and special variables.
;;;
;;; FIXED (was: the second DOLIST iterated over ASSERTION, the class name
;;; and an unbound variable, rather than ASSERTIONS, the list bound just
;;; above).  The assertion objects were therefore never published under
;;; their own names, and the CONNECT3 calls below could not find them.

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
    ;; Publish every box and every node under its own name, so that the
    ;; symbol BROKER-CONSTRAINT evaluates to the box named BROKER-CONSTRAINT
    ;; and the wiring below can be written in the vocabulary of the diagram.
    (dolist (l constraints) (set (constraint-name l) l))
    (dolist (l assertions) (set (assertion-name l) l))
    (connect3 broker-constraint broker1 broker2 broker-opinion)
    (connect3 mystic-constraint mystic1 mystic2 mystic-opinion)
    (connect3 your-constraint
              broker-opinion
              mystic-opinion
              your-opinion))

;;; ------------------------------------------------------------
;;; 5. Local propagation rules, one method per gate
;;; ------------------------------------------------------------
;;; Each method below encodes the interval arithmetic of one gate, in both
;;; directions, as three calls to PROPAGATE-VIA-ASSERTION: one forward to
;;; the output, then two backward to the inputs.  Dispatching on the class
;;; of the box is what makes this data-driven -- adding an XOR gate means
;;; adding a class and a method, and touching nothing that already exists.
;;;
;;; The bounds themselves are the Frechet inequalities relating the truth
;;; of a compound proposition to the truth of its parts.  Each method
;;; destructures its box into the six bounds it needs with a single LET*,
;;; then states the rules as arithmetic over those names -- the interval
;;; equivalent of the data-abstraction discipline of Chapter 6.

(defmethod propagate-via-box ((constraint not-box))
  "Relay evidence through a negation, forward then backward.
Both directions are the same reflection about 1/2:
    truth(not I) = 1 - truth(I)
so bounds on one side map to bounds on the other with the endpoints
exchanged -- the upper bound on I becomes the lower bound on O, because
the *more* true I may be, the *less* true its negation must be:
    1 - ui <= truth(O) <= 1 - li
    1 - uo <= truth(I) <= 1 - lo
Unlike the two-input gates this rule loses no information in either
direction: the interval on O determines the interval on I exactly."
    (let* ((i (constraint-input constraint))
           (o (constraint-output constraint))
           (li (assertion-lower-bound i))
           (ui (assertion-upper-bound i))
           (lo (assertion-lower-bound o))
           (uo (assertion-upper-bound o)))
        ;; Forward: the input constrains the output.
        (propagate-via-assertion o constraint (- 1 ui) (- 1 li))
        ;; Backward: and symmetrically, the output constrains the input.
        (propagate-via-assertion i constraint (- 1 uo) (- 1 lo))))

(defmethod propagate-via-box ((constraint or-box))
  "Relay evidence through a disjunction, forward then backward.

Forward, from lower/upper bounds on A and B:
    max(la, lb) <= truth(A or B) <= ua + ub
  -- the disjunction is at least as true as its truest disjunct, and at
  most as true as their sum.

Backward, from bounds on the output O:
    lo - ub <= truth(A) <= uo
  -- rearranging o <= a + b gives a >= o - b, while o >= max(a, b)
  gives a <= o.  Symmetrically for B.

Note that the sum UA + UB may exceed 1; clamping back into [0, 1] is
handled once, centrally, in PROPAGATE-VIA-ASSERTION rather than being
repeated in every gate."
    (let* ((a (constraint-input-a constraint))
           (b (constraint-input-b constraint))
           (o (constraint-output constraint))
           (la (assertion-lower-bound a))
           (ua (assertion-upper-bound a))
           (lb (assertion-lower-bound b))
           (ub (assertion-upper-bound b))
           (lo (assertion-lower-bound o))
           (uo (assertion-upper-bound o)))
        ;; Forward: inputs constrain the output.
        (propagate-via-assertion o constraint (max la lb) (+ ua ub))
        ;; Backward: the output constrains each input in turn.
        (propagate-via-assertion a constraint (- lo ub) uo)
        (propagate-via-assertion b constraint (- lo ua) uo)))

(defmethod propagate-via-box ((constraint and-box))
  "Relay evidence through a conjunction, forward then backward.

Forward, from lower/upper bounds on A and B:
    la + lb - 1 <= truth(A and B) <= min(ua, ub)
  -- the conjunction can be no truer than its weaker conjunct, and the
  lower bound is the Frechet inequality: two propositions each nearly
  true cannot both fail at once.

Backward, from bounds on the output O:
    0 <= truth(A) <= 1 + uo - lb
  -- rearranging o >= a + b - 1.  No informative lower bound on an input
  follows from the output alone, so 0 is passed and the existing lower
  bound survives the MAX in PROPAGATE-VIA-ASSERTION unchanged."
    (let* ((a (constraint-input-a constraint))
           (b (constraint-input-b constraint))
           (o (constraint-output constraint))
           (la (assertion-lower-bound a))
           (ua (assertion-upper-bound a))
           (lb (assertion-lower-bound b))
           (ub (assertion-upper-bound b))
           (lo (assertion-lower-bound o))
           (uo (assertion-upper-bound o)))
        ;; Forward: inputs constrain the output.
        (propagate-via-assertion o constraint (+ la lb -1) (min ua ub))
        ;; Backward: the output constrains each input in turn.
        (propagate-via-assertion a constraint 0 (+ 1 (- uo lb)))
        (propagate-via-assertion b constraint 0 (+ 1 (- uo la)))))

;;; ------------------------------------------------------------
;;; 6. The propagation engine
;;; ------------------------------------------------------------

(defmethod propagate-via-assertion ((assertion assertion)
                                    (source constraint)
                                    lower upper)
  "Intersect ASSERTION's current interval with the new bounds [LOWER, UPPER]
proposed by box SOURCE, and recur outward if anything actually changed.

This is the half of the mutual recursion that decides whether propagation
continues.  PROPAGATE-VIA-BOX proposes; this method disposes.

Three properties are worth drawing out:

  * INTERSECTION, NOT ASSIGNMENT.  The new upper bound is (min old upper)
    and the new lower bound is (max old lower), so an interval can only
    ever shrink.  A box that proposes something weaker than what we
    already know is simply ignored.

  * CENTRAL CLAMPING.  The outer (max 0 ...) and (min 1 ...) keep every
    interval inside [0, 1], which is why the gate methods above are free
    to compute unclamped quantities such as UA + UB.

  * TERMINATION.  Intervals form a partial order under inclusion, the
    update is monotonically narrowing, and recursion is gated on an actual
    change.  The propagation is therefore a chaotic iteration descending a
    bounded lattice, and must reach a fixpoint rather than cycling -- even
    though the net contains no cycles here, this is what would make the
    algorithm safe if it did."
    (let* ((old-upper (assertion-upper-bound assertion))
            (old-lower (assertion-lower-bound assertion))
            ;; Intersect: narrow towards the proposal, never away from it.
            (new-upper (max 0 (min old-upper upper)))
            (new-lower (min 1 (max old-lower lower))))
        (unless (= old-upper new-upper)
            (setf (assertion-upper-bound assertion) new-upper))
        (unless (= old-lower new-lower)
            (setf (assertion-lower-bound assertion) new-lower))
        ;; The change guard: propagation continues only where new
        ;; information was genuinely gained.  This is both the efficiency
        ;; measure and the termination condition.
        (when (or (/= old-lower new-lower) (/= old-upper new-upper))
            (format t "~%Constraint ~a has modified ~a's values:"
                    (constraint-name source)
                    (assertion-name assertion))
            (format t "~%[~4,2f, ~4,2f] --> [~4,2f, ~4,2f]"
                    old-lower old-upper
                    new-lower new-upper)
            ;; Recur outward through every *other* box incident on this
            ;; node.  SOURCE already knows what it just told us, so
            ;; re-firing it could only reproduce the same proposal.
            (dolist (constraint (assertion-constraints assertion))
                (unless (eq constraint source)
                    (propagate-via-box constraint))))))

(defmethod initiate-propagation ((assertion assertion) lower upper)
  "Inject evidence by forcing ASSERTION to [LOWER, UPPER], then push the
consequences through the net.

This is the entry point, and the one place where an interval is assigned
outright rather than intersected: the caller is asserting external
knowledge, not deriving it.  Every subsequent update travels through
PROPAGATE-VIA-ASSERTION and is therefore narrowing.

Returns NIL, being the value of the terminating DOLIST."
    (setf (assertion-upper-bound assertion) upper)
    (setf (assertion-lower-bound assertion) lower)
    (format t "~%You have started propagation from ~a with values:"
            (assertion-name assertion))
    (format t "~%[~4,2f, ~4,2f]" lower upper)
        (dolist (constraint (assertion-constraints assertion))
            (propagate-via-box constraint)))

;;; ------------------------------------------------------------
;;; 7. Running the example
;;; ------------------------------------------------------------
;;; Pin the first broker to [0.25, 0.75] -- weak, roughly balanced
;;; evidence -- and watch the net settle.  The OR box should widen our
;;; confidence in BROKER-OPINION relative to either broker alone, and the
;;; AND box should then temper YOUR-OPINION by whatever the mystics
;;; contribute (here nothing: they remain at their [0, 1] ignorance
;;; default, so the conjunction stays weakly bounded).
;;;
;;; The ~a directive prints the *return value* of INITIATE-PROPAGATION,
;;; which is NIL; the interesting output is the trace the propagation
;;; emits along the way.

(format t "~%~a" (initiate-propagation broker1 .25 .75))
(format t "~%~a" (initiate-propagation broker2 .33 .66))
(format t "~%~a" (initiate-propagation mystic1 .15 .15))
(format t "~%~a" (initiate-propagation mystic2 .85 .85))

(format t "~%~a" (initiate-propagation your-opinion .5 .75))