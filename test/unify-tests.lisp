(in-package #:birdrat-propsolver/test)

(in-suite kernel-tests)

(defun unify-sexp! (left-sexp right-sexp sigma)
  (unify-formulas!
   (sexp->formula left-sexp)
   (sexp->formula right-sexp)
   sigma))

(defun lookup-sexp (sigma symbol)
  (let ((formula (substitution-lookup sigma symbol)))
    (and formula
         (formula->sexp formula))))

(defun sigma-count (sigma)
  (hash-table-count sigma))

(defun snapshot-substitution (sigma)
  (let ((snapshot (make-substitution :test (hash-table-test sigma))))
    (maphash
     (lambda (key val)
       (substitution-bind! snapshot key val))
     sigma)
    snapshot))

(defun substitution= (left right)
  (and (= (sigma-count left) (sigma-count right))
       (loop for key being the hash-keys of left
               using (hash-value left-value)
             always (let ((right-value (substitution-lookup right key)))
                      (and right-value
                           (formula= left-value right-value))))))

(defmacro fails-with-unchanged-sigma ((sigma) &body body)
  `(let ((snapshot (snapshot-substitution ,sigma)))
     (is (not (progn ,@body)))
     (is (substitution= snapshot ,sigma))))

(test unify-identical-prop-vars-without-bindings
  "Identical propositional variables should unify without creating bindings."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! 'ph 'ph sigma)))
    (is (= 0 (sigma-count sigma)))))

(test unify-identical-compound-formulas-without-bindings
  "Identical compound formulas should unify without creating bindings."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:imp ph (:not ps))
                               '(:imp ph (:not ps))
                               sigma)))
    (is (= 0 (sigma-count sigma)))))

(test unify-prop-var-with-compound-formula
  "An unbound left variable should bind to the right compound formula."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! 'ph '(:not ps) sigma)))
    (is (equal '(:not ps) (lookup-sexp sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-compound-formula-with-prop-var
  "An unbound right variable should bind to the left compound formula."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:not ps) 'ph sigma)))
    (is (equal '(:not ps) (lookup-sexp sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-implication-recursively-binds-both-sides
  "Implication unification should recurse through from and to formulas."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:imp ph ps)
                               '(:imp (:not ch) ch)
                               sigma)))
    (is (equal '(:not ch) (lookup-sexp sigma 'ph)))
    (is (equal 'ch (lookup-sexp sigma 'ps)))
    (is (= 2 (sigma-count sigma)))))

(test unify-negation-recursively-unifies-child
  "Negation unification should recurse into the negated formula."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:not ph)
                               '(:not (:imp ps ch))
                               sigma)))
    (is (equal '(:imp ps ch) (lookup-sexp sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-repeated-variable-consistency-succeeds
  "Repeated occurrences of a variable should accept the same constraint."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:imp ph ph)
                               '(:imp (:not ps) (:not ps))
                               sigma)))
    (is (equal '(:not ps) (lookup-sexp sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-variable-variable-orientation
  "Variable-variable unification should bind the left variable to the right one."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! 'ph 'ps sigma)))
    (is (equal 'ps (lookup-sexp sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-constructor-mismatch-fails
  "Different formula constructors should not unify without a variable."
  (let ((sigma (make-substitution)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! '(:not ph) '(:imp ps ch) sigma))
    (is (= 0 (sigma-count sigma)))))

(test unify-repeated-variable-conflict-fails
  "Conflicting constraints on a repeated variable should fail."
  (let ((sigma (make-substitution)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! '(:imp ph ph)
                   '(:imp (:not ps) ps)
                   sigma))
    (is (= 0 (sigma-count sigma)))
    (is (not (substitution-lookup sigma 'ph)))))

(test unify-direct-occurs-check-fails
  "A variable should not bind to a formula that directly contains it."
  (let ((sigma (make-substitution)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ph '(:imp ph ps) sigma))
    (is (= 0 (sigma-count sigma)))))

(test unify-occurs-check-through-negation-fails
  "A variable should not bind to a negation that contains it."
  (let ((sigma (make-substitution)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ph '(:not ph) sigma))
    (is (= 0 (sigma-count sigma)))))

(test unify-occurs-check-through-implication-subtree-fails
  "A variable should not bind to an implication subtree that contains it."
  (let ((sigma (make-substitution)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ph '(:imp ps (:not ph)) sigma))
    (is (= 0 (sigma-count sigma)))))

(test unify-occurs-check-through-existing-substitution-fails
  "Occurs-check should follow existing substitutions while checking a binding."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ps (sexp->formula '(:not ph)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ph '(:imp ch ps) sigma))
    (is (equal '(:not ph) (lookup-sexp sigma 'ps)))
    (is (= 1 (sigma-count sigma)))))

(test unify-failed-attempt-does-not-commit-delta
  "Tentative bindings from a failed unification should be discarded."
  (let ((sigma (make-substitution)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! '(:imp ph ph)
                   '(:imp (:not ps) ps)
                   sigma))
    (is (= 0 (sigma-count sigma)))
    (is (not (substitution-lookup sigma 'ph)))))

(test unify-existing-sigma-survives-failure
  "A failed unification should not change existing committed bindings."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'x (sexp->formula '(:not y)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! '(:imp ph ph)
                   '(:imp (:not ps) ps)
                   sigma))
    (is (equal '(:not y) (lookup-sexp sigma 'x)))
    (is (not (substitution-lookup sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-success-commits-delta
  "Successful unification should commit tentative bindings into sigma."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:imp ph ps)
                               '(:imp (:not ch) ch)
                               sigma)))
    (is (equal '(:not ch) (lookup-sexp sigma 'ph)))
    (is (equal 'ch (lookup-sexp sigma 'ps)))
    (is (= 2 (sigma-count sigma)))))

(test unify-multiple-subtree-deltas-are-committed
  "Successful recursive unification should commit bindings from multiple subtrees."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:imp (:not ph) (:imp ps ch))
                               '(:imp (:not a) (:imp b c))
                               sigma)))
    (is (equal 'a (lookup-sexp sigma 'ph)))
    (is (equal 'b (lookup-sexp sigma 'ps)))
    (is (equal 'c (lookup-sexp sigma 'ch)))
    (is (= 3 (sigma-count sigma)))))

(test unify-existing-binding-is-respected
  "Unification should respect an existing binding that already satisfies the constraint."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ph (sexp->formula '(:not ps)))
    (is (eq sigma (unify-sexp! 'ph '(:not ps) sigma)))
    (is (equal '(:not ps) (lookup-sexp sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-existing-binding-induces-new-binding
  "A walked existing binding can induce a new binding on its target variable."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ph (sexp->formula 'ps))
    (is (eq sigma (unify-sexp! 'ph '(:not ch) sigma)))
    (is (equal 'ps (lookup-sexp sigma 'ph)))
    (is (equal '(:not ch) (lookup-sexp sigma 'ps)))
    (is (= 2 (sigma-count sigma)))))

(test unify-existing-incompatible-binding-fails
  "An existing incompatible binding should make unification fail without mutation."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ph (sexp->formula '(:not ps)))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ph '(:imp a b) sigma))
    (is (equal '(:not ps) (lookup-sexp sigma 'ph)))
    (is (= 1 (sigma-count sigma)))))

(test unify-existing-direct-cycle-fails-on-left-side
  "A direct cycle reached from the left side should fail without mutation."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ph (sexp->formula 'ps))
    (substitution-bind! sigma 'ps (sexp->formula 'ph))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ph 'ch sigma))
    (is (equal 'ps (lookup-sexp sigma 'ph)))
    (is (equal 'ph (lookup-sexp sigma 'ps)))
    (is (= 2 (sigma-count sigma)))))

(test unify-existing-direct-cycle-fails-on-right-side
  "A direct cycle reached from the right side should fail without mutation."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ph (sexp->formula 'ps))
    (substitution-bind! sigma 'ps (sexp->formula 'ph))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ch 'ph sigma))
    (is (equal 'ps (lookup-sexp sigma 'ph)))
    (is (equal 'ph (lookup-sexp sigma 'ps)))
    (is (= 2 (sigma-count sigma)))))

(test unify-cycle-during-occurs-check-fails
  "A cycle found during occurs-check should make unification fail."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ps (sexp->formula 'ch))
    (substitution-bind! sigma 'ch (sexp->formula 'ps))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ph '(:imp a ps) sigma))
    (is (equal 'ch (lookup-sexp sigma 'ps)))
    (is (equal 'ps (lookup-sexp sigma 'ch)))
    (is (= 2 (sigma-count sigma)))))

(test unify-walks-substitution-chains
  "Unification should walk chains and bind the final unbound variable."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ph (sexp->formula 'ps))
    (substitution-bind! sigma 'ps (sexp->formula 'ch))
    (is (eq sigma (unify-sexp! 'ph '(:not a) sigma)))
    (is (equal 'ps (lookup-sexp sigma 'ph)))
    (is (equal 'ch (lookup-sexp sigma 'ps)))
    (is (equal '(:not a) (lookup-sexp sigma 'ch)))
    (is (= 3 (sigma-count sigma)))))

(test unify-chained-binding-plus-occurs-check-fails
  "Occurs-check should follow substitution chains before accepting a binding."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ps (sexp->formula 'ch))
    (fails-with-unchanged-sigma (sigma)
      (unify-sexp! 'ch '(:imp ps a) sigma))
    (is (equal 'ch (lookup-sexp sigma 'ps)))
    (is (= 1 (sigma-count sigma)))))

(test unify-mutates-only-on-success-for-failure-examples
  "Representative failed unifications should leave sigma exactly unchanged."
  (dolist (failure '(((:not ph) (:imp ps ch))
                     (ph (:imp ph ps))
                     (ph (:not ph))
                     (ph (:imp ps (:not ph)))))
    (let ((sigma (make-substitution)))
      (substitution-bind! sigma 'x (sexp->formula '(:not y)))
      (fails-with-unchanged-sigma (sigma)
        (unify-sexp! (first failure) (second failure) sigma)))))

(test unify-returns-same-sigma-object-on-success
  "The destructive API should return the same sigma object on success."
  (let ((sigma (make-substitution)))
    (is (eq sigma (unify-sexp! '(:imp ph ps)
                               '(:imp (:not ch) ch)
                               sigma)))))
