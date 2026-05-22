(in-package #:birdrat-propsolver/test)

(in-suite kernel-tests)

(test make-substitution-creates-empty-table
  "Make-substitution should create an empty hash table with the requested test."
  (let ((default-sigma (make-substitution))
        (equal-sigma (make-substitution :test #'equal)))
    (is (hash-table-p default-sigma))
    (is (= 0 (hash-table-count default-sigma)))
    (is (eq 'eq (hash-table-test default-sigma)))
    (is (hash-table-p equal-sigma))
    (is (= 0 (hash-table-count equal-sigma)))
    (is (eq 'equal (hash-table-test equal-sigma)))))

(test copy-substitution-copies-bindings
  "Copy-substitution should create new tables with the same bindings."
  (let* ((first-sigma (make-substitution))
         (second-sigma (make-substitution :test #'equal))
         (third-sigma (make-substitution))
         (first-formula (sexp->formula '(:not ph)))
         (second-formula (sexp->formula '(:imp ph ps)))
         (third-formula (sexp->formula '(:imp (:not ph) (:imp ps ch)))))
    (substitution-bind! first-sigma 'x first-formula)
    (substitution-bind! first-sigma 'y second-formula)
    (substitution-bind! second-sigma 'x third-formula)
    (substitution-bind! third-sigma 'a first-formula)
    (substitution-bind! third-sigma 'b third-formula)
    (dolist (sigma (list first-sigma second-sigma third-sigma))
      (let ((sigma-copy (copy-substitution sigma)))
        (is (not (eq sigma sigma-copy)))
        (is (= (hash-table-count sigma)
               (hash-table-count sigma-copy)))
        (is (eq (hash-table-test sigma)
                (hash-table-test sigma-copy)))
        (maphash
         (lambda (key val)
           (is (formula= val (substitution-lookup sigma-copy key))))
         sigma)))))

(test substitution-bind-updates-and-rejects-bad-bindings
  "Substitution-bind! should store formula bindings and reject malformed bindings."
  (let ((sigma (make-substitution))
        (nested-formula (sexp->formula '(:imp z (:imp a (:not b)))))
        (negated-formula (sexp->formula '(:not ph)))
        (implication-formula (sexp->formula '(:imp ph ps))))
    (is (eq sigma (substitution-bind! sigma 'x nested-formula)))
    (is (formula= nested-formula (substitution-lookup sigma 'x)))
    (substitution-bind! sigma 'y negated-formula)
    (is (formula= negated-formula (substitution-lookup sigma 'y)))
    (substitution-bind! sigma 'x implication-formula)
    (is (formula= implication-formula (substitution-lookup sigma 'x)))
    (signals error (substitution-bind! sigma nil nested-formula))
    (signals error (substitution-bind! sigma 17 nested-formula))
    (signals error (substitution-bind! sigma 'bad nil))
    (signals error (substitution-bind! sigma 'bad '(:not ph)))))

(test clear-substitution-removes-bindings
  "Clear-substitution! should return the table with all bindings removed."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'x (sexp->formula '(:not ph)))
    (substitution-bind! sigma 'y (sexp->formula '(:imp ph ps)))
    (is (= 2 (hash-table-count sigma)))
    (is (eq sigma (clear-substitution! sigma)))
    (is (= 0 (hash-table-count sigma)))
    (is (not (substitution-lookup sigma 'x)))
    (is (not (substitution-lookup sigma 'y)))))

(test apply-substitution-updates-formulas
  "Apply-substitution should replace bound variables and leave formulas well formed."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'x
                        (sexp->formula '(:imp z (:imp a (:not b)))))
    (let ((result (apply-substitution sigma
                                      (sexp->formula '(:imp x y)))))
      (is (formula-p result))
      (is (equal '(:imp (:imp z (:imp a (:not b))) y)
                 (formula->sexp result)))))
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'x (sexp->formula '(:not z)))
    (substitution-bind! sigma 'y (sexp->formula '(:imp a b)))
    (let ((result (apply-substitution
                   sigma
                   (sexp->formula '(:imp (:not x) (:imp y x))))))
      (is (formula-p result))
      (is (equal '(:imp (:not (:not z)) (:imp (:imp a b) (:not z)))
                 (formula->sexp result)))))
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'ph (sexp->formula '(:imp ps ch)))
    (let ((result (apply-substitution
                   sigma
                   (sexp->formula '(:imp (:imp ph ph) (:not th))))))
      (is (formula-p result))
      (is (equal '(:imp (:imp (:imp ps ch) (:imp ps ch)) (:not th))
                 (formula->sexp result))))))

(test empty-substitution-preserves-formula
  "An empty substitution should leave a formula structurally unchanged."
  (let* ((formula (sexp->formula
                   '(:imp (:imp p (:not q)) (:imp r (:imp p q)))))
         (result (apply-substitution (make-substitution) formula)))
    (is (formula-p result))
    (is (formula= formula result))
    (is (equal (formula->sexp formula)
               (formula->sexp result)))))

(test self-referential-substitution-is-applied-once
  "A substitution whose value mentions the same variable should not recurse forever."
  (let ((sigma (make-substitution)))
    (substitution-bind! sigma 'p (sexp->formula '(:imp p x)))
    (let ((result (apply-substitution
                   sigma
                   (sexp->formula
                    '(:imp (:imp p (:not p)) (:imp p p))))))
      (is (formula-p result))
      (is (equal '(:imp
                   (:imp (:imp p x) (:not (:imp p x)))
                   (:imp (:imp p x) (:imp p x)))
                 (formula->sexp result))))))
