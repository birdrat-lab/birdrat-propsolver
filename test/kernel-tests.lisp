(in-package #:birdrat-propsolver/test)

(def-suite kernel-tests)
(in-suite kernel-tests)

(defun run-tests ()
  (run! 'kernel-tests))

(test sexp-formula-round-trip
  "Converting a formula sexp to a formula and back should keep the same sexp."
  (dolist (sexp '(ph
                  (:not ph)
                  (:imp ph ps)
                  (:imp (:not ph) (:imp ps ph))
                  (:imp (:imp (:not ph) ps) (:not (:imp ch ph)))))
    (is (equal sexp (formula->sexp (sexp->formula sexp))))))

(test formula-sexp-round-trip
  "Converting a formula to a sexp and back should keep the same formula."
  (let ((formulas
          (list
           (make-prop-var :name 'ph)
           (make-negation :formula (make-prop-var :name 'ph))
           (make-implication :from (make-prop-var :name 'ph)
                             :to (make-prop-var :name 'ps))
           (make-implication
            :from (make-negation :formula (make-prop-var :name 'ph))
            :to (make-implication :from (make-prop-var :name 'ps)
                                  :to (make-prop-var :name 'ph))))))
    (dolist (formula formulas)
      (is (formula= formula (sexp->formula (formula->sexp formula)))))))

(test formula-p-basic-and-complicated
  "Formula-p should accept valid formulas and reject nil or malformed formulas."
  (let* ((prop-var (make-prop-var :name 'ph))
         (negation (make-negation :formula (make-prop-var :name 'ph)))
         (implication (make-implication :from (make-prop-var :name 'ph)
                                        :to (make-prop-var :name 'ps)))
         (complicated
           (make-implication
            :from (make-implication
                   :from (make-negation :formula (make-prop-var :name 'ph))
                   :to (make-implication :from (make-prop-var :name 'ps)
                                         :to (make-prop-var :name 'ch)))
            :to (make-implication
                 :from (make-prop-var :name 'ps)
                 :to (make-negation :formula (make-prop-var :name 'ph)))))
         (malformed
           (make-implication
            :from (make-implication
                   :from (make-negation :formula (make-prop-var :name 'ph))
                   :to (make-implication :from 2
                                         :to (make-prop-var :name 'ch)))
            :to (make-implication
                 :from (make-prop-var :name 'ps)
                 :to (make-negation :formula (make-prop-var :name 'ph))))))
    (is (formula-p prop-var))
    (is (formula-p negation))
    (is (formula-p implication))
    (is (not (formula-p nil)))
    (is (formula-p complicated))
    (is (not (formula-p malformed)))))

(test formula-depth-examples
  "Formula-depth should count the deepest nesting of operators."
  (is (= 0 (formula-depth (sexp->formula 'ph))))
  (is (= 1 (formula-depth (sexp->formula '(:not ph)))))
  (is (= 1 (formula-depth (sexp->formula '(:imp ph ps)))))
  (is (= 3 (formula-depth (sexp->formula '(:imp (:imp (:not ph) ps) ch))))))

(test formula-size-examples
  "Formula-size should count the total symbols in each formula tree."
  (is (= 1 (formula-size (sexp->formula 'ph))))
  (is (= 2 (formula-size (sexp->formula '(:not ph)))))
  (is (= 3 (formula-size (sexp->formula '(:imp ph ps)))))
  (is (= 6 (formula-size (sexp->formula '(:imp (:imp (:not ph) ps) ch))))))

(test formula-vars-complicated
  "Formula-vars should return each unique propositional variable once."
  (let ((vars (formula-vars
               (sexp->formula
                '(:imp
                  (:imp (:not ph) (:imp ps ch))
                  (:imp ps (:not ph)))))))
    (is (null (set-difference '(ph ps ch) vars :test #'eq)))
    (is (null (set-difference vars '(ph ps ch) :test #'eq)))))

(test formula-equality
  "Formula= should identify matching formulas and distinguish different formulas."
  (is (formula= (sexp->formula 'ph)
                (sexp->formula 'ph)))
  (is (formula= (sexp->formula '(:imp (:not ph) ps))
                (sexp->formula '(:imp (:not ph) ps))))
  (is (not (formula= (sexp->formula 'ph)
                     (sexp->formula 'ps))))
  (is (not (formula= (sexp->formula '(:imp (:not ph) ps))
                     (sexp->formula '(:imp (:not ph) ch))))))

(test sexp-to-formula-malformed-inputs
  "Sexp->formula should signal an error for sexps that are not formulas."
  (dolist (sexp '(nil
                  ()
                  17
                  (:not)
                  (:not ph ps)
                  (:imp ph)
                  (:imp ph ps ch)
                  (:and ph ps)))
    (signals error (sexp->formula sexp))))

(test formula-operations-malformed-inputs
  "Formula operations that expect formulas should signal errors on malformed input."
  (dolist (formula '(nil
                     ph
                     (:not ph)
                     (:imp ph ps)
                     17))
    (signals error (formula-depth formula))
    (signals error (formula-size formula))
    (signals error (formula-vars formula))
    (signals error (formula->sexp formula))))

(test formula-equality-malformed-inputs
  "Formula= should return false, not signal an error, for malformed unequal inputs."
  (is (not (formula= 'ph '(:not ph))))
  (is (not (formula= '(:not ph) '(:imp ph ph))))
  (is (not (formula= '(:imp ph ps) '(:imp ps ph)))))

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
