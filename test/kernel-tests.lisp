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
