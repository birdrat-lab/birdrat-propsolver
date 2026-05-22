(in-package #:birdrat-propsolver/test)

(in-suite kernel-tests)

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

(test formula-to-sexp-malformed-inputs
  "Formula->sexp should signal an error when passed malformed input."
  (dolist (formula '(nil
                     ph
                     (:not ph)
                     (:imp ph ps)
                     17))
    (signals error (formula->sexp formula))))
