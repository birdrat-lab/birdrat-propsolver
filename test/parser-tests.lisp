(in-package #:birdrat-propsolver/test)

(in-suite kernel-tests)

(test sexp-formula-round-trip
  "Converting a formula sexp to a formula and back should keep the same sexp."
  (dolist (sexp '(ph
                  (:not ph)
                  (:imp ph ps)
                  (:imp (:not ph) (:imp ps ph))
                  (:imp (:imp (:not ph) ps) (:not (:imp ch ph)))))
    (is (equal sexp (formula->sexp (sexp->formula sexp))))))

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
