(in-package #:birdrat-propsolver/test)

(in-suite kernel-tests)

(defun f (sexp)
  (sexp->formula sexp))

(defun normalize-formula-sexp (sexp)
  (cond
    ((keywordp sexp) sexp)
    ((symbolp sexp) (intern (symbol-name sexp) *package*))
    ((consp sexp) (mapcar #'normalize-formula-sexp sexp))
    (t sexp)))

(defun s (formula)
  (normalize-formula-sexp (formula->sexp formula)))

(defun cd-sexp (major-sexp minor-sexp)
  (let ((result (condensed-detach (f major-sexp)
                                  (f minor-sexp))))
    (and result (s result))))

(defun bind-sexp! (sigma symbol formula-sexp)
  (substitution-bind! sigma symbol (f formula-sexp))
  sigma)

(test axiom-schemas-have-expected-shape
  "The P2 axiom constructors should return formulas with the expected shapes."
  (is (equal '(:imp ph (:imp ps ph))
             (s (p2-axiom-1))))
  (is (equal '(:imp
               (:imp ph (:imp ps ch))
               (:imp (:imp ph ps)
                     (:imp ph ch)))
             (s (p2-axiom-2))))
  (is (equal '(:imp
               (:imp (:not ph) (:not ps))
               (:imp ps ph))
             (s (p2-axiom-3)))))

(test axiom-schemas-are-well-formed
  "The P2 axiom constructors should return well-formed formulas."
  (is (formula-p (p2-axiom-1)))
  (is (formula-p (p2-axiom-2)))
  (is (formula-p (p2-axiom-3)))
  (is (formula= (p2-axiom-1) (p2-axiom-1)))
  (is (formula= (p2-axiom-2) (p2-axiom-2)))
  (is (formula= (p2-axiom-3) (p2-axiom-3))))

(test apply-unifier-empty-substitution
  "Apply-unifier should leave a formula unchanged when sigma has no bindings."
  (let* ((sigma (make-substitution))
         (formula (f '(:imp ph (:not ps))))
         (result (apply-unifier sigma formula)))
    (is (formula= formula result))
    (is (equal (s formula) (s result)))))

(test apply-unifier-direct-binding
  "Apply-unifier should replace a directly bound variable."
  (let ((sigma (make-substitution)))
    (bind-sexp! sigma 'ph '(:not ps))
    (is (equal '(:not ps)
               (s (apply-unifier sigma (f 'ph)))))))

(test apply-unifier-unbound-variable
  "Apply-unifier should leave an unbound variable unchanged."
  (let ((sigma (make-substitution)))
    (is (equal 'ph
               (s (apply-unifier sigma (f 'ph)))))))

(test apply-unifier-recurses-through-negation
  "Apply-unifier should apply bindings under negation."
  (let ((sigma (make-substitution)))
    (bind-sexp! sigma 'ph '(:imp ps ch))
    (is (equal '(:not (:imp ps ch))
               (s (apply-unifier sigma (f '(:not ph))))))))

(test apply-unifier-recurses-through-implication
  "Apply-unifier should apply bindings on both sides of an implication."
  (let ((sigma (make-substitution)))
    (bind-sexp! sigma 'ph '(:not a))
    (bind-sexp! sigma 'ps 'b)
    (is (equal '(:imp (:not a) b)
               (s (apply-unifier sigma (f '(:imp ph ps))))))))

(test apply-unifier-chain-binding
  "Apply-unifier should follow chained variable bindings."
  (let ((sigma (make-substitution)))
    (bind-sexp! sigma 'ph 'ps)
    (bind-sexp! sigma 'ps '(:not ch))
    (is (equal '(:not ch)
               (s (apply-unifier sigma (f 'ph)))))))

(test apply-unifier-chain-inside-compound-formula
  "Apply-unifier should follow chained bindings inside compound formulas."
  (let ((sigma (make-substitution)))
    (bind-sexp! sigma 'ph '(:imp ps ch))
    (bind-sexp! sigma 'ps '(:not a))
    (is (equal '(:imp (:not a) ch)
               (s (apply-unifier sigma (f 'ph)))))))

(test apply-unifier-cycle-returns-nil
  "Apply-unifier should return nil instead of looping on cyclic substitutions."
  (let ((sigma (make-substitution)))
    (bind-sexp! sigma 'ph 'ps)
    (bind-sexp! sigma 'ps 'ph)
    (is (null (apply-unifier sigma (f 'ph))))))

(test cd-axiom-1
  "Condensed detachment with axiom 1 should instantiate the consequent."
  (let ((atomic-result (condensed-detach (p2-axiom-1) (f 'a)))
        (compound-result (condensed-detach (p2-axiom-1) (f '(:not ch)))))
    (is (formula-p atomic-result))
    (is (equal '(:imp ps a) (s atomic-result)))
    (is (formula-p compound-result))
    (is (equal '(:imp ps (:not ch)) (s compound-result)))))

(test cd-simple-success-cases
  "Condensed detachment should instantiate consequents with successful unifiers."
  (let ((repeated-result (cd-sexp '(:imp ph (:imp ph ph))
                                  '(:not ch)))
        (unbound-result (cd-sexp '(:imp ph ps)
                                 '(:not ch)))
        (contained-result (cd-sexp '(:imp ph (:imp ps ph))
                                   '(:not ch))))
    (is (equal '(:imp (:not ch) (:not ch)) repeated-result))
    (is (equal 'ps unbound-result))
    (is (equal '(:imp ps (:not ch)) contained-result))))

(test cd-axiom-2
  "Condensed detachment with axiom 2 should instantiate all implication variables."
  (let ((result (condensed-detach (p2-axiom-2)
                                  (f '(:imp a (:imp b c))))))
    (is (formula-p result))
    (is (equal '(:imp (:imp a b)
                 (:imp a c))
               (s result)))))

(test cd-axiom-3
  "Condensed detachment with axiom 3 should instantiate contraposition variables."
  (let ((result (condensed-detach (p2-axiom-3)
                                  (f '(:imp (:not a) (:not b))))))
    (is (formula-p result))
    (is (equal '(:imp b a)
               (s result)))))

(test cd-non-implication-major-fails
  "Condensed detachment should fail when the major premise is not an implication."
  (is (null (condensed-detach (f '(:not ph))
                              (f 'ph)))))

(test cd-constructor-mismatch-fails
  "Condensed detachment should fail when antecedent and minor constructors mismatch."
  (is (null (cd-sexp '(:imp (:not ph) ps)
                    '(:imp a b)))))

(test cd-occurs-check-fails
  "Condensed detachment should fail when antecedent unification fails occurs-check."
  (is (null (cd-sexp '(:imp ph q)
                    '(:imp ph ps))))
  (is (null (cd-sexp '(:imp ph q)
                    '(:not ph)))))

(test cd-resolves-unifier-chains
  "Condensed detachment should apply the unifier by following chains."
  (is (equal '(:not ch)
             (cd-sexp '(:imp (:imp ph ps) ph)
                      '(:imp ps (:not ch)))))
  (is (equal '(:imp (:not ch) (:not ch))
             (cd-sexp '(:imp (:imp ph ps)
                        (:imp ph ps))
                      '(:imp ps (:not ch))))))
