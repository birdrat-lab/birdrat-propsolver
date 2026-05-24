(in-package #:birdrat-propsolver/test)

(in-suite kernel-tests)

(defun checker-f (sexp)
  (sexp->formula sexp))

(defun checker-normalize-sexp (sexp)
  (cond
    ((keywordp sexp) sexp)
    ((symbolp sexp) (intern (symbol-name sexp) *package*))
    ((consp sexp) (mapcar #'checker-normalize-sexp sexp))
    (t sexp)))

(defun checker-s (formula)
  (checker-normalize-sexp (formula->sexp formula)))

(defun axiom-1-shape-p (formula)
  (and
   (implication-p formula)
   (let ((outer-left (implication-from formula))
         (outer-right (implication-to formula)))
     (and
      (prop-var-p outer-left)
      (implication-p outer-right)
      (let ((inner-left (implication-from outer-right))
            (inner-right (implication-to outer-right)))
        (and
         (prop-var-p inner-left)
         (prop-var-p inner-right)
         (eq (prop-var-name outer-left)
             (prop-var-name inner-right))
         (not (eq (prop-var-name outer-left)
                  (prop-var-name inner-left)))))))))

(defun axiom-2-shape-p (formula)
  (and
   (implication-p formula)
   (let* ((left (implication-from formula))
          (right (implication-to formula)))
     (and
      (implication-p left)
      (implication-p right)
      (let* ((ph-1 (implication-from left))
             (left-to (implication-to left))
             (right-from (implication-from right))
             (right-to (implication-to right)))
        (and
         (prop-var-p ph-1)
         (implication-p left-to)
         (implication-p right-from)
         (implication-p right-to)
         (let ((ps-1 (implication-from left-to))
               (ch-1 (implication-to left-to))
               (ph-2 (implication-from right-from))
               (ps-2 (implication-to right-from))
               (ph-3 (implication-from right-to))
               (ch-2 (implication-to right-to)))
           (and
            (every #'prop-var-p (list ps-1 ch-1 ph-2 ps-2 ph-3 ch-2))
            (eq (prop-var-name ph-1) (prop-var-name ph-2))
            (eq (prop-var-name ph-1) (prop-var-name ph-3))
            (eq (prop-var-name ps-1) (prop-var-name ps-2))
            (eq (prop-var-name ch-1) (prop-var-name ch-2))
            (not (eq (prop-var-name ph-1) (prop-var-name ps-1)))
            (not (eq (prop-var-name ph-1) (prop-var-name ch-1)))
            (not (eq (prop-var-name ps-1) (prop-var-name ch-1)))))))))))

(defun axiom-3-shape-p (formula)
  (and
   (implication-p formula)
   (let ((left (implication-from formula))
         (right (implication-to formula)))
     (and
      (implication-p left)
      (implication-p right)
      (let ((not-ph (implication-from left))
            (not-ps (implication-to left))
            (ps (implication-from right))
            (ph (implication-to right)))
        (and
         (negation-p not-ph)
         (negation-p not-ps)
         (prop-var-p (negation-formula not-ph))
         (prop-var-p (negation-formula not-ps))
         (prop-var-p ps)
         (prop-var-p ph)
         (eq (prop-var-name (negation-formula not-ph))
             (prop-var-name ph))
         (eq (prop-var-name (negation-formula not-ps))
             (prop-var-name ps))
         (not (eq (prop-var-name ph)
                  (prop-var-name ps)))))))))

(test axiom-proof-formula-known-axioms
  "Axiom proof names should map to the expected raw axiom formulas."
  (is (equal '(:imp ph (:imp ps ph))
             (checker-s (axiom-proof-formula :p2-axiom-1))))
  (is (equal '(:imp
               (:imp ph (:imp ps ch))
               (:imp (:imp ph ps)
                     (:imp ph ch)))
             (checker-s (axiom-proof-formula :p2-axiom-2))))
  (is (equal '(:imp
               (:imp (:not ph) (:not ps))
               (:imp ps ph))
             (checker-s (axiom-proof-formula :p2-axiom-3)))))

(test axiom-proof-formula-invalid-input
  "Unknown or malformed axiom proof inputs should return nil."
  (is (null (axiom-proof-formula :not-an-axiom)))
  (is (null (axiom-proof-formula '(:cd :p2-axiom-1 :p2-axiom-2))))
  (is (null (axiom-proof-formula nil)))
  (is (null (axiom-proof-formula 17))))

(test freshen-formula-well-formed
  "Freshening should produce well-formed formulas with fresh variable symbols."
  (let ((fresh-axiom (freshen-formula (p2-axiom-1)))
        (fresh-var (freshen-formula (checker-f 'ph))))
    (is (formula-p fresh-axiom))
    (is (prop-var-p fresh-var))
    (is (not (eq 'ph (prop-var-name fresh-var))))))

(test freshen-formula-repeated-variable-consistency
  "Freshening should preserve repeated-variable relationships in one formula."
  (let* ((fresh (freshen-formula (checker-f '(:imp ph (:imp ps ph)))))
         (outer-left (implication-from fresh))
         (outer-right (implication-to fresh))
         (inner-left (implication-from outer-right))
         (inner-right (implication-to outer-right)))
    (is (formula-p fresh))
    (is (prop-var-p outer-left))
    (is (prop-var-p inner-left))
    (is (prop-var-p inner-right))
    (is (eq (prop-var-name outer-left)
            (prop-var-name inner-right)))
    (is (not (eq (prop-var-name outer-left)
                 (prop-var-name inner-left))))))

(test freshen-formula-distinct-variables
  "Freshening should map distinct variables to distinct fresh symbols."
  (let* ((fresh (freshen-formula (checker-f '(:imp ph ps))))
         (left (implication-from fresh))
         (right (implication-to fresh)))
    (is (prop-var-p left))
    (is (prop-var-p right))
    (is (not (eq (prop-var-name left)
                 (prop-var-name right))))))

(test freshen-formula-preserves-shape
  "Freshening should preserve formula constructors and repeated variables."
  (let* ((fresh (freshen-formula (checker-f '(:imp (:not ph) (:imp ps ph)))))
         (left (implication-from fresh))
         (right (implication-to fresh))
         (left-ph (negation-formula left))
         (right-ph (implication-to right)))
    (is (implication-p fresh))
    (is (negation-p left))
    (is (implication-p right))
    (is (prop-var-p left-ph))
    (is (prop-var-p right-ph))
    (is (eq (prop-var-name left-ph)
            (prop-var-name right-ph)))))

(test freshen-formula-independent-calls
  "Independent freshening calls should produce independent symbols."
  (let* ((fresh-1 (freshen-formula (p2-axiom-1)))
         (fresh-2 (freshen-formula (p2-axiom-1)))
         (name-1 (prop-var-name (implication-from fresh-1)))
         (name-2 (prop-var-name (implication-from fresh-2))))
    (is (formula-p fresh-1))
    (is (formula-p fresh-2))
    (is (not (eq name-1 name-2)))))

(test freshen-formula-axiom-2-relationships
  "Freshening axiom 2 should preserve each repeated variable relationship."
  (let ((fresh (freshen-formula (p2-axiom-2))))
    (is (formula-p fresh))
    (is (axiom-2-shape-p fresh))))

(test check-proof-axiom-leaves
  "Checking axiom leaves should return fresh well-formed axiom instances."
  (let ((axiom-1 (check-proof :p2-axiom-1))
        (axiom-2 (check-proof :p2-axiom-2))
        (axiom-3 (check-proof :p2-axiom-3)))
    (is (and axiom-1 (formula-p axiom-1)))
    (is (axiom-1-shape-p axiom-1))
    (is (and axiom-2 (formula-p axiom-2)))
    (is (axiom-2-shape-p axiom-2))
    (is (and axiom-3 (formula-p axiom-3)))
    (is (axiom-3-shape-p axiom-3))))

(test check-proof-axiom-leaves-are-fresh
  "Repeated checks of the same axiom should produce independent fresh symbols."
  (let* ((first-proof (check-proof :p2-axiom-1))
         (second-proof (check-proof :p2-axiom-1))
         (first-name (prop-var-name (implication-from first-proof)))
         (second-name (prop-var-name (implication-from second-proof))))
    (is (formula-p first-proof))
    (is (formula-p second-proof))
    (is (not (eq first-name second-name)))))

(test check-proof-invalid-proofs
  "Malformed proofs should fail by returning nil."
  (dolist (proof '(:not-a-proof
                   (:not-cd :p2-axiom-1 :p2-axiom-2)
                   (:cd)
                   (:cd :p2-axiom-1)
                   (:cd :p2-axiom-1 :p2-axiom-2 :p2-axiom-3)
                   (:cd :bad :p2-axiom-1)
                   (:cd :p2-axiom-1 :bad)
                   nil
                   17
                   "proof"))
    (is (null (check-proof proof)))))

(test check-proof-cd-basic
  "Checking a CD proof should evaluate child proofs and apply condensed detachment."
  (let ((result (check-proof (make-cd-proof :p2-axiom-1 :p2-axiom-1))))
    (is (not (null result)))
    (is (formula-p result)))
  (let ((result (check-proof '(:cd :p2-axiom-1 :p2-axiom-1))))
    (is (not (null result)))
    (is (implication-p result))))

(test check-proof-nested-cd-smoke
  "Nested CD proofs should recursively check without signaling errors."
  (let ((result (check-proof '(:cd
                               :p2-axiom-1
                               (:cd :p2-axiom-1 :p2-axiom-1)))))
    (is (or (null result)
            (formula-p result)))))

(test check-proof-freshening-regression
  "Identical axiom leaves in a CD proof should not fail due to shared schema variables."
  (let ((result (check-proof '(:cd :p2-axiom-1 :p2-axiom-1))))
    (is (not (null result)))
    (is (formula-p result))))
