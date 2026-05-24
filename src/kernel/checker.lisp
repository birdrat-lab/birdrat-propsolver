(in-package #:birdrat-propsolver/kernel)

(defun axiom-proof-formula (name)
  "Translates an axiom proof symbol to a formula."
  (cond
    ((eq name :p2-axiom-1) (p2-axiom-1))
    ((eq name :p2-axiom-2) (p2-axiom-2))
    ((eq name :p2-axiom-3) (p2-axiom-3))
    (t nil)))
			       
(defun freshen-formula (formula)
  "Return a copy of FORMULA with fresh propositional variables."
  (let ((renaming-table (make-substitution)))
    (labels ((freshen-rec (f)
               (cond
                 ((prop-var-p f)
                  (let* ((old-name (prop-var-name f))
                         (existing (substitution-lookup renaming-table old-name)))
                    (if existing
                        existing
                        (let ((fresh-var
                                (make-prop-var
                                 :name (gensym (symbol-name old-name)))))
                          (substitution-bind! renaming-table old-name fresh-var)
                          fresh-var))))

                 ((negation-p f)
                  (make-negation
                   :formula (freshen-rec (negation-formula f))))

                 ((implication-p f)
                  (make-implication
                   :from (freshen-rec (implication-from f))
                   :to   (freshen-rec (implication-to f)))))))
      (freshen-rec formula))))


(defun check-proof (proof)
  "Return the formula derived by PROOF, or NIL if PROOF does not check."
  (cond
    ((axiom-proof-p proof)
     (let ((formula
             (axiom-proof-formula proof)))
       (if formula
           (freshen-formula formula)
           nil)))

    ((cd-proof-p proof)
     (let ((major-formula
             (check-proof (second proof))))
       (if (null major-formula)
           nil
           (let ((minor-formula
                   (check-proof (third proof))))
             (if (null minor-formula)
                 nil
                 (condensed-detach major-formula
                                    minor-formula))))))

    (t
     nil)))
