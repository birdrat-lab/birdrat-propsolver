(in-package #:birdrat-propsolver/kernel)

(defun condensed-detach (major minor)
  "Apply condensed detachment to MAJOR and MINOR, or return NIL."
  (if (not (implication-p major))
      nil
      (let ((sigma (make-substitution)))
        (if (unify-formulas! (implication-from major)
                             minor
                             sigma)
            (apply-unifier sigma
                           (implication-to major))
            nil))))

(defun apply-unifier (sigma formula)
  "Apply unifier SIGMA to FORMULA, following substitution chains."
  (let ((delta (make-substitution :test (hash-table-test sigma))))
    (labels ((apply-rec (f)
	       (cond
		 ((prop-var-p f) (let ((walked (walk-formula f sigma delta)))
				   (cond
				     ((null walked) nil)
				     ((eq f walked) f)
				     (t (apply-rec walked)))))
		 ((negation-p f) (let ((child (apply-rec (negation-formula f))))
				   (if (null child)
				       nil
				       (make-negation :formula child))))
		 ((implication-p f) (let ((left (apply-rec (implication-from f))))
				      (if (null left)
					  nil
					  (let ((right (apply-rec (implication-to f))))
					    (if (null right)
						nil
						(make-implication :from left :to right)))))))))
      (apply-rec formula))))
