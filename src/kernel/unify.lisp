(in-package #:birdrat-propsolver/kernel)

(defun layered-substitution-lookup (base delta symbol)
  "Look up SYMBOL in DELTA first, then BASE."
  (or (substitution-lookup delta symbol)
      (substitution-lookup base symbol)))

(defun walk-formula (formula sigma delta &optional seen)
  "Follow top-level prop-var bindings through SIGMA and DELTA."
  (if (not (prop-var-p formula))
      formula
      (let ((name (prop-var-name formula)))
        (if (member name seen :test #'eq)
            nil
            (let ((binding
                    (layered-substitution-lookup
                     sigma
                     delta
                     name)))
              (if binding
                  (walk-formula binding sigma delta (cons name seen))
                  formula))))))

(defun occurs-check-fails-p (symbol formula sigma delta)
  "Return true iff binding SYMBOL to FORMULA would be cyclic or unsafe."
  (cond
    ((prop-var-p formula) (let ((walked (walk-formula formula sigma delta)))
			    (cond
			      ((null walked) t)
			      ((prop-var-p walked) (eq symbol (prop-var-name walked)))
			      (t (occurs-check-fails-p symbol walked sigma delta)))))

    ((negation-p formula) (occurs-check-fails-p symbol
						(negation-formula formula)
						sigma
						delta))

    ((implication-p formula) (or
			      (occurs-check-fails-p symbol
						    (implication-from formula)
						    sigma
						    delta)
			      (occurs-check-fails-p symbol
						    (implication-to formula)
						    sigma
						    delta)))))

(defun unify-bind-symbol! (symbol formula sigma delta)
  "Tentatively bind SYMBOL to FORMULA in DELTA."
  (let ((existing (layered-substitution-lookup sigma delta symbol)))
    (cond
      (existing (unify-formulas-internal! existing
					  formula
					  sigma
					  delta))
      (t
       (let ((walked (walk-formula formula sigma delta)))
         (cond
           ((null walked) nil)
           ((and (prop-var-p walked) (eq symbol (prop-var-name walked))) t)
           ((occurs-check-fails-p symbol walked sigma delta) nil)
           (t (substitution-bind! delta symbol walked) t)))))))

(defun unify-formulas-internal! (left right sigma delta)
  "Unify LEFT and RIGHT, writing tentative bindings to DELTA."
  (let ((left-walked (walk-formula left sigma delta))
        (right-walked (walk-formula right sigma delta)))
    (cond
      ((or (null left-walked) (null right-walked)) nil)
      ((formula= left-walked right-walked) t)
      ((prop-var-p left-walked) (unify-bind-symbol! (prop-var-name left-walked)
						    right-walked
						    sigma
						    delta))
      ((prop-var-p right-walked) (unify-bind-symbol! (prop-var-name right-walked)
						     left-walked
						     sigma
						     delta))
      ((and (negation-p left-walked) (negation-p right-walked)) (unify-formulas-internal! (negation-formula left-walked)
											  (negation-formula right-walked)
											  sigma
											  delta))
      ((and (implication-p left-walked) (implication-p right-walked)) (and
								       (unify-formulas-internal! (implication-from left-walked)
												 (implication-from right-walked)
												 sigma
												 delta)
								       (unify-formulas-internal! (implication-to left-walked)
												 (implication-to right-walked)
												 sigma
												 delta)))
      (t nil))))


(defun commit-substitution-delta! (sigma delta)
  "Commit all tentative bindings from DELTA into SIGMA."
  (maphash
   #'(lambda (key val)
       (substitution-bind! sigma key val))
   delta)
  sigma)


(defun unify-formulas! (left right sigma)
  "Unify LEFT and RIGHT, mutating SIGMA only on success."
  (let ((delta
          (make-substitution
           :test (hash-table-test sigma))))
    (if (unify-formulas-internal! left right sigma delta)
        (commit-substitution-delta! sigma delta)
        nil)))
