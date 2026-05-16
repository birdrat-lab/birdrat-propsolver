(defstruct prop-var
  name)

(defstruct negation
  formula)

(defstruct implication
  from
  to)


(defun formula-fold (formula prop-var-case negation-case implication-case malformed-case)
  "Syntactic helper for various formula functions. Wrapper that lets one recurse
over a formula."
  (cond
    ((prop-var-p formula)
     (funcall prop-var-case (prop-var-name formula)))
    ((negation-p formula)
     (funcall negation-case
      (formula-fold (negation-formula formula)
		    prop-var-case
		    negation-case
		    implication-case
		    malformed-case)))
    ((implication-p formula)
     (funcall implication-case
	      (formula-fold (implication-from formula)
			    prop-var-case
			    negation-case
			    implication-case
			    malformed-case)
	      (formula-fold (implication-to formula)
		       prop-var-case
		       negation-case
		       implication-case
		       malformed-case)))
    (t (funcall malformed-case formula))))

(defun formula-p (maybe-formula)
  "For a given S expression, checks if it's a formula"
  (let ((prop-var-case (lambda (f) t))
	(negation-case (lambda (result) (and t result)))
	(implication-case (lambda (left-result right-result) (and t left-result right-result)))
	(malformed-case (lambda (f) nil)))
    (formula-fold formula prop-var-case negation-case implication-case malformed-case)))


(defun formula-depth (formula)
  "Calculates formula depth, i.e. how many operators are applied in one expression."
  (let ((prop-var-case (lambda (a) 0))
	(negation-case (lambda (result) (+ 1 result)))
	(implication-case (lambda (left-result right-result) (+ 1 (max left-result right-result))))
	(malformed-case (lambda (f)  (error "Malformed formula: ~S" f))))
    (formula-fold formula prop-var-case negation-case implication-case malformed-case)))
     
(defun formula-size (formula)
  "Calculates formula size, i.e. how many symbols there are in the whole expression."
  (let ((prop-var-case (lambda (a) 1))
	(negation-case (lambda (result) (+ 1 result)))
	(implication-case (lambda (left-result right-result) (+ 1 left-result right-result)))
	(malformed-case (lambda (f)  (error "Malformed formula: ~S" f))))
    (formula-fold formula prop-var-case negation-case implication-case malformed-case)))
