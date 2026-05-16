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
  "For a passed object, checks if it's a formula"
  (formula-fold maybe-formula
		(lambda (f) (and (prop-var-name f) (symbolp (prop-var-name f))))
		(lambda (result) (and t result))
		(lambda (left-result right-result) (and t left-result right-result))
		(lambda (f) nil)))

(defun formula-depth (formula)
  "Calculates formula depth, i.e. how many operators are applied in one expression."
  (formula-fold maybe-formula
		(lambda (a) 0)
		(lambda (result) (+ 1 result))
		(lambda (left-result right-result) (+1 (max left-result right-result)))
		(lambda (f)  (error "Malformed formula: ~S" f))))


(defun formula-vars (formula)
  "Count the number of variables in a formula."
  (formula-fold maybe-formula
		(lambda (a) 1)
		(lambda (result) (result))
		(lambda (left-result right-result) (+ left-result right-result))
		(lambda (f)  (error "Malformed formula: ~S" f))))


(defun formula-size (formula)
  "Calculates formula size, i.e. how many symbols there are in the whole expression."
  (formula-fold maybe-formula
		(lambda (a) 1)
		(lambda (result) (+ 1 result))
		(lambda (left-result right-result) (+ 1 left-result right-result))
		(lambda (f)  (error "Malformed formula: ~S" f))))


