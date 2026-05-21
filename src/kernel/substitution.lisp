(in-package #:birdrat-propsolver/kernel)
 
(defun make-substitution (&key (test #'eq))
  "Create a fresh empty substitution table."
  (make-hash-table :test test))

(defun copy-substitution (sigma &key (test (hash-table-test sigma)))
  "Return a shallow copy of substitution SIGMA."
  (let ((sigma-copy
          (make-hash-table
           :test test
           :size (hash-table-count sigma))))
    (maphash
     #'(lambda (key val)
         (setf (gethash key sigma-copy) val))
     sigma)
    sigma-copy))

(defun substitution-lookup (sigma symbol)
  "Return the formula bound to SYMBOL in SIGMA, or NIL if unbound."
  (gethash symbol sigma))

(defun substitution-bind! (sigma key val)
  "Destructively binds KEY to VAL in SIGMA"
  (if (and key (symbolp key) (formula-p val))
      (progn
	(setf (gethash key sigma) val)
	sigma)
      (error "Malformed binding setting ~S to ~S" key val)))

(defun clear-substitution! (sigma)
  "Remove all bindings from substitution SIGMA"
  (clrhash sigma)
  sigma)

(defun apply-substitution (sigma formula)
  "Apply substitution SIGMA to FORMULA in one structural pass."
  (formula-fold formula
                (lambda (prop-var-name)
                  (let ((binding
                          (substitution-lookup sigma prop-var-name)))
                    (if binding
                        binding
                        (make-prop-var :name prop-var-name))))
                (lambda (f)
                  (make-negation :formula f))
                (lambda (left-f right-f)
                  (make-implication :from left-f
                                    :to right-f))
                (lambda (f)
                  (error "Malformed formula: ~S" f))))
