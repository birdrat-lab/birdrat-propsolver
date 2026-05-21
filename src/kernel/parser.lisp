(in-package #:birdrat-propsolver/kernel)

(defun sexp->formula (sexp)
  (cond
    ((and
      (symbolp sexp)
      sexp)
     (make-prop-var :name sexp))
    ((and
      (listp sexp)
      (= (length sexp) 2)
      (eq (first sexp) :not))
     (make-negation :formula (sexp->formula (second sexp))))
    ((and
      (listp sexp)
      (= (length sexp) 3)
      (eq (first sexp) :imp))
     (make-implication
      :from (sexp->formula (second sexp))
      :to (sexp->formula (third sexp))))
    (t (error "Malformed sexp for formula conversion: ~S" sexp))))
     
     
	     
