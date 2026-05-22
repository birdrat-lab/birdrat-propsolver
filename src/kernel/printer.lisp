(in-package #:birdrat-propsolver/kernel)


(defun formula->sexp (formula)
  "Converts a formula to a printable S-expression"
  (formula-fold
   formula
   (lambda (f) f)
   (lambda (f) (list :not f))
   (lambda (left-f right-f) (list :imp left-f right-f))
   (lambda (f)  (error "Malformed formula: ~S" f))))
