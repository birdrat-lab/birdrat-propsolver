(defpackage #:birdrat-propsolver/kernel
  (:use #:cl)
  (:export
   ;; formula structs and generated constructors/accessors
   #:prop-var
   #:make-prop-var
   #:prop-var-p
   #:prop-var-name

   #:negation
   #:make-negation
   #:negation-p
   #:negation-formula

   #:implication
   #:make-implication
   #:implication-p
   #:implication-from
   #:implication-to

   ;; formula operations
   #:formula-p
   #:formula-depth
   #:formula-size
   #:formula-vars
   #:formula=

   ;; surface syntax conversion
   #:sexp->formula
   #:formula->sexp

   ;; substitution operations
   #:make-substitution
   #:copy-substitution
   #:substitution-bound-symbol-p
   #:substitution-lookup
   #:substitution-bind!
   #:clear-substitution!
   #:apply-substitution))
