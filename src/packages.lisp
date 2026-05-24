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
   #:substitution-lookup
   #:substitution-bind!
   #:clear-substitution!
   #:apply-substitution

   ;; unification operations
   #:unify-formulas!

   ;; axioms
   #:p2-axiom-1
   #:p2-axiom-2
   #:p2-axiom-3

   ;; condensed detachment
   #:condensed-detach

   ;; proof operations
   #:make-axiom-proof
   #:make-cd-proof
   #:axiom-proof-p
   #:cd-proof-p
   #:proof-p
   #:proof-size
   #:proof-depth))
