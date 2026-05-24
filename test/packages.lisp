(defpackage #:birdrat-propsolver/test
  (:use #:cl #:fiveam)
  (:import-from #:birdrat-propsolver/kernel
                #:formula-p
                #:formula-depth
                #:formula-size
                #:formula-vars
                #:formula=
                #:sexp->formula
                #:formula->sexp
                #:make-prop-var
                #:make-negation
                #:make-implication
		#:make-substitution
		#:copy-substitution
		#:substitution-lookup
		#:substitution-bind!
		#:clear-substitution!
		#:apply-substitution
                #:unify-formulas!
                #:p2-axiom-1
                #:p2-axiom-2
                #:p2-axiom-3
                #:apply-unifier
                #:condensed-detach
                #:make-axiom-proof
                #:make-cd-proof
                #:axiom-proof-p
                #:cd-proof-p
                #:proof-p
                #:proof-size
                #:proof-depth
                #:axiom-proof-formula
                #:freshen-formula
                #:check-proof)
  (:export #:run-tests))
