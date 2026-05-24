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
                #:condensed-detach)
  (:export #:run-tests))
