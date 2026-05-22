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
		#:apply-substitution)
  (:export #:run-tests))
