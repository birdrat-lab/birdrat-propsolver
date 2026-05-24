(in-package #:birdrat-propsolver/kernel)

(defun p2-axiom-1 ()
  (sexp->formula '(:imp ph (:imp ps ph))))

(defun p2-axiom-2 ()
  (sexp->formula '(:imp
		   (:imp ph (:imp ps ch))
		   (:imp (:imp ph ps)
		    (:imp ph ch)))))
(defun p2-axiom-3 ()
  (sexp->formula '(:imp
		   (:imp (:not ph) (:not ps))
		   (:imp ps ph))))
  
    
