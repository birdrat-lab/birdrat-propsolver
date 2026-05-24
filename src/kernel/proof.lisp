(in-package #:birdrat-propsolver/kernel)

(defun make-axiom-proof (name)
  "Create a proof object for a known P2 axiom name."
  (if
   (or (eq name :p2-axiom-1)
       (eq name :p2-axiom-2)
       (eq name :p2-axiom-3))
   name
   (error "Malformed axiom passed: ~S" name)))

(defun make-cd-proof (major minor)
  "Create a condensed-detachment proof from MAJOR and MINOR proofs."
  (list :cd major minor))

(defun axiom-proof-p (proof)
  "Return true when PROOF is one of the known axiom proof names."
  (and
   (symbolp proof)
    (or (eq proof :p2-axiom-1)
	(eq proof :p2-axiom-2)
	(eq proof :p2-axiom-3))))

(defun cd-proof-p (proof)
  "Return true when PROOF has the shape of a condensed-detachment proof."
  (and
   (listp proof)
   (= (length proof) 3)
   (eq (first proof) :cd)))

(defun proof-p (proof)
  "Return true when PROOF is a well-formed proof tree."
  (cond
    ((axiom-proof-p proof) t)
    ((and
      (cd-proof-p proof)
      (proof-p (second proof))
      (proof-p (third proof))) t)
    (t nil)))

(defun proof-size (proof)
  "Return the number of nodes in PROOF."
  (cond
    ((axiom-proof-p proof) 1)
    ((cd-proof-p proof) (+ 1 (proof-size (second proof)) (proof-size (third proof))))
    (t (error "Malformed proof ~S" proof))))

(defun proof-depth (proof)
  "Return the maximum condensed-detachment nesting depth of PROOF."
  (cond
    ((axiom-proof-p proof) 0)
    ((cd-proof-p proof) (+ 1 (max (proof-depth (second proof)) (proof-depth (third proof)))))
    (t (error "Malformed proof ~S" proof))))
