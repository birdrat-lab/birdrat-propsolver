;;;; birdrat-propsolver.asd

(asdf:defsystem "birdrat-propsolver"
  :description "A propositional proof-search system using a small verified kernel, MCTS, and optional learning guidance."
  :author "birdrat-lab"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :serial nil
  :components
  ((:module "src"
    :components
    ((:file "packages")

     (:module "kernel"
      :serial t
      :depends-on ("packages")
      :components
      ((:file "formula")
       (:file "parser")
       (:file "printer")
       (:file "substitution")
       (:file "unify")
       (:file "axioms")
       (:file "condensed-detachment")
       (:file "proof")
       (:file "checker")))

     (:module "search"
      :serial t
      :depends-on ("kernel")
      :components
      ((:file "state")
       (:file "actions")
       (:file "rollout")
       (:file "scoring")
       (:file "transpositions")
       (:file "mcts")))

     (:module "metamath"
      :serial t
      :depends-on ("kernel")
      :components
      ((:file "dproof")
       (:file "pmproofs-parser")
       (:file "benchmarks")))

     (:module "lean"
      :serial t
      :depends-on ("kernel")
      :components
      ((:file "export")
       (:file "check")))

     (:module "protocol"
      :serial t
      :depends-on ("kernel" "search")
      :components
      ((:file "json")
       (:file "python-client")))))))

(asdf:defsystem "birdrat-propsolver/test"
  :description "Tests for birdrat-propsolver."
  :author "birdrat-lab"
  :license "MIT"
  :depends-on ("birdrat-propsolver" "fiveam")
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "packages")
     (:file "kernel-tests")
     (:file "unify-tests")
     (:file "cd-tests")
     (:file "proof-tests")
     (:file "search-tests"))))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :birdrat-propsolver/test
                               :run-tests)))
