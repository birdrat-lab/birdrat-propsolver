# birdrat-propsolver

A propositional proof-search system.

The project is intended to separate:

- a small Common Lisp proof kernel,
- symbolic search, initially MCTS,
- optional Python-side learning guidance,
- export/checking interfaces for Metamath and Lean.

The initial target is short proof search for propositional logic benchmarks, especially condensed-detachment-style proof corpora.
