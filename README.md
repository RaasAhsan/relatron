# relatron

A library for typed relational and logic programming in Scala, based on µkanren.

## Motivation
There are many reasons why a typed embedding of relational programming is beneficial. Several are described here:
1. Soundness: well-typed Scala code implies well-typed relational queries. In other words, it is harder to write an unsound relational program.
2. Reification: by expecting the user to ascribe the types of the free variables of their queries, the interpreter can automatically figure out how to reify internal state into metalanguage terms.
3. Language extensions: easily support extensions to the core language via ad hoc polymorphism (type classes).

## Features
* Standard library
  * Natural numbers (Peano encoding)
  * Lists
* Arithmetic expressions
  * Evaluation
  * Typing

## Reference
1. Relational Programming in miniKanren: Techniques, Applications, and Implementations (https://scholarworks.iu.edu/dspace/bitstream/handle/2022/8777/Byrd_indiana_0093A_10344.pdf)
3. µKanren: A Minimal Functional Core for Relational Programming (http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
2. Typed Embedding of a Relational Language in OCaml (https://arxiv.org/pdf/1805.11006.pdf)
