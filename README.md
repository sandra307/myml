MyML is a simplified ML-like language interpreter written in OCaml. This project was inspired by the Cornell CS3110 course and lectures from IITM's CS3100 (Paradigms of Programming).
The interpreter supports:
Typed lambda calculus with Hindley-Milner type inference
Basic types: integers, booleans
Functions and let-bindings
Conditionals (if-then-else)
A simple REPL to test expressions interactively

I was deeply fascinated by how languages understand types and ensure correctness. Through this project, I explored:
How expressions are represented and evaluated
How types are inferred using unification
How environments and substitutions work behind the scenes
How a simple language can still feel powerful and expressive

This project taught me:
How to work with recursive data types and variants in OCaml
How Hindley-Milner type inference works (beyond theory!)
The joy (and frustration) of debugging parsers and unifiers :)
Grateful to Sheera Shamsu Ma'am for introducing me to OCaml and thanks to CS3110 which made me realise how beautiful, efficient and correct programming in OCaml can be.

Now about the project structure,this project contains-
ast.ml – Defines the abstract syntax tree (AST) for the MiniML-like language, covering constructs like integers, booleans, functions, variables, and control flow.
types.ml – Contains type representations (TInt, TBool, TFun, etc.) and utility functions for unification and substitutions used in type inference.
lexer.ml – A handwritten lexer that converts raw input into a sequence of tokens like LET, FUN, IF, THEN, and more.
parser.ml – A handcrafted recursive-descent parser that processes tokens from the lexer and constructs the corresponding AST.
infer.ml – Implements Hindley-Milner type inference, including polymorphic type generalization, instantiation, and unification.
env.ml – Manages the typing environment, supporting generalization, lookup, and extension of variable bindings.
eval.ml – Implements the big-step operational semantics to evaluate expressions defined by the AST.
repl.ml – Acts as the main entry point. It provides an interactive Read-Eval-Print Loop, allowing users to type expressions, see their inferred types, and evaluate them on the fly.
dune, dune-project – Build configuration files for compiling and running the project using the Dune build system.

This project was a hands-on journey through core concepts in type systems and interpreters. Building each layer manually helped me better appreciate the elegance behind functional language design.
