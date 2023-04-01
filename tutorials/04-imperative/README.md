Constraint-based Type Inference
===============================

This directory contains a sample type inference program that consists of a
separate constraint solver. The constraint solver receives constraints from the
inference step. Once inference is finished, the solver solves all constraints
at once.

The constraints can be best seen as a 'micro-language' with its own set of
particular syntactic rules and a specific method to 'evaluate' the constraints.
Evaluation in this context corresponds to finding the right substitution that
correctly types the entire program.

## Running This Program

Make sure you have [Stack][3] installed. Then, run the following command in a
terminal:

```sh
stack exec main
```

This will open up a REPL where you can experiment with the toy language.

[3]: https://haskellstack.org/

