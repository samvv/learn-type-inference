Generic Compiler Library for Haskell
====================================

This directory contains a Haskell package that combines several utilities for
compiler development into one big optimised library.

## Contents

The following list will give you an idea of what's inside this library. Note
that not everything may be implemented yet, as this is very much a work in
progress.

 - A foundational **algebraic effect system** based on the work of Ningning Xie
   and Daan Leijen. Almost all other components of this library make use of it.
 - Some **opinionated parser combinators** that are tuned to work on highly
   structured source code and not on binary data. The combinators are explicitly
   not polymorphic in the type of underlying text stream, in the hope that GHC
   et al. can make better optimisation decisions.
 - Basic building blocks for creating a **read-eval-print loop**.
 - Helper types and functions for performing **type inference**.
 - Some utilities for pretty-printing text ranges etc.

In the future, this library may be extended with specific code generation targets.

## License

Different parts of the code are licensed under different licenses. All
copyright goes to the original authors. See `package.yaml` for more information
about the authors.

