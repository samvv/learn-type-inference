Learn Type Inference
====================

This repository will contain a set of Haskell programs and related material to
teach you how to build type inference algorithms. We start with basic
Hindley-Milner and work our way to dependent types.

Only a basic background in computer science is required. The examples gradually
increase in complexity.

## Tutorials

 - [Hindley-Milner type inference][https://github.com/samvv/learn-type-inference/tree/master/tutorials/01-simple-hindley-milner]
 - [Optimising Hindley-Milner type inference][https://github.com/samvv/learn-type-inference/tree/master/tutorials/02-optimising-hindley-milner]

## Compiler Library

To support the tutorials and potentially make it easier to create new
languages, this repository contains [a separate package][1] that does some of
the heavy lifting. It contains generic helpers to structure the code, parser
combinators and more.

[1]: https://github.com/samvv/learn-type-inference/tree/master/complib

## License

The code in this repository is licensed under the MIT license unless otherwise
noted. You are free to fork this repository and make changes. Just make sure to
leave a reference to the original content.

