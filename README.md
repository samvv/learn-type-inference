Learn Type Inference
====================

This repository will contain a set of Haskell programs and related material to
teach you how to build type inference algorithms. We start with basic
Hindley-Milner and work our way to dependent types.

Only a basic background in computer science is required. The examples gradually
increase in complexity.

_⚠️ I'm by no means an expert in type theory or functional programming languages.
I'm merely very interested in these topics and spend quite some time learning
them. Always think critical, and take what you read with a grain of salt._

## Tutorials

 - [Hindley-Milner type inference][tut01]
 - [Optimising Hindley-Milner type inference][tut02]

[tut01]: https://github.com/samvv/learn-type-inference/tree/master/tutorials/01-simple-hindley-milner
[tut02]: https://github.com/xnning/EvEff

## Thank You

This repository wouldn't be possible without the following excellent materials:

 - [Write You A Haskell][ref1] by Stephen Diehl provided much of the boilerplate
   for the classical Hindley-Milner code as well as some essential hints on how
   the algorithm works in specific cases.
 - [EvEff][ref2] by Ningning Xie for providing a very clean implementation of
   algebraic effects, making it that much easier to combine monad-like
   computations.
 - [Typing Haskell in Haskell][ref3] by Mark P. Jones contains an essential
   overview of Haskell's type system. 

[ref1]: https://github.com/sdiehl/write-you-a-haskell
[ref2]: https://github.com/xnning/EvEff
[ref3]: https://web.cecs.pdx.edu/~mpj/thih/

## License

The code in this repository is licensed under the MIT license unless otherwise
noted. You are free to fork this repository and make changes. Just make sure to
leave a reference to the original content.

