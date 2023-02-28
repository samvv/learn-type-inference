{-# LANGUAGE GADTs, OverloadedStrings, TypeOperators, FlexibleContexts, Rank2Types #-}

module Language.Toy.Compiler where

import qualified Data.Text as T
import Control.Ev.Eff
import Control.Ev.Util

class Pretty a where
  pretty :: a -> T.Text

instance Pretty a => Pretty [a] where
  pretty [] = ""
  pretty [ x ] = pretty x
  pretty (h:t) = pretty h <> "\n" <> pretty t

data Diagnostic where
  MkDiag :: Pretty d => d -> Diagnostic

instance Pretty Diagnostic where
  pretty (MkDiag d) = pretty d

--data Diagnostic d
--  = ParseDiagnostics (ParseErrorBundle T.Text Void)
--  | CustomDiagnostic d
--  deriving (Show, Eq)

type DiagnosticBundle = [Diagnostic]

type Compile = Writer DiagnosticBundle

runCompile :: Eff (Compile :* ()) a -> (a, DiagnosticBundle)
runCompile m = runEff (writer m)

diagnostic :: (Compile :? e, Pretty d) => d -> Eff e ()
diagnostic d = perform tell [ MkDiag d ]

