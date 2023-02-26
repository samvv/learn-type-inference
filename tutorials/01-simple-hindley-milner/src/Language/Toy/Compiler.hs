{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Language.Toy.Compiler where

import Data.Void (Void)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Text.Megaparsec.Error (ParseErrorBundle)
import Control.Ev.Eff
import Control.Ev.Util

import Language.Toy.AST
import Language.Toy.Types

data Diagnostic
  = OccursCheck
  | BindingNotFound BS.ByteString
  | UnificationError Type Type
  | ParseDiagnostics (ParseErrorBundle T.Text Void)
  deriving (Show, Eq)

type DiagnosticBundle = [Diagnostic]

data CompileResult a
  = CompileSuccess DiagnosticBundle a
  | CompileFailure DiagnosticBundle

type Compile = Writer DiagnosticBundle

runCompile :: Eff (Compile :* ()) a -> (a, DiagnosticBundle)
runCompile m = runEff (writer m)

diagnostic :: (Compile :? e) => Diagnostic -> Eff e ()
diagnostic d = perform tell [ d ]

