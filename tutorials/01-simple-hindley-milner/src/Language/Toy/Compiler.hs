module Language.Toy.Compiler where

import Data.Void (Void)
import qualified Data.Text as T
import Language.Toy.AST
import Language.Toy.Types
import Text.Megaparsec.Error (ParseErrorBundle)

data TypeWithLoc
  = TypeWithLoc Type Span
  deriving (Show)

data Diagnostic
  = OccursCheck TypeWithLoc
  | ParseDiagnostics (ParseErrorBundle T.Text Void)
  deriving (Show)

type DiagnosticBundle = [Diagnostic]

data CompileResult a
  = CompileSuccess DiagnosticBundle a
  | CompileFailure DiagnosticBundle

