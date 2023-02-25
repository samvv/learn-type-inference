module Language.Toy.Compiler where

import Data.Void (Void)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Language.Toy.AST
import Language.Toy.Types
import Text.Megaparsec.Error (ParseErrorBundle)

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

