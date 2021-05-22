module Language.Toy.Frontend where

import qualified Data.Text as T
import Text.Megaparsec (runParser)
import Language.Toy.Parser
import Language.Toy.Compiler

fParse :: FilePath -> Parser a -> T.Text -> CompileResult a
fParse fname p input
  = case runParser p fname input of
      Left errs -> CompileFailure [(ParseDiagnostics errs)]
      Right x -> CompileSuccess [] x

