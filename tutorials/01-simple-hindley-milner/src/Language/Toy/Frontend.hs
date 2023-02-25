module Language.Toy.Frontend where

import Text.Megaparsec (runParser)
import Control.Ev.Eff (runEff)
import Control.Ev.Util (exceptEither)
import qualified Data.Text as T

import Language.Toy.Compiler
import Language.Toy.AST
import Language.Toy.Parser
import Language.Toy.Infer (inferExpr, Scheme)
import qualified Language.Toy.TypeEnv as TE

fParse :: FilePath -> Parser a -> T.Text -> CompileResult a
fParse fname p input
  = case runParser p fname input of
      Left errs -> CompileFailure [(ParseDiagnostics errs)]
      Right x -> CompileSuccess [] x

fInfer :: TE.TypeEnv -> Expr -> CompileResult Scheme
fInfer env x = case runEff $ exceptEither $ inferExpr env x of
  Left d -> CompileFailure [d]
  Right scm -> CompileSuccess [] scm
