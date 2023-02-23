module Language.Toy.Parser (
  Parser
, pExpr
) where

import Data.Char (isDigit)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Text.Megaparsec.Char.Lexer as L

import Debug.Trace

import Language.Toy.AST

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space
  space1 
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

rword :: T.Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["in","if","then","else","true","false","not","and","or","let"]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pIdentifier :: Parser BS.ByteString
pIdentifier = BSC.pack <$> (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else pure x

pConstExpr :: Parser Expr
pConstExpr
  = do digits <- takeWhile1P (Just "a digit") isDigit <* sc
       pure $ Const $ VInt (read $ T.unpack digits :: Integer)

pRefExpr
  = Ref <$> pIdentifier

pLamExpr 
  = do single '\\' *> sc
       param <- pIdentifier
       string "->" *> sc
       body <- pExpr
       pure $ Lam param body

pLetExpr
  = do rword "let"
       name <- pIdentifier
       single '=' *> sc
       e1 <- pExpr
       rword "in"
       e2 <- pExpr
       pure $ Let name e1 e2

pExpr
  = do e1 <- pExpr'
       es <- many pExpr'
       pure $ foldl App e1 es

pExpr'
  = pConstExpr
  <|> pRefExpr
  <|> pLetExpr
  <|> pLamExpr

