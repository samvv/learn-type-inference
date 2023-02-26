{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Language.Toy.Parser (
  Parser
, pExpr
, parse
) where

import Control.Ev.Eff
import Data.Char (isDigit)
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Toy.Compiler
import Language.Toy.AST
import qualified Data.Text.Internal.Builder as T
import Data.Text.Encoding (encodeUtf8)

type Parser = Parsec Void T.Text

parse :: (Compile :? e) => Parser a -> FilePath -> T.Text -> Eff e (Maybe a)
parse p fname input
  = case runParser p fname input of
      Left errs -> diagnostic (ParseDiagnostics errs) >> pure Nothing
      Right x -> pure $ Just x

sc :: Parser ()
sc = L.space
  space1 
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

rword :: T.Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["in","if","then","else","true","false","let"]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pIdentifier :: Parser BS.ByteString
pIdentifier = BSC.pack <$> (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else pure x

pIntExpr :: Parser Expr
pIntExpr
  = do digits <- takeWhile1P (Just "a digit") isDigit <* sc
       pure $ Const $ VInt (read $ T.unpack digits :: Integer)

pBoolExpr :: Parser Expr
pBoolExpr 
  = try $ pTrue <|> pFalse
  where pTrue = rword "true" >> pure (Const $ VBool True)
        pFalse = rword "false" >> pure (Const $ VBool False)


pStringExpr :: Parser Expr
pStringExpr
  = do single '\"'
       cs <- many $ satisfy (/= '\"')
       single '\"'
       pure $ Const $ VString $ BSC.pack cs

pConstExpr
  = pIntExpr 
  <|> pStringExpr
  <|> pBoolExpr

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

pExpr'
  = do e1 <- pExpr''
       es <- many pExpr''
       pure $ foldl App e1 es

symbol s = string s *> sc

pExpr = makeExprParser pExpr' [
    [ InfixL (symbol "*" >> pure (App . App (Ref "*")))
    , InfixL (symbol "/" >> pure (App . App (Ref "/"))) ]
  , [ InfixL (symbol "+" >> pure (App . App (Ref "+")))
    , InfixL (symbol "-" >> pure (App . App (Ref "-"))) ]
  ]

pExpr''
  = pConstExpr
  <|> pRefExpr
  <|> pLetExpr
  <|> pLamExpr

