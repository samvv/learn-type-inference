module Language.Toy.Parser where

import Data.Char (isDigit)
import Data.Void (Void)
import Language.Toy.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space
  space1 
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

rword :: T.Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

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
  = do digits <- takeWhile1P (Just "a digit") isDigit
       pure $ Const $ VInt (read $ T.unpack digits :: Integer)

pRefExpr
  = Ref <$> pIdentifier

pExpr
  = pConstExpr
  <|> pRefExpr

