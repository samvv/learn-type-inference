module Main where

import Language.Toy.AST
import Language.Toy.Compiler (Diagnostic(..), CompileResult(..))
import Language.Toy.Frontend (fParse)
import Language.Toy.Parser
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

testParse :: Parser a -> T.Text -> Maybe a
testParse p input
  = case fParse "#<anonymous>" p input of
      CompileFailure ds -> Nothing
      CompileSuccess ds x -> Just x

main :: IO ()
main = hspec $ do
  describe "a Toy language parser" $ do
    it "can parse a simple constant expression" $ do
      testParse pExpr "1" `shouldBe` Just (Const (VInt 1))

