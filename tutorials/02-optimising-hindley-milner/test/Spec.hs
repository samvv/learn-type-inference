{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Main where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Control.Ev.Eff
import Control.Ev.Util (exceptEither, Except)

import Language.Toy.AST
import Language.Toy.Compiler
import Language.Toy.Parser (Parser, pExpr, parse)
import Language.Toy.Types
import Language.Toy.Infer (inferExpr, Scheme(Forall))
import Language.Toy.Pretty (pretty)
import qualified Language.Toy.TypeEnv as TE

execCompile :: Eff (Compile :* ()) a -> a
execCompile m = if ds /= [] then error $ T.unpack $ pretty ds else a
  where (a, ds) = runCompile m

testParse :: Parser a -> T.Text -> Maybe a
testParse p input
  = execCompile $ parse p "#<test>" input

defaultTE = TE.fromList [ ("substr", Forall [] (TArrow typeString (TArrow typeInt typeString))) ]

main :: IO ()
main = hspec $ do
  describe "a Toy language parser" $ do
    it "can parse a simple reference" $ do
      testParse pExpr "foo" `shouldBe` Just (Ref "foo")
    it "can parse a simple constant expression" $ do
      testParse pExpr "1" `shouldBe` Just (Const (VInt 1))
    it "can parse a simple let declaration" $ do
      testParse pExpr "let a = 1 in a" `shouldBe` Just (Let "a" (Const (VInt 1)) (Ref "a"))
    it "can parse a simple lambda abstraction" $ do
      testParse pExpr "\\x -> x" `shouldBe` Just (Lam "x" (Ref "x"))
    it "can parse a simple application" $ do
      testParse pExpr "f 1" `shouldBe` Just (App (Ref "f") (Const (VInt 1)))
    it "constructs applications left-to-right" $ do
      testParse pExpr "f x y z" `shouldBe` Just (App (App (App (Ref "f") (Ref "x")) (Ref "y")) (Ref "z"))
  describe "a Toy language inferencer" $ do
    it "correctly infers Int for an integer literal" $ do
       execCompile (inferExpr TE.empty (Const (VInt 1))) `shouldBe` (Forall [] typeInt)
    it "correctly infers Bool for an boolean literal" $ do
       execCompile (inferExpr TE.empty (Const (VInt 1))) `shouldBe` (Forall [] typeInt)
    it "correctly infers String for an string literal" $ do
       execCompile (inferExpr TE.empty (Const (VInt 1))) `shouldBe` (Forall [] typeInt)
    it "correctly infers the identity function" $ do
       execCompile (inferExpr TE.empty (Lam "x" (Ref "x"))) `shouldBe` (Forall [ TV "a" ] (TArrow (TVar $ TV "a") (TVar $ TV "a")))
    it "correctly infers the identity function applied to a boolean" $ do
       execCompile (inferExpr TE.empty (App (Lam "x" (Ref "x")) (Const (VInt 1)))) `shouldBe` (Forall [] typeInt)
    it "correctly infers a reference to a local let-binding" $ do
       execCompile (inferExpr TE.empty (Let "x" (Const (VInt 1)) (Ref "x"))) `shouldBe` (Forall [] typeInt)

