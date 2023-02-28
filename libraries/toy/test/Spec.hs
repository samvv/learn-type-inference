{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts, Rank2Types #-}

module Main where

import qualified Data.Text as T
import Test.Hspec
import Control.Ev.Eff

import Language.Toy.AST
import Language.Toy.Compiler
import Language.Toy.Parser (Parser, pToplevel, pExpr, parse)

execCompile :: Eff (Compile :* ()) a -> a
execCompile m = if null ds then a else error $ T.unpack $ pretty ds
  where (a, ds) = runCompile m

testParse :: Parser a -> T.Text -> Maybe a
testParse p input
  = execCompile $ parse p "#<test>" input

main :: IO ()
main = hspec $ do
  describe "a Toy language parser" $ do
    it "can parse a simple reference" $ do
      testParse pExpr "foo" `shouldBe` Just (Ref "foo")
    it "can parse a simple constant expression" $ do
      testParse pExpr "1" `shouldBe` Just (Const (VInt 1))
    it "can parse a simple let declaration" $ do
      testParse pExpr "let a = 1 in a" `shouldBe` Just (Let "a" (Const (VInt 1)) (Ref "a"))
    it "can parse a let declaration with some parameters" $ do
      testParse pExpr "let add a b = a + b in add 1 2" `shouldBe` Just (Let "add" (Lam "a" (Lam "b" (App (App (Ref "+") (Ref "a")) (Ref "b")))) (App (App (Ref "add") (Const (VInt 1))) (Const (VInt 2))))
    it "can parse a simple lambda abstraction" $ do
      testParse pExpr "\\x -> x" `shouldBe` Just (Lam "x" (Ref "x"))
    it "can parse a lambda abstraction with multiple parameters" $ do
      testParse pExpr "\\x y -> x + y" `shouldBe` Just (Lam "x" (Lam "y" (App (App (Ref "+") (Ref "x")) (Ref "y"))))
    it "can parse a simple application" $ do
      testParse pExpr "f 1" `shouldBe` Just (App (Ref "f") (Const (VInt 1)))
    it "constructs applications left-to-right" $ do
      testParse pExpr "f x y z" `shouldBe` Just (App (App (App (Ref "f") (Ref "x")) (Ref "y")) (Ref "z"))
    it "can parse a simple definition" $ do
      testParse pToplevel "let x = 1" `shouldBe` Just (Def "x" (Const (VInt 1)))
    it "can parse a definition with some parameters" $ do
      testParse pToplevel "let add a b = a + b" `shouldBe` Just (Def "add" (Lam "a" (Lam "b" (App (App (Ref "+") (Ref "a")) (Ref "b")))))

