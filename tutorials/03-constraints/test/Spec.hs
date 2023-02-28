{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Main where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Control.Ev.Eff
import Control.Ev.Util (exceptEither, Except)

import Language.Toy.AST
import Language.Toy.Compiler
import Language.Toy.Types
import Language.Toy.Infer (inferExpr, Scheme(Forall))
import qualified Language.Toy.TypeEnv as TE

testCompile :: Eff (Compile :* ()) a -> a
testCompile m = if null ds then a else error $ T.unpack $ pretty ds
  where (a, ds) = runCompile m

defaultTE = TE.fromList [ ("substr", Forall [] (TArrow typeString (TArrow typeInt typeString))) ]

main :: IO ()
main = hspec $ do
  describe "a Toy language inferencer" $ do
    it "correctly infers Int for an integer literal" $ do
       testCompile (inferExpr TE.empty (Const (VInt 1))) `shouldBe` (Forall [] typeInt)
    it "correctly infers Bool for an boolean literal" $ do
       testCompile (inferExpr TE.empty (Const (VInt 1))) `shouldBe` (Forall [] typeInt)
    it "correctly infers String for an string literal" $ do
       testCompile (inferExpr TE.empty (Const (VInt 1))) `shouldBe` (Forall [] typeInt)
    it "correctly infers the identity function" $ do
       testCompile (inferExpr TE.empty (Lam "x" (Ref "x"))) `shouldBe` (Forall [ TV "a" ] (TArrow (TVar $ TV "a") (TVar $ TV "a")))
    it "correctly infers the identity function applied to a boolean" $ do
       testCompile (inferExpr TE.empty (App (Lam "x" (Ref "x")) (Const (VInt 1)))) `shouldBe` (Forall [] typeInt)
    it "correctly infers a reference to a local let-binding" $ do
       testCompile (inferExpr TE.empty (Let "x" (Const (VInt 1)) (Ref "x"))) `shouldBe` (Forall [] typeInt)

