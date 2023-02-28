{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Main where

import Control.Ev.Eff

import Language.Toy.Compiler
import Language.Toy.AST (Toplevel)
import Language.Toy.Types
import Language.Toy.Parser (parse, pToplevel)
import Language.Toy.Infer (inferToplevel)
import Language.Toy.CLI
import qualified Language.Toy.TypeEnv as TE

pipeline :: (Compile :? e) => Toplevel -> TE.TypeEnv -> Eff e (Maybe (TE.TypeEnv, Scheme))
pipeline d env
   = inferToplevel d env >>= pure . Just

defaultTE = TE.fromList [
  ("*", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("/", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("+", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("-", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("not", Forall [] (TArrow typeBool typeBool)),
  ("and", Forall [] (TArrow typeBool (TArrow typeBool typeBool))),
  ("or", Forall [] (TArrow typeBool (TArrow typeBool typeBool)))
  ]

main :: IO ()
main
  = cli pipeline defaultTE

