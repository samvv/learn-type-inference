{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Main where

import qualified Data.Text as T
import Control.Ev.Eff

import Language.Toy.Compiler
import Language.Toy.AST
import Language.Toy.Infer (inferToplevel)
import qualified Language.Toy.TypeEnv as TE
import Language.Toy.Types
import Language.Toy.CLI

pipeline :: (Compile :? e) => Toplevel -> TE.TypeEnv -> Eff e (Maybe (TE.TypeEnv, Scheme))
pipeline x env
   = inferToplevel x env >>= pure . Just

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

