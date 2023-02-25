{-# LANGUAGE OverloadedStrings #-}

module Language.Toy.Pretty where

import qualified Data.Text as T

import Language.Toy.Types
import Data.Text.Encoding (decodeUtf8)

class Pretty a where
  pretty :: a -> T.Text

instance Pretty TVar where
  pretty (TV x) = T.pack x

instance Pretty Type where
  pretty (TVar x) = pretty x
  pretty (TArrow a b) = pretty a <> " -> " <> pretty b
  pretty (TCon x) = decodeUtf8 x

instance Pretty Scheme where
  pretty (Forall [] ty) = pretty ty
  pretty (Forall tvs ty) = "forall " <> T.intercalate " " (map pretty tvs) <> ". " <> pretty ty

