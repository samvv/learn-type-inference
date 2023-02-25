{-# LANGUAGE OverloadedStrings #-}

module Language.Toy.Pretty where

import qualified Data.Text as T

import Language.Toy.Compiler
import Language.Toy.Types
import Data.Text.Encoding (decodeUtf8)
import Text.Megaparsec (errorBundlePretty)

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

instance Pretty Diagnostic where
  pretty (BindingNotFound x) = "type checking error: binding '" <> decodeUtf8 x <> "' could not be found"
  pretty (UnificationError a b) = "type checking error: types " <> pretty a <> " and " <> pretty b <> " do not match"
  pretty OccursCheck = "type checking error: a type variable ocurred somewhere inside its own definition, which is not allowed"
  pretty (ParseDiagnostics bundle) = T.pack $ "parse error: " <> errorBundlePretty bundle

instance Pretty a => Pretty [a] where
  pretty [ x ] = pretty x
  pretty (h:t) = pretty h <> "\n" <> pretty t
