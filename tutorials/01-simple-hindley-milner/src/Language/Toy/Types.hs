module Language.Toy.Types where

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Toy.Compiler

newtype TVar = TV String
  deriving (Eq, Ord, Show)

data Type
  = TArrow Type Type
  | TVar TVar
  | TCon BS.ByteString
  deriving (Show, Eq)

typeInt = TCon "Int"
typeString = TCon "String"
typeBool = TCon "Bool"

instance Pretty TVar where
  pretty (TV x) = T.pack x

instance Pretty Type where
  pretty (TVar x) = pretty x
  pretty (TArrow a b) = pretty a <> " -> " <> pretty b
  pretty (TCon x) = decodeUtf8 x

data TypeError
  = OccursCheck
  | BindingNotFound BS.ByteString
  | UnificationError Type Type

instance Pretty TypeError where
  pretty (BindingNotFound x) = "type checking error: binding '" <> decodeUtf8 x <> "' could not be found"
  pretty (UnificationError a b) = "type checking error: types " <> pretty a <> " and " <> pretty b <> " do not match"
  pretty OccursCheck = "type checking error: a type variable ocurred somewhere inside its own definition, which is not allowed"

data Scheme
  = Forall [TVar] Type
  deriving (Show, Eq)

instance Pretty Scheme where
  pretty (Forall [] ty) = pretty ty
  pretty (Forall tvs ty) = "forall " <> T.intercalate " " (map pretty tvs) <> ". " <> pretty ty

type Subst = Map.Map TVar Type

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable Type where

  apply _ (TCon a)         = TCon a
  apply s t@(TVar a)       = Map.findWithDefault t a s
  apply s (t1 `TArrow` t2) = apply s t1 `TArrow` apply s t2

  ftv TCon{}           = Set.empty
  ftv (TVar a)         = Set.singleton a
  ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as
