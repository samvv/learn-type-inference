module Language.Toy.Types where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

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

data Scheme
  = Forall [TVar] Type
  deriving (Show, Eq)

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

