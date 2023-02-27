module Language.Toy.TypeEnv (
  TypeEnv,
  lookup,
  add,
  empty,
  fromList
) where

import Prelude hiding (lookup)

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import Language.Toy.Types

newtype TypeEnv = TE (Map.Map BS.ByteString Scheme)

lookup k (TE m) = Map.lookup k m

add :: BS.ByteString -> Scheme -> TypeEnv -> TypeEnv
add k ty (TE m) = TE $ Map.insert k ty m

empty :: TypeEnv
empty = TE Map.empty

fromList :: [(BS.ByteString, Scheme)] -> TypeEnv
fromList xs = TE $ Map.fromList xs

instance Substitutable TypeEnv where
  apply s (TE m) =  TE $ Map.map (apply s) m
  ftv (TE env) = ftv $ Map.elems env

