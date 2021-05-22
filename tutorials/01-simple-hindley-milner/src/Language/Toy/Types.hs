module Language.Toy.Types where

import qualified Data.ByteString as BS

data Type
  = TArrow Type Type
  | TVar BS.ByteString
  | TCon BS.ByteString [Type]
  deriving (Show)

