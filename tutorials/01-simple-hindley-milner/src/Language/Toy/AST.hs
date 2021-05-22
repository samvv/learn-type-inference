module Language.Toy.AST where

import qualified Data.ByteString as BS

data Span
  = Span !Int !Int
  deriving (Show, Eq, Ord)

data Value
  = VInt Integer
  | VBool Bool
  | VString String
  deriving (Show, Eq, Ord)

data Expr
  = Let BS.ByteString Expr Expr
  | Ref BS.ByteString
  | App Expr Expr
  | Abs BS.ByteString Expr
  | Const Value
  deriving (Show, Eq)

