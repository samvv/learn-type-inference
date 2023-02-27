module Language.Toy.AST where

import qualified Data.ByteString as BS

data Span
  = Span !Int !Int
  deriving (Show, Eq, Ord)

data Value
  = VInt Integer
  | VBool Bool
  | VString BS.ByteString
  deriving (Show, Eq, Ord)

data Toplevel
  = Def BS.ByteString Expr
  | Expr Expr

data Expr
  = Let BS.ByteString Expr Expr
  | Ref BS.ByteString
  | App Expr Expr
  | Lam BS.ByteString Expr
  | Const Value
  deriving (Show, Eq)

