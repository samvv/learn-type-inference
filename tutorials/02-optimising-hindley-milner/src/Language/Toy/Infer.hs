{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Language.Toy.Infer (
  inferExpr
, inferToplevel
, Scheme(..)
) where 

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Control.Ev.Eff
import Control.Ev.Util
import Control.Monad (replicateM, when, foldM)
import Data.Maybe (fromMaybe)
import Data.List (nub)

import Language.Toy.Types
import qualified Language.Toy.TypeEnv as TE
import Language.Toy.AST
import Language.Toy.Compiler

data InferState
  = InferState {
      count :: Int,
      sub :: Subst
    }

type Infer = State InferState

fresh :: (Infer :? e) => Eff e Type
fresh
  = do s <- perform get ()
       let i = count s
       perform put s { count = i+1 } 
       pure $ TVar $ TV $ letters !! i

-- apply :: Subst -> Type -> Type
-- apply s (TArrow t1 t2) = TArrow (apply s t1) (apply s t2)
-- apply s tv@(TVar (TV id)) = fromMaybe tv $ Map.lookup id s
-- apply s (TCon name) = TCon name

-- | Get the freely occurring variables in the given `Type`
-- |
-- | Assuming no `Scheme` is present, every type variable counts as a free variable.

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

withInfer :: (Compile :? e) => Eff (Infer :* e) a -> Eff e a
withInfer
  = state InferState { count = 0, sub = Map.empty }

closeOver :: Type -> Scheme
closeOver ty = normalize scm
  where scm = generalize TE.empty ty

generalize :: TE.TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

normalize (Forall tvs body)
  = Forall (map snd m) (apply body) 
  where m = zip (nub $ fv body) (map TV letters)
        apply ty@(TCon _) = ty
        apply (TArrow p r) = TArrow (apply p) (apply r)
        apply (TVar id) = case lookup id m of
          Nothing -> error "invariant failed to hold"
          Just x -> TVar x
        fv (TVar x) =  [ x ]
        fv (TArrow a b) = fv a <> fv b
        fv (TCon _) = []

mono :: Type -> Scheme
mono = Forall []

instantiate :: (Infer :? e) => Scheme -> Eff e Type
instantiate (Forall tvs t) 
  = do tvs' <- mapM (const fresh) tvs
       let s = Map.fromList $ zip tvs tvs'
       pure $ apply s t

infer :: (Compile :? e, Infer :? e) => TE.TypeEnv -> Expr -> Eff e Type
infer env expr  = case expr of

  Ref x -> case TE.lookup x env of
    Nothing -> do diagnostic (BindingNotFound x)
                  fresh
    Just scm -> instantiate scm

  Lam x body -> do
    paraTy <- fresh
    let env' = TE.add x (mono paraTy) env
    retTy <- infer env' body
    pure $ paraTy `TArrow` retTy

  App e1 e2 -> do
    retTy <- fresh
    t1 <- infer env e1
    t2 <- infer env e2
    unify t1 (TArrow t2 retTy)
    pure retTy

  Let x e1 e2 -> do
    t1 <- infer env e1
    let t' = generalize env t1
    infer (TE.add x t' env) e2

  Const v -> pure $ case v of
    VInt _ -> typeInt
    VString _ -> typeString
    VBool _ -> typeBool

inferToplevel :: (Compile :? e) => Toplevel -> TE.TypeEnv -> Eff e (TE.TypeEnv, Scheme)
inferToplevel (Def name e) env
  = do scm <- inferExpr env e
       pure (TE.add name scm env, scm)
inferToplevel (Expr e) env
  = do scm <- inferExpr env e
       pure (env, scm)

inferExpr :: (Compile :? e) => TE.TypeEnv -> Expr -> Eff e Scheme
inferExpr te expr
  = do scm <- withInfer $ infer te expr >>= find
       pure $ closeOver scm

occursCheck :: TVar -> Type -> Bool
occursCheck a t = a `Set.member` ftv t

find :: (Infer :? e, Substitutable a) => a -> Eff e a
find k
  = do s <- perform get ()
       pure $ apply (sub s) k

assign :: (Infer :? e) => TVar -> Type -> Eff e ()
assign tv ty
  = do s <- perform get ()
       perform put s { sub = Map.insert tv ty (sub s) }

bind :: (Compile :? e, Infer :? e) => TVar -> Type -> Eff e ()
bind (TV x) (TVar (TV y)) | x == y
  = return ()
bind tv ty | occursCheck tv ty
  = diagnostic OccursCheck
bind tv ty
  = do s <- perform get ()
       perform put s { sub = Map.insert tv ty (sub s) }

unify t1 t2
  = do t1' <- find t1
       t2' <- find t2
       unify' t1' t2'

unify' :: (Compile :? e, Infer :? e) => Type -> Type -> Eff e ()
unify' (TVar x) ty = bind x ty
unify' ty (TVar x) = bind x ty
unify' (TArrow a1 a2) (TArrow b1 b2)
  = do unify a1 b1
       unify a2 b2
unify' (TCon x) (TCon y) | x == y
  = pure ()
unify' a b
  = diagnostic (UnificationError a b)

