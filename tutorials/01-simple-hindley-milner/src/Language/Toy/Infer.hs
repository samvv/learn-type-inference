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
import Control.Monad (replicateM, when)
import Data.Maybe (fromMaybe)
import Data.List (nub)

import Language.Toy.Types
import qualified Language.Toy.TypeEnv as TE
import Language.Toy.AST
import Language.Toy.Compiler

newtype InferState
  = InferState {
      count :: Int
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

runInfer :: (Compile :? e) => Eff (Infer :* e) (Subst, Type) -> Eff e Scheme
runInfer m 
  = do res <- state InferState { count = 0 } m
       pure $ closeOver res

closeOver :: (Subst, Type) -> Scheme
closeOver (sub, ty) = normalize scm
  where scm = generalize TE.empty (apply sub ty)

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

infer :: (Compile :? e, Infer :? e) => TE.TypeEnv -> Expr -> Eff e (Subst, Type)
infer env expr  = case expr of

  Ref x -> case TE.lookup x env of
    Nothing -> do diagnostic (BindingNotFound x)
                  t <- fresh
                  pure (Map.empty, t)
    Just scm -> instantiate scm >>= \t -> pure (Map.empty, t)

  Lam x body -> do
    paraTy <- fresh
    let env' = TE.add x (mono paraTy) env
    (s1, retTy) <- infer env' body
    pure (s1, apply s1 paraTy `TArrow` retTy)

  App e1 e2 -> do
    retTy <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (TArrow t2 retTy)
    pure (s3 <> s2 <> s1, apply s3 retTy)

  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (TE.add x t' env') e2
    return (s2 <> s1, t2)

  Const v -> pure (Map.empty, valTy v)

  where valTy (VInt _) = typeInt
        valTy (VString _) = typeString
        valTy (VBool _) = typeBool

inferToplevel :: (Compile :? e) => Toplevel -> TE.TypeEnv -> Eff e (TE.TypeEnv, Scheme)
inferToplevel (Def name e) env
  = do scm <- inferExpr env e
       pure (TE.add name scm env, scm)
inferToplevel (Expr e) env
  = do scm <- inferExpr env e
       pure (env, scm)

inferExpr :: (Compile :? e) => TE.TypeEnv -> Expr -> Eff e Scheme
inferExpr te expr
  = runInfer $ infer te expr

occursCheck a t = a `Set.member` ftv t

bind :: (Compile :? e) => TVar -> Type -> Eff e Subst
bind (TV x) (TVar (TV y)) | x == y
  = pure Map.empty
bind tv ty | occursCheck tv ty
  = do diagnostic OccursCheck
       pure Map.empty
bind tv ty
  = pure $ Map.fromList [(tv, ty)]

unify :: (Compile :? e) => Type -> Type -> Eff e Subst
unify (TVar x) ty = bind x ty
unify ty (TVar x) = bind x ty
unify (TArrow a1 a2) (TArrow b1 b2)
  = do s1 <- unify a1 b1
       s2 <- unify (apply s1 a2) (apply s1 b2)
       pure $ s2 <> s1
unify (TCon x) (TCon y) | x == y
  = pure Map.empty
unify a b
  = do diagnostic (UnificationError a b)
       pure Map.empty
   

