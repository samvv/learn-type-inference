{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module CompLib.Exn (
  Exn (..),
  toMaybe,
  exceptDefault
) where

import CompLib.Eff

newtype Exn e ans
  = Exn {
      failure :: forall a. Op () a e ans
    }

toMaybe :: Eff (Exn :* e) a -> Eff e (Maybe a)
toMaybe
  = handlerRet Just $ Exn {
      failure = operation $ \() _ -> pure Nothing
    }

exceptDefault :: a -> Eff (Exn :* e) a -> Eff e a
exceptDefault x
  = handler $ Exn {
      failure = operation $ \() _ -> pure x
    }

