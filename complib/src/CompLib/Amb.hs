module CompLib.Amb (
  Amb (..)
) where

import CompLib.Eff

newtype Amb e ans
  = Amb {
      toss :: Op () Bool e ans
    }

