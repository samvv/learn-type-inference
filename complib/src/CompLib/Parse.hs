{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module CompLib.Parse where

import Data.Char (isDigit, digitToInt)
import qualified Data.Text as T

import CompLib.Eff
import CompLib.Amb
import CompLib.Exn

data ParseDiagnostic
  = NoChoiceMatched

newtype Parse e ans
  = Parse {
      satisfy :: forall a. Op (T.Text -> Maybe (a, T.Text)) a e ans
    }

pChoice :: (Amb :? e) => [Eff e a] -> Eff e a
pChoice [ x ]
  = x
pChoice (x : xs)
  = do b <- perform toss ()
       if b then x else pChoice xs

pSatisfy1 :: (Parse :? e) => (Char -> Bool) -> Eff e T.Text
pSatisfy1 pred
 = perform satisfy f
  where f input = if T.length res == 0 then Nothing else Just (res, T.drop (T.length res) input)
          where res = T.takeWhile pred input

pDigit :: (Parse :? e) => Eff e Integer
pDigit
  = do digits <- pSatisfy1 isDigit
       pure (read $ T.unpack digits :: Integer)

parse :: (Exn :? e) => T.Text -> Eff (Parse :* e) b -> Eff e b
parse input
 = handlerLocalRet input (\x s -> x) $ Parse {
     satisfy = operation $ \p k ->
       do input <- perform lget ()
          case p input of
            Nothing -> perform failure ()
            Just (x, rest) -> perform lput rest *> k x
   }

