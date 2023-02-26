{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Control.Monad.State
import System.Console.Repline
import Control.Ev.Eff
import qualified Data.Text as T

import Language.Toy.Pretty
import Language.Toy.Compiler
import Language.Toy.Types
import Language.Toy.Parser (parse, pExpr)
import Language.Toy.Infer (inferExpr)
import qualified Language.Toy.TypeEnv as TE
import qualified Data.Text.IO as TIO

data ReplState = RS { count :: Int, env :: TE.TypeEnv }

type Repl a = HaskelineT (StateT ReplState IO) a

ini :: Repl ()
ini = lift $ liftIO $ putStrLn "Welcome!\nPress Ctrl+D to exit."

cmd :: String -> Repl ()
cmd input
  = do s <- get
       lift $ put $ s { count = count s + 1 }
       let fname = "#<repl:" ++ show (count s) ++ ">"
       let (a, ds) = runCompile $ pipeline fname (T.pack input) (env s)
       when (ds /= []) $ 
         lift $ liftIO $ TIO.putStrLn $ pretty ds
       case a of
         Nothing -> pure ()
         Just res -> lift $ liftIO $ TIO.putStrLn $ pretty res

pipeline :: (Compile :? e) => String -> T.Text -> TE.TypeEnv -> Eff e (Maybe Scheme)
pipeline fname input env
   = do x <- parse pExpr fname $ input
        case x of
          Nothing -> pure Nothing
          Just x -> inferExpr env x >>= pure . Just

final :: Repl ExitDecision
final
  = do lift $ liftIO $ putStrLn "Goodbye!"
       return Exit

completer :: Monad m => WordCompleter m
completer n = do
  let names = ["let", "if"]
  return $ filter (isPrefixOf n) names

defaultTE = TE.fromList [
  ("*", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("/", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("+", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("-", Forall [] (TArrow typeInt (TArrow typeInt typeInt))),
  ("not", Forall [] (TArrow typeBool typeBool)),
  ("and", Forall [] (TArrow typeBool (TArrow typeBool typeBool))),
  ("or", Forall [] (TArrow typeBool (TArrow typeBool typeBool)))
  ]

initState :: ReplState
initState = RS { count = 1, env = defaultTE }

main :: IO ()
main
  = void $ runStateT (evalRepl (const . pure $ "> ") cmd [] (Just ':') (Just "paste") (Word completer) ini final) initState

