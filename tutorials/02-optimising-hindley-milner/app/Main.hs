{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Control.Monad.State
import System.Console.Repline
import Control.Ev.Eff
import qualified Data.Text as T

import Language.Toy.Compiler
import Language.Toy.AST
import Language.Toy.Types
import Language.Toy.Parser (parse, pToplevel)
import Language.Toy.Infer (inferToplevel, inferExpr)
import Language.Toy.Pretty
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
         Just (env', scm) -> do lift $ modify $ \s -> s { env = env' }
                                lift $ liftIO $ TIO.putStrLn $ pretty scm

pipeline :: (Compile :? e) => String -> T.Text -> TE.TypeEnv -> Eff e (Maybe (TE.TypeEnv, Scheme))
pipeline fname input env
   = do x <- parse pToplevel fname $ input
        case x of
          Nothing -> pure Nothing
          Just d -> inferToplevel d env >>= pure . Just

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

