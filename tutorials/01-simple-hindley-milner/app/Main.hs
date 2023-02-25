module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Control.Monad.State
import System.Console.Repline
import qualified Data.Text as T

import Language.Toy.Pretty
import Language.Toy.Compiler
import Language.Toy.Frontend (fParse, fInfer)
import Language.Toy.Parser (pExpr)
import qualified Language.Toy.TypeEnv as TE
import Language.Toy.Infer (inferExpr)
import qualified Data.Text.IO as TIO

data ReplState = RS { count :: Int, env :: TE.TypeEnv }

type Repl a = HaskelineT (StateT ReplState IO) a

ini :: Repl ()
ini = lift $ liftIO $ putStrLn "Welcome!\nPress Ctrl+D to exit."

cmd :: String -> Repl ()
cmd input = do s <- get
               lift $ put $ s { count = count s + 1 }
               let fname = "#<repl:" ++ show (count s) ++ ">"
               case fParse fname pExpr $ T.pack input of
                  CompileFailure ds -> lift $ liftIO $ TIO.putStrLn $ pretty ds
                  CompileSuccess ds expr -> case fInfer (env s) expr of
                    CompileFailure ds -> lift $ liftIO $ TIO.putStrLn $ pretty ds
                    CompileSuccess ds scm -> lift $ liftIO $ TIO.putStrLn $ pretty scm

final :: Repl ExitDecision
final
  = do lift $ liftIO $ putStrLn "Goodbye!"
       return Exit

completer :: Monad m => WordCompleter m
completer n = do
  let names = ["let", "if"]
  return $ filter (isPrefixOf n) names

initState :: ReplState
initState = RS { count = 1, env = TE.empty }

main :: IO ()
main
  = void $ runStateT (evalRepl (const . pure $ "> ") cmd [] (Just ':') (Just "paste") (Word completer) ini final) initState

