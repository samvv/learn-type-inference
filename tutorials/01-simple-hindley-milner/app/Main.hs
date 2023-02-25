module Main where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Control.Monad.State
import System.Console.Repline
import qualified Data.Text as T

import Language.Toy.Compiler
import Language.Toy.Frontend (fParse, fInfer)
import Language.Toy.Parser (pExpr)
import qualified Language.Toy.TypeEnv as TE
import Language.Toy.Infer (inferExpr)

data ReplState = RS { count :: Int, env :: TE.TypeEnv }

type Repl a = HaskelineT (StateT ReplState IO) a

ini :: Repl ()
ini = lift $ liftIO $ putStrLn "Welcome!\nPress Ctrl+D to exit."

output = lift . liftIO . putStrLn

cmd :: String -> Repl ()
cmd input = do s <- get
               lift $ put $ s { count = count s + 1 }
               let fname = "#<repl:" ++ show (count s) ++ ">"
               case fParse fname pExpr $ T.pack input of
                  CompileFailure ds -> output $ "parse error: " <> show ds
                  CompileSuccess ds expr -> case fInfer (env s) expr of
                    CompileFailure ds -> output $ "type-checking error: " <> show ds
                    CompileSuccess ds scm -> output $ show scm

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

