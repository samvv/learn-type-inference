{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, Rank2Types #-}

module Language.Toy.CLI where

import Data.List (isPrefixOf)
import Control.Monad.State
import System.Console.Repline
import Control.Ev.Eff
import qualified Data.Text as T
import Data.Kind (Type)

import Language.Toy.Compiler
import Language.Toy.AST (Toplevel)
import Language.Toy.Parser (parse, pToplevel)
import qualified Data.Text.IO as TIO

data family TypeEnv a :: Type

data ReplState env = RS { count :: Int, env :: env }

type Repl env a = HaskelineT (StateT (ReplState env) IO) a

ini :: Repl env ()
ini = lift $ liftIO $ putStrLn "Welcome!\nPress Ctrl+D to exit."

type Pipeline env a = Toplevel -> env -> Eff (Compile :* ()) (Maybe (env, a))

cmd :: Pretty a => Pipeline env a -> String -> Repl env ()
cmd pipeline input
  = do s <- get
       lift $ put $ s { count = count s + 1 }
       let fname = "#<repl:" ++ show (count s) ++ ">"
       let (res, ds) = runCompile $ do
              x <- parse pToplevel fname (T.pack input)
              case x of
                Nothing -> pure Nothing
                Just node -> pipeline node (env s)
       unless (null ds) $ 
         lift $ liftIO $ TIO.putStrLn $ pretty ds
       case res of
         Nothing -> pure ()
         Just (env', a) ->
           do lift $ modify $ \s -> s { env = env' }
              lift $ liftIO $ TIO.putStrLn $ pretty a

--pipeline :: (Compile :? e, Pretty a) => String -> T.Text -> env -> Eff e (Maybe (env, a))
--pipeline fname input env
--          Just d -> inferToplevel d env >>= pure . Just

final :: Repl env ExitDecision
final
  = do lift $ liftIO $ putStrLn "Goodbye!"
       return Exit

completer :: Monad m => WordCompleter m
completer n = do
  let names = ["let", "if"]
  return $ filter (isPrefixOf n) names

cli :: Pretty a => Pipeline env a -> env -> IO ()
cli pipeline initEnv
  = void $ runStateT (evalRepl (const . pure $ "> ") (cmd pipeline) [] (Just ':') (Just "paste") (Word completer) ini final) RS { count = 1, env = initEnv }

