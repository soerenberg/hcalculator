module Main (main) where

import Lib

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main =
  do args <- getArgs
     case length args of
        0 -> printIntro >> runRepl
        1 -> evalAndPrint $ args !! 0
        _ -> putStrLn "Pass 0 args for Repl, 1 arg for single evaluation."

runRepl :: IO ()
runRepl = doUntil (== "exit") (readPrompt "hcalc> ") evalAndPrint (putStrLn "")

printIntro :: IO ()
printIntro = putStrLn "--- hcalc ---\nPrint 'exit' to quit."

doUntil :: Monad m => (a -> Bool) -> m a -> (a -> m b) -> m b -> m b
doUntil p x f e =
  do i <- x
     case p i of
       False -> f i >> doUntil p x f e
       True -> e

readPrompt :: String -> IO String
readPrompt s = (putStr s) >> hFlush stdout >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint s = putStrLn . evaledToStr $ (readExpr s) >>= eval
