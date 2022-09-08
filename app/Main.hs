module Main (main) where

import Lib

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let evaled = (readExpr $ args !! 0) >>= eval
    putStrLn . evaledToStr $ evaled
