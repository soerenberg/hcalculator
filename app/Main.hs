module Main (main) where

import Lib

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let res = readExpr $ args !! 0
    putStrLn . show $ res
