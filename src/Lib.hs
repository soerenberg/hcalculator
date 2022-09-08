module Lib
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except


data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Atom Float deriving (Show)

data CalcError = Parser ParseError
               | Unknown String deriving (Show)

type ThrowsError = Either CalcError

readExpr :: String -> ThrowsError Expr
readExpr s  = case parse parseExpr "hcalc" s of
                Left e -> throwError $ Parser e
                Right v -> return v

parseExpr :: Parser Expr
parseExpr = parseNumber

parseNumber :: Parser Expr
parseNumber = (Atom . read) <$> many1 digit
