module Lib
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec
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
parseExpr = parseTerm

parseTerm :: Parser Expr
parseTerm = chainl1 parseFactor parseTermOp

parseTermOp :: Parser (Expr -> Expr -> Expr)
parseTermOp =
    do spaces
       s <- char '+' <|> char '-'
       spaces
       case s of
        '+' -> return Add
        '-' -> return Sub

parseFactor :: Parser Expr
parseFactor = chainl1 parseUnary parseFactorOp

parseFactorOp :: Parser (Expr -> Expr -> Expr)
parseFactorOp =
    do spaces
       s <- char '*' <|> char '/'
       spaces
       case s of
          '*' -> return Mul
          '/' -> return Div

parseUnary :: Parser Expr
parseUnary = parseNegated <|> parseNumber

parseNegated :: Parser Expr
parseNegated = do spaces
                  char '-'
                  expr <- parseNumber
                  spaces
                  case expr of
                    (Atom n) -> return . Atom $ (-n)
                    otherwise -> fail "no negative number"

parseNumber :: Parser Expr
parseNumber = do
    x <- (Atom . read) <$> many1 digit
    spaces
    return x
