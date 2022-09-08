module Lib
    ( readExpr
    , eval
    , evaledToStr
    )
    where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Except


data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Atom Float deriving (Show)

data CalcError = Parser ParseError
               | InvalidCompute Expr Expr String
               | IncompletEval Expr
               | Unknown String deriving (Show)

type ThrowsError = Either CalcError

evaledToStr :: ThrowsError Float -> String
evaledToStr x = extractRight $ catchError (show <$> x) (Right . show)

extractRight :: ThrowsError a -> a
extractRight (Right v) = v

eval :: Expr -> ThrowsError Float
eval (Atom x) = return x
eval (Add a b) = do x <- eval a
                    y <- eval b
                    return $ x + y
eval (Sub a b) = do x <- eval a
                    y <- eval b
                    return $ x - y
eval (Mul a b) = do x <- eval a
                    y <- eval b
                    return $ x * y
eval (Div a b) = do x <- eval a
                    y <- eval b
                    return $ x / y

toStr :: ThrowsError Expr -> ThrowsError String
toStr (Right (Atom x)) = return . show $ x
toStr (Right y) = throwError $ IncompletEval y
toStr (Left e) = Left e

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
