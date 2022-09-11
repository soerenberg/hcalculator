module Lib.Internal 
    ( Expr
    , CalcError
    , ThrowsError
    , evaledToStr
    , eval
    , toStr
    , readExpr
    , parseExpr
    , parseTerm
    , parseTermOp
    , parseFactor
    , parseFactorOp
    , parseUnary
    , parseNumber
    ) where

import Text.ParserCombinators.Parsec
    ( (<|>)
    , Parser
    , ParseError
    , chainl1
    , char
    , digit
    , many1
    , parse
    , option
    , spaces
    )
import Control.Monad.Except (catchError, throwError)
import Data.Either (fromRight)


data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Atom Float deriving (Eq, Show)

data CalcError = Parser ParseError
               | InvalidCompute Expr Expr String
               | IncompletEval Expr
               | DivisionByZero
               | Unknown String deriving (Eq, Show)

type ThrowsError = Either CalcError

evaledToStr :: ThrowsError Float -> String
evaledToStr x = fromRight "Could not catch error." $
                    catchError (show <$> x) (Right . show)

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
                    case y of
                        0.0 -> throwError DivisionByZero
                        _ -> return $ x / y

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
           _ -> fail "Error parsing op, expected '+' or '-'."

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
           _ -> fail "Error parsing op, expected '*' or '/'."

parseUnary :: Parser Expr
parseUnary = parseNumber

parseNumber :: Parser Expr
parseNumber = (Atom . read) <$> (plus <|> minus <|> float) <* spaces
    where plus = char '+' >> float
          minus = (:) <$> char '-' <*> float
          float = (++) <$> int <*> option "" decimal
          decimal = (:) <$> char '.' <*> int
          int = many1 digit
