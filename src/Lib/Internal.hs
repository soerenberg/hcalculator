module Lib.Internal 
    ( Expr (..)
    , CalcError (..)
    , ThrowsError
    , evaledToStr
    , eval
    , readExpr
    , parseExpr
    , parseTerm
    , parseTermOp
    , parseFactor
    , parseFactorOp
    , parsePrimary
    , parseParenthesized
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
               | DivisionByZero deriving (Eq, Show)

type ThrowsError = Either CalcError

evaledToStr :: ThrowsError Float -> String
evaledToStr x = fromRight "Could not catch error." $
                    catchError (niceShowFloat <$> x) (Right . show)

niceShowFloat :: Float -> String
niceShowFloat x = if x == fromInteger r then show r else show x
    where r = round x :: Integer

eval :: Expr -> ThrowsError Float
eval (Atom x) = return x
eval (Add a b) = (+) <$> (eval a) <*> (eval b)
eval (Sub a b) = (-) <$> (eval a) <*> (eval b)
eval (Mul a b) = (*) <$> (eval a) <*> (eval b)
eval (Div a b) = do x <- eval a
                    y <- eval b
                    case y of
                        0.0 -> throwError DivisionByZero
                        _ -> return $ x / y

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
parseFactor = chainl1 parsePrimary parseFactorOp

parseFactorOp :: Parser (Expr -> Expr -> Expr)
parseFactorOp =
    do spaces
       s <- char '*' <|> char '/'
       spaces
       case s of
           '*' -> return Mul
           '/' -> return Div
           _ -> fail "Error parsing op, expected '*' or '/'."

parsePrimary :: Parser Expr
parsePrimary = parseParenthesized <|> parseNumber

parseParenthesized :: Parser Expr
parseParenthesized = (char '(') >> parseExpr <* (char ')')

parseNumber :: Parser Expr
parseNumber = (Atom . read) <$> (plus <|> minus <|> float) <* spaces
    where plus = char '+' >> float
          minus = (:) <$> char '-' <*> float
          float = (++) <$> int <*> option "" decimal
          decimal = (:) <$> char '.' <*> int
          int = many1 digit
