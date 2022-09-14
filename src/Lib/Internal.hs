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
    , (<?>)
    , Parser
    , ParseError
    , chainl1
    , char
    , digit
    , eof
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
          | Pow Expr Expr
          | Atom Float deriving (Eq, Show)

data CalcError = Parser ParseError
               | DivisionByZero
               | NegativeExponent
               | NonIntegralExponent deriving (Eq)

instance Show CalcError where
    show (Parser e) = "Error: " ++ (show e)
    show DivisionByZero = "Division by zero not allowed."
    show NegativeExponent = "Exponent must be non-negative."
    show NonIntegralExponent = "Exponent must be integral."

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
eval (Pow a b) = do
    x <- eval a
    y <- eval b
    let r | y < 0 = throwError NegativeExponent
          | not . isIntegral $ y = throwError NonIntegralExponent
          | otherwise = return $ x ^ (round y :: Int)
    r

isIntegral :: Float -> Bool
isIntegral x = x == fromInteger r
    where r = round x :: Integer


readExpr :: String -> ThrowsError Expr
readExpr s  = case parse (parseExpr <* eof) "hcalc" s of
                Left e -> throwError $ Parser e
                Right v -> return v

parseExpr :: Parser Expr
parseExpr = parseTerm <* spaces

parseTerm :: Parser Expr
parseTerm = chainl1 parseFactor parseTermOp <* spaces

parseTermOp :: Parser (Expr -> Expr -> Expr)
parseTermOp =
    do s <- char '+' <|> char '-'
       spaces
       case s of
           '+' -> return Add
           '-' -> return Sub
           _ -> fail "Error parsing op, expected '+' or '-'."

parseFactor :: Parser Expr
parseFactor = chainl1 parsePower parseFactorOp <* spaces

parseFactorOp :: Parser (Expr -> Expr -> Expr)
parseFactorOp =
    do s <- char '*' <|> char '/'
       spaces
       case s of
           '*' -> return Mul
           '/' -> return Div
           _ -> fail "Error parsing op, expected '*' or '/'."

parsePower :: Parser Expr
parsePower = chainl1 parsePrimary (char '^' >> return Pow <* spaces)

parsePrimary :: Parser Expr
parsePrimary = parseParenthesized <|> parseNumber

parseParenthesized :: Parser Expr
parseParenthesized = (char '(') >> parseExpr <*
                     (char ')' <?> "closing \")\"") <* spaces

parseNumber :: Parser Expr
parseNumber = (Atom . read) <$> (plus <|> minus <|> float) <* spaces <?>
    "a number"
    where plus = char '+' >> float
          minus = (:) <$> char '-' <*> float
          float = (++) <$> int <*> option "" decimal
          decimal = (:) <$> char '.' <*> int
          int = many1 digit
