{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Solutions.Day18 where

import qualified Data.Maybe as Maybe
import qualified Either
import qualified List
import qualified Map
import qualified Set
import qualified Text.Read as Read
import qualified Vector

import Foldable hiding (countBy)
import Function
import Functor
import Input hiding (get)

import Text.Parsec (parse, eof, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (try)
import Text.ParserCombinators.Parsec.Char (oneOf, char, digit, string, satisfy)
import Text.ParserCombinators.Parsec.Combinator (between, many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import qualified Text.ParserCombinators.Parsec.Expr as E
--import FunctionsAndTypesForParsing

data Expr = Int Int | Plus Expr Expr | Prod Expr Expr | Parens Expr
  deriving Show

evalExpr :: Expr -> Int
evalExpr (Int x) = x
evalExpr (Plus x y) = evalExpr x + evalExpr y
evalExpr (Prod x y) = evalExpr x * evalExpr y
evalExpr (Parens e) = evalExpr e

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace  p = parseWithEof (whitespace >> p)

intE :: Parser Expr
intE = do
    n <- lexeme $ many1 digit
    return $ Int $ read n

parensE :: Parser Expr -> Parser Expr
parensE simpleExprImpl = do
    void $ lexeme $ char '('
    e <- simpleExprImpl
    void $ lexeme $ char ')'
    return $ Parens e

term :: Parser Expr -> Parser Expr
term simpleExprImpl = intE <|> parensE simpleExprImpl

term7 :: Parser Expr
term7 = term simpleExpr7

plusE :: Parser Expr
plusE = do
    e0 <- term7
    void $ lexeme $ char '+'
    e1 <- simpleExpr7
    return $ Plus e0 e1

prodE :: Parser Expr
prodE = do
    e0 <- term7
    void $ lexeme $ char '*'
    e1 <- simpleExpr7
    return $ Prod e0 e1

simpleExpr7 :: Parser Expr
simpleExpr7 = do
    -- first parse a term
    e <- term7
    -- then see if it is followed by an '+ expr' suffix
    maybeSuffix e
  where
    -- this function takes an expression, and parses a
    -- '+ expr' suffix, returning an Add expression
    -- it recursively calls itself via the maybeAddSuffix function
    plusSuffix e0 = do
        void $ lexeme $ char '+'
        e1 <- term7
        maybeSuffix (Plus e0 e1)
    prodSuffix e0 = do
        void $ lexeme $ char '*'
        e1 <- term7
        maybeSuffix (Prod e0 e1)
    -- this is the wrapper for addSuffix, which adapts it so that if
    -- addSuffix fails, it returns just the original expression
    maybeSuffix e = plusSuffix e <|> prodSuffix e <|> return e

part1 :: _ -> IO ()
part1 lines = do
  --print $ parseWithWhitespace simpleExpr7 "6 + (3 + 2)"
  print $ sum $ map (evalExpr . Either.getRight . parseWithWhitespace simpleExpr7) lines

plusTimesExpr :: Parser Expr
plusTimesExpr = E.buildExpressionParser pteTable pteTerm

pteTable :: [[E.Operator _ _ _]]
pteTable = [ [E.Infix (Plus <$ symbol "+") E.AssocLeft]
             , [E.Infix (Prod <$ symbol "*") E.AssocLeft]
           ]

pteTerm :: Parser Expr
pteTerm = pteNum <|> pteParens

pteNum :: Parser Expr
pteNum = Int <$> integer

pteParens :: Parser Expr
pteParens = Parens <$> between (symbol "(") (symbol ")") plusTimesExpr

integer :: Parser Int
integer = read <$> lexeme (many1 digit)

symbol :: String -> Parser String
symbol s = lexeme $ string s

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

part2 :: _ -> IO ()
part2 lines = do
  --print $ regularParse plusTimesExpr "1 + 2 * 3 + 4 * 5 + 6"
  --print $ evalExpr $ Either.getRight
  --  $ regularParse plusTimesExpr "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
  print $ sum $ map (evalExpr . Either.getRight . regularParse plusTimesExpr) lines

solve :: String -> IO ()
solve = go . list id where
  go lines = do
    part1 lines
    part2 lines
