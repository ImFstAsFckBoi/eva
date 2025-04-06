{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}


module Eva.Parser (parse) where

import Eva.Expr (Expr(..), Term(..))
import Data.Char (isAlpha, isNumber)
import Control.Applicative (Alternative(..))

{-

<S> ::= <E>

<E> ::= <F> <E'>
    |   <F>

<E'>::= "+" <F> <E'>            // Addition
    |   "-" <F> <E'>            // Subtraction
    |   ϵ

<F> ::= <G> <F'>
    |   <G>

<F'>::= "*" <G> <F'>            // Multiplication
    |   "/" <G> <F'>            // Division
    |   ϵ

<G> ::= <H> "^" <G>             // Exponentiation
    |   <H>

<H> ::= "-" <T>                 // Unary minus
    | <T>

<T> ::= "(" <E> ")"             // Grouping
    |   sym                     // Symbolic constant
    |   num                     // Numeric value

-}

type Error a b = Either a b

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }


instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Right (a, rest) -> Right (f a, rest)
      Left e          -> Left e


instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)

  (Parser pf) <*> (Parser pa) = Parser $
    \input -> case pf input of
      Right (f, rest1) -> case pa rest1 of
                            Right (a, rest2) -> Right (f a, rest2)
                            Left e           -> Left e
      Left e           -> Left e


instance Monad Parser where
  (Parser pa) >>= f = Parser $ \input ->
    case pa input of
      Right (a, rest) -> runParser (f a) rest
      Left e          -> Left e

instance Alternative Parser where
  empty = Parser $ \_ -> Left "No alternative"
  (Parser a) <|> (Parser b) = Parser $ \input -> case a input of
                                                   Right r  -> Right r
                                                   Left err -> case b input of
                                                                 Right br -> Right br
                                                                 Left berr -> Left $ "No alternative matched:\n1) " ++ err ++ "\n2) " ++ berr


parseChar :: Char -> Parser ()
parseChar c = Parser $ \input -> case input of
                                   (a:as) -> if a == c then Right ((), as) else Left ""
                                   []     -> Left $ "Tried to parse '" ++ c:"' but found end of string"

parseS :: Parser Expr
parseS = parseE


parseE :: Parser Expr
parseE = parseF >>= parseE'


parseE' :: Expr -> Parser Expr
parseE' e = (parseChar '+' *> (Add e <$> parseF) >>= parseE')
            <|> (parseChar '-' *> (Sub e <$> parseF) >>= parseE')
            <|> pure e

parseF :: Parser Expr
parseF = parseG >>= parseF'


parseF' ::  Expr -> Parser Expr
parseF' e = (parseChar '*' *> (Mult e <$> parseG) >>= parseF')
            <|> (parseChar '/' *> (Frac e <$> parseG) >>= parseF')
            <|> pure e


parseG :: Parser Expr
parseG = do l <- parseH
            parseChar '^' *> (Expo l <$> parseG) <|> pure l


parseH :: Parser Expr
parseH = parseChar '-' *> (Umin <$> parseT) <|> parseT


parseT :: Parser Expr
parseT = parseChar '(' *> parseE <* parseChar ')'
         <|> parseSymbol
         <|> parseValue

parseSymbol :: Parser Expr
parseSymbol = Parser $ \input -> case input of
                                   (a:as) -> if isAlpha a
                                             then runParser (Term . Symbol <$> parseUntilF isAlpha) (a:as)
                                             else Left $ "Invalid first character '" ++ a:"' found then parsing symbol"
                                   []     -> Left "Tried to parse a symbol but found end of string"


parseValue :: Parser Expr
parseValue = Parser $ \input -> case input of
                                   (a:as) -> if isNumber a
                                             then runParser (Term . Value . read <$> parseUntilF isNumber) (a:as)
                                             else Left $ "Invalid first character '" ++ a:"' found then parsing value"
                                   []     -> Left "Tried to parse a value but found end of string"


parseUntilF :: (Char -> Bool) -> Parser String
parseUntilF f = Parser $ \input -> case input of
                                     (a:as) -> if f a
                                               then runParser ((a:) <$> parseUntilF f) as
                                               else Right ("", a:as)
                                     [] -> Right ("", [])


parse :: String -> Either String Expr
parse as =  do (e, _) <- runParser parseS as
               return e
