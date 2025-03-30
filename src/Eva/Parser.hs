module Eva.Parser where

import Eva.Expr (Expr(..), Term(..))
import Data.Char (isAlpha, isNumber)


{-

<S> ::= <E> $

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



s :: String -> (Expr, String)
s = e


e :: String -> (Expr, String)
e as = e' s l where (l, s) = f as


e' :: String -> Expr -> (Expr, String)
e' (a:as) p | a == '+' = (Add p r, s)
            | a == '-' = (Sub p r, s)
            | otherwise = (p, a:as)
            where
              (r, s) = f as

f :: String -> (Expr, String)
f as = f' s l where (l, s) = g as


f' :: String -> Expr -> (Expr, String)
f' (a:as) p | a == '*' = (Mult p r, s)
            | a == '/' = (Frac p r, s)
            | otherwise = (p, a:as)
            where
              (r, s) = g as

g :: String -> (Expr, String)
g as | b == '^' = (Expo l r, s)
     | otherwise = (l, b:bs)
     where
       (r, s) = g bs
       (l, b:bs) = h as
    
h :: String -> (Expr, String)
h (a:as) | a == '-' = (Umin e, s)
         | otherwise = t (a:as)
         where
           (e, s) = t as

t :: String -> (Expr, String)
t (a:as) | a == '(' = (c, bs)
         | isAlpha a = (Term (Symbol sym), ss)
         | isNumber a = (Term (Value (read num)), ns)
         where
           (sym, ss) = parseUntilF isAlpha (a:as)
           (num, ns) = parseUntilF isNumber (a:as)
           (c, b:bs) = e as


parseUntilF :: (Char -> Bool) -> String -> (String, String)
parseUntilF _ [] = ("", "")
parseUntilF f (a:as) | f a = (a:n, r)
                     | otherwise = ("", a:as)
                     where
                       (n, r) = parseUntilF f as


parse :: String -> Expr
parse as | r == "$" = expr
         | otherwise = undefined
         where
           (expr, r) = e (as ++ "$")

