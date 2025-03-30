module Eva.Expr (
  Expr(..),
  Term(..),
  exprSize) where

data Expr = Frac Expr Expr
          | Mult Expr Expr
          | Expo Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Umin Expr
          | Term Term


exprSize :: Expr -> Int
exprSize (Frac a b) = exprSize a + exprSize b
exprSize (Mult a b) = exprSize a + exprSize b
exprSize (Expo a b) = exprSize a + exprSize b
exprSize (Term _) = 1


instance Show Expr where
  show (Frac a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (Mult (Term (Symbol a)) b) = show b ++ a
  show (Mult a (Term (Symbol b))) = show a ++ b
  show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Expo a b) = show a ++ "^" ++ show b
  show (Add a b) = show a ++ "+" ++ show b
  show (Sub a b) = show a ++ "-" ++ show b
  show (Umin a) = "-" ++ show a
  show (Term a) = show a


data Term = Symbol String
          | Value Integer

instance Show Term where
  show (Symbol a) = a
  show (Value a) = show a
