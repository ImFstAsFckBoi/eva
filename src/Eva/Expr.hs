module Eva.Expr (
  Expr(..),
  Term(..),
  exprSize,
  showTree) where

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


showTree :: Expr -> String
showTree e = go e 0
  where
    buildStr :: Int -> String -> String
    buildStr n s | n > 0 = s ++ buildStr (n-1) s
                 | n <= 0 = ""

    getIndent :: Int -> String
    getIndent n = buildStr (n-1) "┃ " ++ if n > 0 then "┗━" else ""

    goBinary :: Char -> Int -> Expr -> Expr -> String
    goBinary c i a b = getIndent i ++ c:"\n" ++ go a (i+1) ++ go b (i+1)

    goTerm :: Term -> Int -> String
    goTerm (Symbol s) i = getIndent i ++ s ++ "\n"
    goTerm (Value v) i = getIndent i ++ show v ++ "\n"

    go :: Expr -> Int -> String
    go (Frac a b) i = goBinary '/' i a b
    go (Mult a b) i = goBinary '*' i a b
    go (Expo a b) i = goBinary '^' i a b
    go (Add a b) i = goBinary '+' i a b
    go (Sub a b) i = goBinary '-' i a b
    go (Term t) i = goTerm t i
    
data Term = Symbol String
          | Value Integer

instance Show Term where
  show (Symbol a) = a
  show (Value a) = show a
