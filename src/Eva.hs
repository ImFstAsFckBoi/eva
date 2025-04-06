module Eva  where

import Eva.Expr (Expr(..), Term(..), exprSize, showTree)
import Eva.Parser (parse)


data ExprState = Reducible Expr | Irreducible Expr
data Op = DIV | MULT | ADD | SUB | EXPO


unwrap :: ExprState -> Expr
unwrap (Reducible a) = a
unwrap (Irreducible a) = a


canReduce :: ExprState -> Bool
canReduce (Irreducible _) = False
canReduce (Reducible _ ) = True


minf :: Ord b => (a -> b) -> [a] -> Maybe a
minf f [] = Nothing
minf f (a:as) = go f a as
  where
    go f c [] = Just c
    go f c (a:as) | f c > f a = go f a as
                  | otherwise = go f c as

maxf :: Ord b => (a -> b) -> [a] -> Maybe a
maxf f [] = Nothing
maxf f (a:as) = go f a as
  where
    go f c [] = Just c
    go f c (a:as) | f c < f a = go f a as
                  | otherwise = go f c as

evaluate :: Expr -> Expr
evaluate e = unwrap (eval (Reducible e))

eval :: ExprState -> ExprState
eval (Reducible e) = evalExpr (Reducible e)
eval (Irreducible e) = Irreducible e

evalExpr :: ExprState -> ExprState
evalExpr (Reducible (Frac a b)) = evalFrac a b
evalExpr (Reducible (Mult a b)) = evalMult a b
evalExpr (Reducible (Term a)) = Irreducible (Term a)
evalExpr (Irreducible (Term a)) = Irreducible (Term a)


evalTerm :: Op -> Term -> Term -> Expr
evalTerm ADD (Value a) (Value b) = Term(Value (a + b))
evalTerm SUB (Value a) (Value b) = Term(Value (a - b))
evalTerm EXPO (Value a) (Value b) = Term(Value (a ^ b))
evalTerm DIV (Value a) (Value b) =  Term (Value (a `div` b))
evalTerm MULT (Value a) (Value b) =  Term (Value (a * b))
evalTerm MULT (Symbol a) (Symbol b) | a == b = Expo (Term (Symbol a)) (Term (Value 2))
                                    | otherwise = Mult (Term (Symbol a)) (Term (Symbol b))
evalTerm DIV a b = Frac (Term a) (Term b)
evalTerm MULT a b = Mult (Term a) (Term b)

-- Reduction rules for multiplications
evalMult :: Expr -> Expr -> ExprState
evalMult (Term a) (Term b) = Irreducible (evalTerm MULT a b)
evalMult (Frac a b) (Frac c d) = Reducible (Frac (unwrap (evalExpr (Reducible (Mult a c)))) (unwrap (evalExpr (Reducible (Mult b d)))))
evalMult a b = if
  canReduce ea || canReduce eb
  then
    evalExpr (Reducible (Mult (unwrap ea) (unwrap eb))) else Irreducible (Mult (unwrap ea) (unwrap eb))
  where
    ea = evalExpr (Reducible a)
    eb = evalExpr (Reducible b)


-- Reduction rules for fractions
evalFrac :: Expr -> Expr -> ExprState
evalFrac (Term a) (Term b) = Irreducible (evalTerm DIV a b)
evalFrac (Frac a b) (Frac c d) = Reducible (Frac (unwrap (evalExpr (Reducible (Mult a d)))) (unwrap (evalExpr (Reducible (Mult b c)))))
evalFrac (Mult a b) (Term c) = evalExpr (Reducible (Mult a (unwrap (evalExpr (Reducible (Frac b (Term c)))))))
evalFrac a b = if
  canReduce ea || canReduce eb
  then
    evalFrac (unwrap ea) (unwrap eb)
  else
    Irreducible (Frac (unwrap ea) (unwrap eb))
  where
    ea = evalExpr (Reducible a)
    eb = evalExpr (Reducible b)


main :: IO()
main = do putStr "> "
          e <- parse <$> getLine
          putStrLn $ case e of
                       Left error -> error
                       Right expr -> (showTree . evaluate) expr

