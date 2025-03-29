

data ExprState = Reducible Expr | Irreducible Expr
data Op = Divide | Multiply


unwrap :: ExprState -> Expr
unwrap (Reducible a) = a
unwrap (Irreducible a) = a


canReduce :: ExprState -> Bool
canReduce (Irreducible _) = False
canReduce (Reducible _ ) = True


data Expr = Frac Expr Expr
          | Mult Expr Expr
          | Expo Expr Expr
          | Term Term


exprSize :: Expr -> Int
exprSize (Frac a b) = exprSize a + exprSize b
exprSize (Mult a b) = exprSize a + exprSize b
exprSize (Expo a b) = exprSize a + exprSize b
exprSize (Term _) = 1

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

instance Show Expr where
  show (Frac a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (Mult (Term (Symbol a)) b) = show b ++ a
  show (Mult a (Term (Symbol b))) = show a ++ b
  show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Expo a b) = show a ++ "^" ++ show b
  show (Term a) = show a


data Term = Symbol String
          | Value Integer

instance Show Term where
  show (Symbol a) = a
  show (Value a) = show a


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
evalTerm Divide (Value a) (Value b) =  Term (Value (a `div` b))
evalTerm Multiply (Value a) (Value b) =  Term (Value (a * b))
evalTerm Multiply (Symbol a) (Symbol b) | a == b = Expo (Term (Symbol a)) (Term (Value 2))
                                     | otherwise = Mult (Term (Symbol a)) (Term (Symbol b))

evalTerm Divide a b = Frac (Term a) (Term b)
evalTerm Multiply a b = Mult (Term a) (Term b)


-- Reduction rules for multiplications
evalMult :: Expr -> Expr -> ExprState
evalMult (Term a) (Term b) = Irreducible (evalTerm Multiply a b)
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
evalFrac (Term a) (Term b) = Irreducible (evalTerm Divide a b)
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
main = do
  print (Frac (Mult (Term (Value 2)) (Term (Symbol "a"))) (Term (Value 2)))
  print (exprSize (unwrap (evalFrac (Mult (Term (Value 2)) (Term (Symbol "a"))) (Term (Value 2)))))
  --print (exprSize(evaluate (Frac (Frac (Term (Symbol "a")) (Term (Value 2))) (Frac (Term (Symbol "b")) (Term (Value 2))))))

