module Hutton where

data Expr = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i)   = i
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit i)   = show i
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
