module MathExpr (Expr) where

data Expr 
    = EInt Integer
    | ENeg Expr
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EExp Expr Expr