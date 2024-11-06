module MathExpr (Expr (..), evalSimple, eval) where

import Prelude hiding (lookup)
import Data.Map

data Expr
    = EInt Integer
    | EVar String
    | ENeg Expr
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EExp Expr Expr
    deriving Eq

evalSimple :: Expr -> Maybe Integer
evalSimple (EInt i) = Just i
evalSimple (EVar _) = Nothing
evalSimple (ENeg e) = negate <$> evalSimple e
evalSimple (EAdd a b) = (+) <$> evalSimple a <*> evalSimple b
evalSimple (EMul a b) = (*) <$> evalSimple a <*> evalSimple b
evalSimple (ESub a b) = (-) <$> evalSimple a <*> evalSimple b
evalSimple (EDiv a b) = quot <$> evalSimple a <*> evalSimple b
evalSimple (EExp a b) = (^) <$> evalSimple a <*> evalSimple b

eval :: Expr -> Map String Expr -> Maybe Integer
eval (EInt i) _ = Just i
eval (EVar v) m = lookup v m >>= (`eval` m)
eval (ENeg e) m = negate <$> eval e m
eval (EAdd a b) m = (+) <$> eval a m <*> eval b m
eval (EMul a b) m = (*) <$> eval a m <*> eval b m
eval (ESub a b) m = (-) <$> eval a m <*> eval b m
eval (EDiv a b) m = quot <$> eval a m <*> eval b m
eval (EExp a b) m = (^) <$> eval a m <*> eval b m

-- eval w/ map

instance Show Expr where
    show = write
        where
            write (EInt i) = show i
            write (EVar v) = v
            write (ENeg e) = '-':write e
            write (EAdd a b) = '(' : write a ++ " + " ++ write b ++ ")"
            write (EMul a b) = '(' : write a ++ " * " ++ write b ++ ")"
            write (ESub a b) = '(' : write a ++ " - " ++ write b ++ ")"
            write (EDiv a b) = '(' : write a ++ " / " ++ write b ++ ")"
            write (EExp a b) = '(' : write a ++ " ^ " ++ write b ++ ")"