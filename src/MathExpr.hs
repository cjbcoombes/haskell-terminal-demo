module MathExpr (Expr (..), evalSimple, eval, exprParser) where

import Prelude hiding (lookup, null, foldr)
import Parser
import Result
import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import Data.Map ( empty, foldrWithKey, lookup, null, singleton, union, Map )
import Data.Maybe (fromMaybe)

data Expr
    = EInt Integer
    | EVar String
    | ENeg Expr
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | EExp Expr Expr
    | EWhere Expr (Map String Expr)
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
evalSimple (EWhere a _) = evalSimple a

evalWith :: Expr -> Map String Expr -> Maybe Integer
evalWith (EInt i) _ = Just i
evalWith (EVar v) m = lookup v m >>= (`evalWith` m)
evalWith (ENeg e) m = negate <$> evalWith e m
evalWith (EAdd a b) m = (+) <$> evalWith a m <*> evalWith b m
evalWith (EMul a b) m = (*) <$> evalWith a m <*> evalWith b m
evalWith (ESub a b) m = (-) <$> evalWith a m <*> evalWith b m
evalWith (EDiv a b) m = quot <$> evalWith a m <*> evalWith b m
evalWith (EExp a b) m = (^) <$> evalWith a m <*> evalWith b m
evalWith (EWhere a w) m = evalWith a (union w m)

eval :: Expr -> Maybe Integer
eval = (`evalWith` empty)

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
            write (EWhere a m) | null m = write a
                               | otherwise = write a ++ " where " ++ drop 2 (foldrWithKey (\k v a -> ", " ++ k ++ " = " ++ write v ++ a) "" m)

exprParser :: Parser Char String Expr
exprParser = expr
    where space = takeWhileP (== ' ')
          token p = space *> p <* space

          num = token $ EInt . read <$> errLabel "invalid number" <!> takeWhile1P (`elem` "0123456789")
          rawvar = errLabel "invalid varname" <!> takeWhile1P isAlpha
          var = token $ EVar <$> rawvar

          add = token $ (EAdd <$ exactP '+') <|> (ESub <$ exactP '-')
          mul = token $ (EMul <$ exactP '*') <|> (EDiv <$ exactP '/')
          exp = token (EExp <$ exactP '^')
          eq = token (exactP '=')
          com = token (exactP ',')
          where' = token $ stringP "where"

          assgn = (,) <$> rawvar <* eq <*> arith
          clause = where' *> chainr1P (uncurry singleton <$> assgn) (union <$ com)

          atom' = num <|> var <|> (exactP '(' *> expr <* exactP ')')
          atom = token $ maybe id (const ENeg) <$> maybeP (exactP '-') <*> atom'

          factor = chainr1P atom exp

          term = chainl1P factor mul

          arith = chainl1P term add

          expr = EWhere <$> arith <*> (fromMaybe empty <$> maybeP clause)