{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Parser (module Parser) where

import Result ( mapReject, Result(..) )
import Control.Applicative (Alternative (..))

data ParseError i e
    = Unexpected i
    | Eof
    | Custom e
    | Branch [ParseError i e]
    | Joined [ParseError i e]
    deriving Show

newtype Parser i e a = Parser { runParser :: [i] -> Result (ParseError i e) (a, [i]) }

infixr 5 <!>

(<!>) :: (ParseError i e -> ParseError i e) -> Parser i e a -> Parser i e a
f <!> p = Parser $ mapReject f . runParser p

errLabel :: e -> ParseError i e -> ParseError i e
errLabel x = Joined . (:[Custom x])

instance Functor (Parser i e) where
    fmap f p = Parser (fmap (mf f) . runParser p)
        where mf f (a, b) = (f a, b)

instance Applicative (Parser i e) where
  pure a = Parser $ Accept . (a,)
  a <*> b = Parser $ \input -> do
    (f, rest) <- runParser a input
    -- runParser (f <$> b) rest
    (r, rest') <- runParser b rest
    return (f r, rest')

instance Monad (Parser i e) where
    return = pure
    a >>= b = Parser $ \input -> do
        (r, rest) <- runParser a input
        runParser (b r) rest

orP :: Parser i e a -> Parser i e a -> Parser i e a
orP a b = Parser $ \input ->
    case runParser a input of
        Accept x -> Accept x
        Reject ea -> case runParser b input of
            Accept x -> Accept x
            Reject eb -> Reject (Branch [ea, eb])

instance Alternative (Parser i e) where
    empty = Parser $ const (Reject Eof)
    (<|>) = orP

eofP :: Parser i e ()
eofP = Parser $ \case
    [] -> Accept ((), [])
    (x:xs) -> Reject $ Unexpected x

predP :: (i -> Bool) -> Parser i e i
predP p = Parser $ \case
    [] -> Reject Eof
    x:xs -> if p x then Accept (x, xs) else Reject (Unexpected x)

exactP :: Eq i => i -> Parser i e i
exactP = predP . (==)

stringP :: Eq i => [i] -> Parser i e [i]
stringP = traverse exactP -- How??
-- stringP [] = return []
-- stringP (x:xs) = (:) <$> exactP x <*> stringP xs
-- stringP = foldr ((<*>) . (((:) <$>) . exactP)) (return [])

splitWhile :: (i -> Bool) -> [i] -> ([i], [i])
splitWhile p [] = ([], [])
splitWhile p (x:xs) = if p x then (x:as, bs) else ([], x:xs)
    where (as, bs) = splitWhile p xs

takeWhileP :: (i -> Bool) -> Parser i e [i]
takeWhileP p = Parser $ Accept . splitWhile p

takeWhile1P :: (i -> Bool) -> Parser i e [i]
takeWhile1P p = Parser $ (\case
        ([], []) -> Reject Eof
        ([], x:xs) -> Reject (Unexpected x)
        t -> Accept t
    ) . splitWhile p

maybeP :: Parser i e a -> Parser i e (Maybe a)
maybeP p = Parser $ \input ->
    case runParser p input of
        Accept (r, rest) -> Accept (Just r, rest)
        Reject e -> Accept (Nothing, input)

someP :: Parser i e a -> Parser i e [a]
someP p = Parser $ \input ->
    case runParser p input of
        Accept (r, rest) -> runParser ((r:) <$> someP p) rest
        Reject e -> Accept ([], input)

some1P :: Parser i e a -> Parser i e [a]
-- some1P p = Parser $ \input -> 
--     case runParser p input of
--         Accept (r, rest) -> runParser ((r:) <$> someP p) rest
--         Reject e -> Reject e
-- some1P p = p >>= (\r -> (r:) <$> someP p)
some1P p = (:) <$> p <*> someP p

chainl1P :: Parser i e a -> Parser i e (a -> a -> a) -> Parser i e a
chainl1P p op = p >>= rest
    where
        -- rest x = (do
        --     f <- op
        --     y <- p
        --     rest (f x y)) <|> return x
        rest x = ((op <*> pure x <*> p) >>= rest) <|> return x

chainr1P :: Parser i e a -> Parser i e (a -> a -> a) -> Parser i e a
-- chainr1P p op = (do
--     x <- p
--     f <- op
--     y <- chainr1P p op
--     return (f x y)) <|> p
chainr1P p op = (flip ($) <$> p <*> op <*> chainr1P p op) <|> p

-- seqP :: Parser i e a -> Parser i e a -> Parser i e [a]
-- seqP a b = (\x y -> [x,y]) <$> a <*> b