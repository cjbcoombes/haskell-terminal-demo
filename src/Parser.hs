{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Parser (module Parser) where

import Result

data ParseError i e
    = Unexpected i
    | Eof
    | Custom e
    | Joined [ParseError i e]
    deriving Show

newtype Parser i e a = Parser { runParser :: [i] -> Result (ParseError i e) (a, [i]) }

instance Functor (Parser i e) where
    fmap f p = Parser (fmap (mf f) . runParser p)
        where mf f (a, b) = (f a, b)

instance Applicative (Parser i e) where
  pure a = Parser $ Accept . (a,)
  a <*> b = Parser $ \input -> do
    (f, rest) <- runParser a input
    (r, rest') <- runParser b rest
    return (f r, rest')

instance Monad (Parser i e) where
    return = pure
    a >>= b = Parser $ \input -> do
        (r, rest) <- runParser a input
        runParser (b r) rest


predP :: (i -> Bool) -> Parser i e i
predP p = Parser $ \case
    [] -> Reject Eof
    x:xs -> if p x then Accept (x, xs) else Reject (Unexpected x)

exactP :: Eq i => i -> Parser i e i
exactP = predP . (==)

splitWhile :: (i -> Bool) -> [i] -> ([i], [i])
splitWhile p [] = ([], [])
splitWhile p (x:xs) = let (as, bs) = splitWhile p xs in (x:as, bs)

takeWhileP :: (i -> Bool) -> Parser i e [i]
takeWhileP p = Parser $ Accept . splitWhile p

orP :: Parser i e a -> Parser i e a -> Parser i e a
orP a b = Parser $ \input ->
    case runParser a input of
        Accept x -> Accept x
        Reject ea -> case runParser b input of
                    Accept x -> Accept x
                    Reject eb -> Reject (Joined [ea, eb])