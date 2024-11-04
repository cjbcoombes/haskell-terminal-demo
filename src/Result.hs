module Result (Result (Accept, Reject), mapReject) where

data Result e a
    = Accept a
    | Reject e
    deriving Show

mapReject :: (e -> ee) -> Result e a -> Result ee a
mapReject f (Reject e) = Reject $ f e
mapReject _ (Accept a) = Accept a

instance Functor (Result e) where
    fmap f (Reject e) = Reject e
    fmap f (Accept a) = Accept (f a)

instance Applicative (Result e) where
    pure = Accept
    (Reject e) <*> _ = Reject e
    (Accept f) <*> r = fmap f r

instance Monad (Result e) where
    return = pure
    (Reject e) >>= _ = Reject e
    (Accept a) >>= f = f a

-- instance Functor (Result e) where
--     fmap :: (a -> b) -> Result e a -> Result e b
--     fmap f r = case r of
--         Reject e -> Reject e
--         Accept a -> Accept (f a)

-- instance Applicative (Result e) where
--     pure :: a -> Result e a
--     pure = Accept
--     (<*>) :: Result e (a -> b) -> Result e a -> Result e b
--     (<*>) r = case r of
--         Reject e -> const (Reject e)
--         Accept f -> fmap f

-- instance Monad (Result e) where
--     return = pure
--     (>>=) r = case r of
--         Reject e -> const (Reject e)
--         Accept a -> ($ a)
