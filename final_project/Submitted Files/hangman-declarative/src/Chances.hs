module Chances where

data Chances a = None | Remaining a
               deriving (Eq)

instance Functor Chances where
    fmap _ None = None
    fmap f (Remaining a) = Remaining (f a)

instance Applicative Chances where
    pure = Remaining
    (Remaining f) <*> (Remaining x) = Remaining (f x)
    _ <*> _ = None

instance Monad Chances where
    return = pure
    (>>=) m g = case m of
      None        -> None
      Remaining x -> g x

normalize' :: (Ord a, Num a) => a -> Chances a
normalize' a | a > 0     = Remaining a
             | otherwise = None

-- monadic bind use
normalize :: (Ord a, Num a) => Chances a -> Chances a
normalize c = c >>= normalize'

fromChancesToHangman :: Chances Int -> Int
formChancesToHangman None = 0
fromChancesToHangman (Remaining a) = a
fromChancesToHangman _ = 0

