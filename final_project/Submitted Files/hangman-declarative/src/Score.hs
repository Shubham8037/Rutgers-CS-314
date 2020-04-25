module Score where

-- | Implementation of Monoid, Functor, Applicative Functor
-- and Monad classes for an artificial
-- list datatype
data Score a = Empty
             | Cons a (Score a)

instance Semigroup (Score a) where
  Empty <> x = x
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (Score a) where
  mempty = Empty
  mappend = (<>)

instance Functor Score where
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative Score where
  pure x = Cons x Empty
  Empty <*> _ = Empty
  (Cons f fs) <*> xs = fmap f xs <> (fs <*> xs)

instance Foldable Score where
  foldr f z Empty = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Monad Score where
  return x = pure x
  Empty >>= _ = Empty
  (Cons x xs) >>= f = foldr mappend (f x) (fmap f xs)

instance Show a => Show (Score a) where
  show Empty = ""
  show (Cons x Empty) = show x
  show (Cons x y) = (show x) ++ "\n" ++ (show y)

{-- Utils --}
fromList :: [a] -> Score a
fromList = foldr Cons Empty

toList :: Score a -> [a]
toList = foldr (:) []

mapHead :: (a -> a) -> Score a -> Score a
mapHead f (Cons x xs) = Cons (f x) xs

getHead :: Score a -> a
getHead (Cons a _) = a

getTail :: Score a -> Score a
getTail Empty = Empty
getTail (Cons _ b) = b

format' :: (Int, String, Int) -> String
format' = \(x, y, z) -> show x ++ " pts, phrase: " ++ y ++
  ", total: " ++ show z ++ "pts"

-- Functorial <$> usage
format :: Score (Int, String) -> Score String
format score = format' <$> total score

-- Monadic bind and return
fstScore :: Num a => Score (a, b) -> Score a
fstScore s = s >>= return . (\(x, y) -> x)

total :: Score (Int, String) -> Score (Int, String, Int)
total Empty = Empty
total (Cons (x, y) Empty) = pure (x, y, x)
total k@(Cons (x, y) xs) = Cons (x, y, sum $ fstScore k) (total xs)
