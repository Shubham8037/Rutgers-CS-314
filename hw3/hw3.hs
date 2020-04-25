import Data.List


--(1)
-- this is the function that will help in 'following function'
helper :: (Eq t, Num t) => t -> t -> [a] -> [a]
helper _ _ [] = []
helper p i (x:xs)
  | p == i = x : helper 1 i xs
  | otherwise = helper (p + 1) i xs

-- this function finds every nth term of a list, helps in implementation of skips
following :: (Eq t, Num t) => t -> [a] -> [a]
following 0 _ = []
following 1 xs = xs
following n xs = helper 1 n xs

--returns a list of list that has every nth element skipped 
skips :: [a] -> [[a]]
skips xs = map (\x -> following x xs) [1..len]
  where len = length xs




--(2)
localMaxima :: Ord a => [a] -> [a]
localMaxima (left:mid:right:rem) = if (mid > left && mid > right) then mid : localMaxima (mid:right:rem) -- checks for each element if its great then prev and next
                         else localMaxima (mid:right:rem)  -- recursively applies function to remaining list
localMaxima _  = []



--(3)
-- helper method for histogram
aestericOrEmpty :: [Integer] -> String
aestericOrEmpty xs = map pointer [0..9]
  where pointer x | x `elem` xs = '*'
               | otherwise = ' '

--helper method for histogram
createhistogram :: [Integer] -> [String]
createhistogram [] = []
createhistogram xs = aestericOrEmpty xs : createhistogram xs'
  where xs' = xs \\ [0..9]  
   
histogram :: [Integer] -> String
histogram xs = (unlines.reverse) (createhistogram xs ) ++ "==========\n0123456789\n"


