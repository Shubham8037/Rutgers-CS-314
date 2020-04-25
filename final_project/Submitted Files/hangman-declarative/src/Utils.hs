module Utils where

import System.Random                      (randomRIO)

getRandom :: [a] -> IO a
getRandom xs = fmap (xs !!) $ randomRIO (0, length xs - 1)


