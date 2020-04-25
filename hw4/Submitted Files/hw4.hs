import System.Random
import System.IO

data GameState = State
               { secret  :: String
               , guessed :: String
               }

instance Show GameState where
    show gs = replace s g
      where s = secret gs
            g = guessed gs

-- | Replaces unguessed characters with '_'
replace :: String -> String -> String
replace s g = (\x -> if (x `elem` g) then x else '_') <$> s

-- | Player can win or keep guessing
data Status = Win
            | Guessing
            deriving Eq

-- | Check game current status
status :: GameState -> Status
status gs = let s = secret gs in
  if s == replace s (guessed gs)
  then Win
  else Guessing

-- | Game loop
loop :: GameState -> IO ()
loop a@(State s g) = do
--  putStr "\ESC[2J"              -- clears terminal
  putStrLn $ show a               -- print current game state
  hSetBuffering stdin NoBuffering
  c <- hGetChar stdin             -- get user input  
  putStrLn ""
  let g' = if c `elem` g          -- check if char has been already
           then g                 -- guessed
           else (c:g)
      s' = (State s g')           -- update state
  case status s' of               -- check game status
    Win      -> do putStrLn s
                   return ()      -- exit
    Guessing -> loop s'           -- ask for input again

main :: IO ()
main = do
  words <- lines <$> readFile "words.txt" -- read dictionary
  i <- randomRIO (0, length words - 1)        -- choose random word
  let gs = State (words !! i) []          -- create initial state
  loop gs                                 -- run game loop
