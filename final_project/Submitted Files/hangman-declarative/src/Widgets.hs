{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Widgets where

import Utils
import Score
import Chances
import Control.Monad                      (void)
import Data.Text                          (Text)
import Data.List                          (intersperse, intercalate)
import qualified Data.Text                as Text
import GI.Gtk                             ( entryGetText, entrySetText
                                          , Box (..), Button (..)
                                          , Label (..), Orientation (..)
                                          , Entry (..) )
import qualified Data.Set                 as Set
import GI.Gtk.Declarative
import Data.Char                          (toUpper)
import Data.List                          ((\\))
import Data.ByteString                    (ByteString)
import Control.Concurrent.Async           (async)
import qualified Data.Vector              as Vector
import Data.List.Split                    (splitOn)
import Data.Functor                       ((<&>))


data State = Game { correct      :: [Char]
                  , wrongs       :: [Char]
                  , secret       :: [Char]
                  , hint         :: [Char]
                  , chances      :: Chances Int         -- Custom ADT
                  , hangman      :: [Text]
                  , dictionary   :: [[Char]]
                  , scores       :: Score (Int, String) -- Custom ADT
                  , showScores   :: Bool
                  }

data Event = Guess Text
           | GetHint Char
           | PlayAgain [Char]
           | SeeScores
           | UnseeScores
           | Quit

stateBox :: State -> Widget Event
stateBox s@(Game {..})
  | didWin            = winBox s
  | (chances /= None) = gameBox s
  | otherwise         = endBox s
  where
    didWin = Set.null $ Set.difference sec guessed
    sec = Set.fromList secret
    guessed = Set.fromList correct

gameBox :: State -> Widget Event
gameBox s = container Box [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties { padding = 10 } guessBox
  , BoxChild defaultBoxChildProperties { padding = 10 } $ hintLabel s
  , BoxChild defaultBoxChildProperties { padding =  5 } $ hintBtn s
  ]

endBox :: State -> Widget Event
endBox s = container Box [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties { padding = 10 } $ failLabel s
  , BoxChild defaultBoxChildProperties { padding =  5 } $ playAgainBtn s
  , BoxChild defaultBoxChildProperties { padding =  5 } $ quitBtn s
  ]

winBox :: State -> Widget Event
winBox s = container Box [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties { padding = 10 } $ winLabel s
  , BoxChild defaultBoxChildProperties { padding =  5 } $ playAgainBtn s
  , BoxChild defaultBoxChildProperties { padding =  5 } $ quitBtn s
  ]

guessBox :: Widget Event
guessBox = container Box [#orientation := OrientationHorizontal]
  [ BoxChild defaultBoxChildProperties { expand = True } choseLabel
  , BoxChild defaultBoxChildProperties { expand = True } guessEntry]

playAgainBtn :: State -> Widget Event
playAgainBtn s@(Game {..}) = widget Button
  [#label := "Play Again!", onM #clicked playAgain']
  where
    playAgain' b = do
      writeScores s
      PlayAgain <$> getRandom dictionary

seeScoresBtn :: Widget Event
seeScoresBtn = widget Button
  [#label := "See last scores", on #clicked SeeScores]

unseeScoresBtn :: Widget Event
unseeScoresBtn = widget Button
  [#label := "Back", on #clicked UnseeScores]

quitBtn :: State -> Widget Event
quitBtn s = widget Button [#label := "Save and quit", onM #clicked quit']
  where
    quit' b = do
      writeScores s
      return Quit

hintBtn :: State -> Widget Event
hintBtn Game {..} = widget Button
  [#label := "Get a hint!", onM #clicked hintEvent]
  where hintEvent = \b -> GetHint <$> getRandom (secret \\ correct)

hintLabel :: State -> Widget Event
hintLabel Game {..} = case (Prelude.null hint) of
  True -> widget Label [#label := ""]
  False -> widget Label
    [ classes ["blue"]
    , #label := ("Consider choosing: " <> Text.pack hint)]

wrongGuessesLabel :: State -> Widget Event
wrongGuessesLabel Game {..} = widget Label
  [#label := ("Wrong guesses: " <> (Text.pack wrongs'))]
  where wrongs' = intercalate ", " ((\c -> [c]) <$> wrongs)

choseLabel:: Widget Event
choseLabel = widget Label [#label := "Choose a letter: "]

guessEntry :: Widget Event
guessEntry = widget Entry [#text := "", onM #activate toGuessEvent]
  where toGuessEvent w = do
          input <- Guess <$> entryGetText w
          entrySetText w ""
          return input

patternLabel :: State -> Widget Event
patternLabel s = widget Label [ Classes ["pattern"]
                              , #label := ("Pattern: " <>
                                          (Text.pack $ getPattern s))]

genScoresLabels :: Show a => Score a -> BoxChild Event
genScoresLabels s = container Box [#orientation := OrientationVertical] $
  Vector.cons makeTitle $
  (Vector.fromList $ toList $ (makeLabels <$> (getTail s))) <&> makeChild
  where
    makeTitle = widget Label [classes ["title"], #label := "Last games:"]
    makeLabels = \l -> widget Label [#label := (Text.pack . show $ l)]
    makeChild x = BoxChild defaultBoxChildProperties
      {padding = 5, expand = False} $ x

currentScoreLabel :: State -> Widget Event
currentScoreLabel Game {..} = widget Label
    [ Classes ["score"]
    , #label := ("Score: " <> (Text.pack . show . fst . getHead $ scores))
    ]

bestScoreLabel :: State -> Widget Event
bestScoreLabel Game {..} = widget Label
    [ Classes ["score"]
    , #label := ("Best: " <> (Text.pack . show . fst . maximum $ scores))
    ]

failLabel :: State -> Widget Event
failLabel Game {..} = widget Label
  [ classes ["red"]
  , #label := ("You have run out of guessess,\n the word was: " <>
              (Text.pack secret))]

winLabel :: State -> Widget Event
winLabel Game {..} = widget Label
  [ classes ["green"]
  , #label := "Horay, you won!"]

getPattern :: State -> String
getPattern Game {..} = intersperse ' ' $ f <$> secret
  where f x | (toUpper x) `elem` correct = toUpper x
--          | (toUpper x) `elem` hints = toUpper x -- hint button adds
            | otherwise = '_'                      -- answer

checkGuess :: State -> Char -> State
checkGuess state@(Game c w s hi ch h words scores _) g
  | g' `elem` c = state { hint = ""            -- already guessed
                        , chances = normalize $
                          ((pure (\x -> x - 1)) <*> ch)            -- Chances applicative usage
                        , scores = mapHead penalty scores }
  | g' `elem` w = state { hint = ""            -- already wrong
                        , chances = normalize $
                          ((pure (\x -> x - 1)) <*> ch)            -- Chances applicative usage 
                        , scores = mapHead penalty scores }
  | g' `elem` s = state { correct = (c++ [g']) -- correct
                        , hint = ""
                        , scores = mapHead (\(x, y) -> (x + 1, y)) scores }
  | otherwise   = state { wrongs = (w ++ [g']) -- wrong
                        , hint = ""
                        , chances = normalize $
                          ((pure (\x -> x - 1)) <*> ch)            -- Chances applicative usage
                        , scores = mapHead mistake scores }
  where g' = toUpper g
        penalty (x, y) | x >= 2 = (x - 2, y) 
                       | otherwise = (0, y)
        mistake (x, y) | x >= 1 = (x - 1, y)
                       | otherwise = (0, y)


styles :: ByteString
styles = mconcat
  [ "button   {border: 2px solid gray; font-weight: 800;}"
  , ".title   {font-size: 1.5em;}"
  , ".pattern {font-size: 2em;}"
  , ".score   {background-color: white; font-size:  2em; color: #0e1f3e;}"
  , ".red     {font-style: italic; font-size: 1.5em; color: red;}"
  , ".blue    {font-style: italic; font-size: 1.5em; color: blue;}"
  , ".green   {font-style: italic; font-size: 1.5em; color: green;}"
  ]

readScores :: IO (Score (Int, String))
readScores = do
  l <- (fmap (splitOn ",")) <$> words <$> readFile "assets/highscores"
  let scores = fromList $ take 10 $ (\[x, y] -> (read x :: Int, y)) <$> l
  return scores

writeScores :: State -> IO ()
writeScores Game{..} = void . async $
  writeFile "assets/highscores" $ show' scores
  where
    show' Empty = ""
    show' (Cons (x, y) cs) = show x ++ "," ++ y ++ " " ++ show' cs

