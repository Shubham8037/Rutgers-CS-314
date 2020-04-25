{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import Utils
import Score
import Chances
import Control.Monad                      (void)
import qualified Data.Text                as Text
import GI.Gtk                             ( Box (..)
                                          , Orientation (..)
                                          , Window (..), Image (..))
import qualified GI.Gtk                   as Gtk
import qualified GI.Gdk                   as Gdk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Data.Char                          (toUpper)
import Data.List                          (isPrefixOf, sort)
import System.Directory                   (getDirectoryContents)
import Control.Concurrent.Async           (async)
import Widgets

view' :: State -> AppView Window Event
view' s@(Game {..}) =
  bin
    Window [ #title := "Hangman"
           , on #deleteEvent (const (True, Quit))
           , #widthRequest  := 640
           , #heightRequest := 480
           ]
    $ container Box [#orientation := OrientationHorizontal]
        [ case (showScores) of
            False -> BoxChild defaultBoxChildProperties
              { padding = 20, expand = True } $
                widget Image [#file := (hangman !! (fromChancesToHangman $ chances ))]
            True -> genScoresLabels $ format scores
        , BoxChild defaultBoxChildProperties { padding = 10
                                             , expand = True } $
            container Box [#orientation := OrientationVertical]
              [ BoxChild defaultBoxChildProperties
                  { padding = 10, expand = True } $ currentScoreLabel s
              , BoxChild defaultBoxChildProperties
                  { padding = 10, expand = True } $ bestScoreLabel s
              , BoxChild defaultBoxChildProperties
                  { padding = 10, expand = True } $ patternLabel s
              , BoxChild defaultBoxChildProperties
                  { padding = 10, expand = True } $ wrongGuessesLabel s
              , BoxChild defaultBoxChildProperties
                  { padding = 10 } $ stateBox s
              , BoxChild defaultBoxChildProperties
                  { padding = 10 } $ case (showScores) of
                     False -> seeScoresBtn
                     True  -> unseeScoresBtn
              ]
        ]

update' :: State -> Event -> Transition State Event
update' state@(Game c w s hi ch h words score _) (Guess g)
  | Text.null g = Transition state (return Nothing)
  | otherwise   = Transition (checkGuess state g') (return Nothing)
  where g' = last (Text.unpack g)

update' state@(Game c w s hi ch h words score _) (GetHint hi') =
  Transition (state { hint = [hi']
                    , chances = normalize $
                      ((pure (\x -> x - 1)) <*> ch) -- Choices applicative
                    }) (return Nothing)             -- (<*>) and pure use

update' _ Quit = Exit

update' state@(Game c w s hi ch h words score sScore) (PlayAgain s') =
  Transition (state {correct = ""
                    , wrongs = ""
                    , secret = s'
                    , hint   = ""
                    , chances = return $ (length h - 1)
                    , scores = pure (0, s') <> score
                    })
                    (return Nothing)

update' state (SeeScores) =
  Transition (state { showScores = True }) (return Nothing)

update' state (UnseeScores) =
  Transition (state { showScores = False }) (return Nothing)

game:: String -> IO ()
game dictionaryPath = do
  void $ Gtk.init Nothing

  screen <- maybe (fail "No screen :(") return =<< Gdk.screenGetDefault
  p      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    words  <-  (map . map) toUpper <$>
               lines <$>
               readFile dictionaryPath
    secret <-  getRandom words
    paths  <-  reverse . sort . filter (isPrefixOf "Hangman") <$>
                 getDirectoryContents "assets/"
    scores <- readScores
    let chances = (length paths) - 1

    void $ runLoop (App { view = view'
                   , update = update'
                   , inputs = []
                   , initialState = Game
                       ""
                       ""
                       secret
                       ""
                       (pure chances) 
                       (Text.pack . ((++) "assets/") <$> paths) words
                       ((Cons (0, secret) Empty) <> scores) 
                       False
                   })
    Gtk.mainQuit
  Gtk.main
