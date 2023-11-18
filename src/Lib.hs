module Lib
    ( someFunc
    ) where

import Game
    ( Gamestate(Gamestate),
      Suit(Spades),
      shuffle,
      initialDeck,
      nipun,
      anish,
      mahesh,
      suresh,
      team1,
      team2,
      runGame )

initialGameState :: Gamestate
initialGameState = Gamestate initialDeck 0 [nipun,mahesh,anish,suresh] [] (team1,team2) Spades

someFunc :: IO ()
someFunc = do {
    deck <- shuffle initialDeck;
    runGame (Gamestate deck 0 [nipun,mahesh,anish,suresh] [] (team1,team2) Spades)
}