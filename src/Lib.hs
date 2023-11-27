module Lib
    ( someFunc
    ) where
import System.Environment (getArgs)
import Client (runClient)
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
import Server (runServer)

initialGameState :: Gamestate
initialGameState = Gamestate initialDeck 0 [nipun,mahesh,anish,suresh] [] (team1,team2) Spades

someFunc :: IO ()
someFunc = do
    putStrLn "Starting application..."
    args  <- getArgs
    if "client" `elem` args 
        then runClient 
        else runServer 