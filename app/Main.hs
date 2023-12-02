module Main (main) where

import Game (startGame)

main :: IO ()
main = do
    _ <- startGame
    putStrLn ""