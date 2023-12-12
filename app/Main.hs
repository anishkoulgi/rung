module Main (main) where

import UI.GameMode
import Objects
import Client
import Server (runServer)

main :: IO ()
main = do
    choice <- selectionMain
    case choice of
        HostMode -> runServer
        ClientMode -> clientMain