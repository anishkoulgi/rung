module Main (main) where

import UI.GameMode (selectionMain)
import UI.Rules (rulesMain)
import Objects
import Client (clientMain)
import Server (runServer)

main :: IO ()
main = do
    choice <- selectionMain
    case choice of
        HostMode -> runServer
        ClientMode -> clientMain
        RuleMode -> rulesMain >> main