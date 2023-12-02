module Client (clientMain) where

import UI.Client.InitialClientPage
import UI.Client.ClientPage
import Objects
import Lens.Micro
import Text.Printf


clientMain :: IO()
clientMain = do
    clientInfo <- getClientData
    printf  "Name: %s\tIP: %s" (clientInfo^.name)  (clientInfo^.ip)
    _ <- showClientUI
    print()