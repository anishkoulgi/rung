{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Server where
import Data.Text (Text)
import Control.Monad (forM_, forever)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent (newMVar, MVar, readMVar, modifyMVar_)
import Control.Exception (finally)
import Client ( app )
import Network.Socket ( withSocketsDo )

type Client = (String, WS.Connection)
type ServerState = [Client]

newServer :: ServerState
newServer = []

doesClientExist :: Client -> ServerState -> Bool
doesClientExist client = any ((== fst client) . fst)

addNewClient :: Client -> ServerState -> ServerState
addNewClient client state = if not (doesClientExist client state)
                            then client : state
                            else state

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcastMessage :: Text -> ServerState -> IO()
broadcastMessage msg state = do
    T.putStrLn msg
    forM_ state $ \(_, conn) -> WS.sendTextData conn msg

runServer :: IO ()
runServer = do
    state <- newMVar newServer
    putStrLn "Hello world"
    inp <- getLine
    if inp == "client" then (withSocketsDo $ WS.runClient "localhost" 3333 "/" app) else (WS.runServer "localhost" 3333 $ application state)
    

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    putStrLn "Listening for clients..."
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        msg <- WS.receiveData conn
        let client = ("New Client", conn)
        modifyMVar_ state $ \s -> do
         let clientList = addNewClient client s
         return clientList
        cli <- readMVar state
        print (map fst cli)
        readMVar state >>= broadcastMessage msg
        finally (talk client state) (disconnect client)
        where
            disconnect client = do
                modifyMVar_ state $ \s -> do
                    return (removeClient client s)

talk :: Client -> MVar ServerState -> IO ()
talk (_, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcastMessage msg