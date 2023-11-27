{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Server where
import Control.Monad (forM_, forever)
import Control.Exception (finally)
import Control.Concurrent (newMVar, MVar, readMVar, modifyMVar_)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Utils (parseHeaders, getCodeFromHeaders, host, port)

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
    WS.runServer host port $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    let request = WS.pendingRequest pending
    let headers = parseHeaders (WS.requestHeaders request)
    let code    = getCodeFromHeaders headers 
    putStrLn $ "Received a connection with code: " ++ show code
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        let client = ("New Client", conn)
        modifyMVar_ state $ \s -> do
            let clientList = addNewClient client s
            return clientList
        finally (talk client state) (disconnect client)
        where
            disconnect client = do
                modifyMVar_ state $ \s -> do
                    return (removeClient client s)

talk :: Client -> MVar ServerState -> IO ()
talk (_, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcastMessage msg