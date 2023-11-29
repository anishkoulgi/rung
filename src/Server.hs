{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Server where
import Control.Monad (forM_, forever)
import Control.Exception (finally)
import Control.Concurrent (newMVar, MVar, readMVar, modifyMVar_)
import Data.Text (Text)
import Data.Map (Map, empty)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Utils (parseHeaders, getCodeFromHeaders, host, port)

type Client = (String, WS.Connection)
data ServerState = ServerState { clients :: [Client], games :: Map String [Client] }

newServer :: ServerState
newServer = ServerState [] empty

doesClientExist :: Client -> ServerState -> Bool
doesClientExist client (ServerState cls _) = any ((== fst client) . fst) cls

addNewClient :: Client -> ServerState -> ServerState
addNewClient client state@(ServerState { clients = cls }) = if not (doesClientExist client state)
                            then state { clients = client : cls } 
                            else state 

removeClient :: Client -> ServerState -> ServerState
removeClient client state@(ServerState { clients = cls }) = state { clients = filteredClients }
    where
        filteredClients = filter ((/= fst client) . fst) cls

broadcastMessage :: Text -> ServerState -> IO()
broadcastMessage msg (ServerState { clients = cls }) = do
    T.putStrLn msg
    forM_ cls $ \(_, conn) -> WS.sendTextData conn msg

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