{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Server where
import Control.Monad (forM_, forever)
import Control.Exception (finally)
import Control.Concurrent (newMVar, MVar, readMVar, modifyMVar_)
import Data.Text (Text)
import Data.ByteString.Char8 as BLU hiding (length, null, filter, putStrLn, any, empty)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Utils (parseHeaders, getNameFromHeaders)
import Constants (host, port)

type Client = (String, WS.Connection)
data ServerState = ServerState { clients :: [Client] }

newServer :: ServerState
newServer = ServerState [] 

runServer :: IO ()
runServer = do
    state <- newMVar newServer
    WS.runServer host port $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    let request = WS.pendingRequest pending
    let headers = parseHeaders (WS.requestHeaders request)
    let name    = getNameFromHeaders headers
    if  not $ null name    -- Check that name is non-empty 
        then do
            acceptConnection state pending name
        else WS.rejectRequest pending (BLU.pack "No name specified")

acceptConnection :: MVar ServerState -> WS.PendingConnection -> String -> IO ()
acceptConnection state pending name = do
    conn <- WS.acceptRequest pending
    let client = (name, conn) 
    addClientToState client state
    putStrLn $ "Client " ++ name ++ " connected"
    WS.withPingThread conn 30 (return ()) $ do
        finally (talk client state) (disconnect client)
        where
            disconnect client = do
                modifyMVar_ state $ \s -> do
                    return (removeClient client s)

talk :: Client -> MVar ServerState -> IO ()
talk (_, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcastMessage msg

broadcastMessage :: Text -> ServerState -> IO()
broadcastMessage msg (ServerState { clients = cls }) = do
    T.putStrLn msg
    forM_ cls $ \(_, conn) -> WS.sendTextData conn msg 

-- ----------------------------------------------------------------------------------------------
-- Helper Functions for modifying ServerState
-- ----------------------------------------------------------------------------------------------

addClientToState :: Client ->  MVar ServerState -> IO ()
addClientToState client state = do
    modifyMVar_ state $ \s -> do
        let clientList = addNewClient client s
        return clientList

doesClientExist :: Client -> ServerState -> Bool
doesClientExist client (ServerState cls)  = any ((== fst client) . fst) cls

addNewClient :: Client -> ServerState -> ServerState
addNewClient client state@(ServerState cls) = if not (doesClientExist client state)
                            then state { clients = client : cls }
                            else state

removeClient :: Client  -> ServerState -> ServerState
removeClient client state@(ServerState cls) = state { clients = filteredClients }
    where
        filteredClients = filter ((/= fst client) . fst) cls