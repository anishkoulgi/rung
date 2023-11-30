{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Server where
import Control.Monad (forM_, forever)
import Control.Exception (finally)
import Control.Concurrent (newMVar, MVar, readMVar, modifyMVar_)
import Data.Text (Text)
import Data.Map (Map, empty, member, insert, (!))
import Data.ByteString.Char8 as BLU hiding (length, null, filter, putStrLn, any, empty)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Utils (parseHeaders, getCodeFromHeaders, generateRandomCode)
import Constants (host, port)

type Client = (String, WS.Connection)
data ServerState = ServerState { clients :: [Client], games :: Map String [Client] }

newServer :: ServerState
newServer = ServerState [] empty

runServer :: IO ()
runServer = do
    state <- newMVar newServer
    WS.runServer host port $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    let request = WS.pendingRequest pending
    let headers = parseHeaders (WS.requestHeaders request)
    let code    = getCodeFromHeaders headers
    isValid <- checkCodeValidity code state
    if  null code || isValid    -- If code is empty (meaning client wants to host a game) or valid
        then do
            gameCode <- if null code then generateRandomCode else return code 
            putStrLn gameCode
            acceptConnection state pending gameCode
        else WS.rejectRequest pending (BLU.pack "Invalid joining code")

acceptConnection :: MVar ServerState -> WS.PendingConnection -> String ->  IO ()
acceptConnection state pending code = do
    conn <- WS.acceptRequest pending
    let client = ("New Client", conn) -- TODO: Change this to a random name or accept name from client
    addClientToState client code state
    WS.withPingThread conn 30 (return ()) $ do
        finally (talk client state) (disconnect client)
        where
            disconnect client = do
                modifyMVar_ state $ \s -> do
                    return (removeClient client code s)

talk :: Client -> MVar ServerState -> IO ()
talk (_, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcastMessage msg

broadcastMessage :: Text -> ServerState -> IO()
broadcastMessage msg (ServerState { clients = cls }) = do
    T.putStrLn msg
    forM_ cls $ \(_, conn) -> WS.sendTextData conn msg -- TODO: Send message to only the clients in the same game

checkCodeValidity :: String -> MVar ServerState -> IO Bool
checkCodeValidity code state = do
    s <- readMVar state
    let gameMap = games s
    return (member code gameMap )

-- ----------------------------------------------------------------------------------------------
-- Helper Functions for modifying ServerState
-- ----------------------------------------------------------------------------------------------

addClientToState :: Client -> String -> MVar ServerState -> IO ()
addClientToState client code state = do
    modifyMVar_ state $ \s -> do
        let gameMap = games s
        let clientList = addNewClient client s
        let newGameMap = if member code gameMap
                            then addToGameMap code client gameMap
                            else addNewGame code client gameMap
        return (clientList { games = newGameMap })

addToGameMap :: String -> Client -> Map String [Client] -> Map String [Client]
addToGameMap code client gameMap = do
    let clientList = gameMap ! code
    let newClientList = client : clientList
    insert code newClientList gameMap

deleteFromGameMap :: String -> Client -> Map String [Client] -> Map String [Client]
deleteFromGameMap code client gameMap = do
    let clientList = gameMap ! code
    let newClientList = filter ((/= fst client) . fst) clientList
    insert code newClientList gameMap

addNewGame :: String -> Client -> Map String [Client] -> Map String [Client]
addNewGame code client gameMap = do
    let newGameMap = insert code [client] gameMap
    newGameMap

doesClientExist :: Client -> ServerState -> Bool
doesClientExist client (ServerState cls _) = any ((== fst client) . fst) cls

addNewClient :: Client -> ServerState -> ServerState
addNewClient client state@(ServerState { clients = cls }) = if not (doesClientExist client state)
                            then state { clients = client : cls }
                            else state

removeClient :: Client -> String -> ServerState -> ServerState
removeClient client code state@(ServerState { clients = cls }) = state { clients = filteredClients, games = newGameMap }
    where
        filteredClients = filter ((/= fst client) . fst) cls
        newGameMap = if member code (games state)
                        then deleteFromGameMap code client (games state)
                        else empty