{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Server where
import Control.Monad (forM_, forever, when)
import Control.Exception (finally)
import Control.Concurrent (newMVar, MVar, readMVar, modifyMVar_)
import Data.ByteString.Char8 as BLU hiding (tail, head, splitAt, map, length, null, filter, putStrLn, any, empty)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Game (Gamestate (playerOrder), Player (Player), Team (Team), shuffle, cardAssign, turn, Named (name), choseCardGs, checkHandleWin, PlayerState, getPlayerState, isFinished)
import Utils (parseHeaders, getNameFromHeaders, getDefaultEmptyGamestate, initializeGameState, parseMessage, Message (Message))
import Constants (host, port)

type Client = (String, WS.Connection)
data ServerState = ServerState { clients :: [Client], gameState :: Gamestate, isStarted :: Bool}

newServer :: ServerState
newServer = ServerState { clients = [], gameState = getDefaultEmptyGamestate, isStarted = False}

runServer :: IO ()
runServer = do
    state <- newMVar newServer
    WS.runServer host port $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    let request = WS.pendingRequest pending
    let headers = parseHeaders (WS.requestHeaders request)
    let playerName = getNameFromHeaders headers
    s <- readMVar state
    if checkSameName s playerName then WS.rejectRequest pending (BLU.pack "Player with same name already playing")
    else if isStarted s then
            if length (clients s) == 4 || not (checkStateExist s playerName)
                then WS.rejectRequest pending (BLU.pack "Game has already started with all 4 clients")
            else checkAndAcceptConnection state playerName pending
        else checkAndAcceptConnection state playerName pending


checkSameName :: ServerState -> String -> Bool
checkSameName state playerName = any (\(nm,_) -> nm == playerName ) (clients state)

checkStateExist :: ServerState -> String -> Bool
checkStateExist state playerName = any (\pl -> name pl == playerName) (playerOrder $ gameState state)

checkAndAcceptConnection :: MVar ServerState -> String -> WS.ServerApp
checkAndAcceptConnection state playerName pending = if not $ null playerName
                                                    then acceptConnection state pending playerName
                                                    else WS.rejectRequest pending (BLU.pack "No name specified")

acceptConnection :: MVar ServerState -> WS.PendingConnection -> String -> IO ()
acceptConnection state pending playerName = do
    conn <- WS.acceptRequest pending
    let client = (playerName, conn)
    addClientToState client state
    putStrLn $ "Client " ++ playerName ++ " connected"
    shouldStartGame <- checkIfAllClientsReady state
    when shouldStartGame $ do
        putStrLn "Starting game"
        initGame state
        readMVar state >>= (\s -> putStrLn "Turn: " >> print (show (turn $ gameState s)))
        broadcastMessage state
    WS.withPingThread conn 30 (return ()) $ do
        finally (talk client state) (disconnect client)
        where
            disconnect client = do
                putStrLn ("Client Disconnected: " ++ fst client)
                modifyMVar_ state $ \s -> do
                    return (removeClient client s)

talk :: Client -> MVar ServerState -> IO ()
talk (_, conn) state = forever $ do
    msg <- WS.receiveData conn
    putStrLn $ "Received message: " ++ show msg
    s <- readMVar state
    if isFinished (gameState s)
        then WS.sendTextData conn (T.pack "Game is finished")
        else if not (isStarted s)
            then WS.sendTextData conn (T.pack "Game has not started yet")
            else case parseMessage msg of
            Left err -> WS.sendTextData conn (T.pack err)
            Right message -> do
                wasValidMove <- performMove message state
                readMVar state >>= (\st -> putStrLn "Next Turn: " >> print (name (turn $ gameState st))) -- only for debugging, remove later
                if wasValidMove
                    then return ()
                    else WS.sendTextData conn (T.pack "Invalid move")

broadcastMessage :: MVar ServerState -> IO()
broadcastMessage state = do
    s <- readMVar state
    let conns = clients s
    let gs = gameState s
    forM_ conns $ \(client, conn) -> WS.sendTextData conn (T.pack $ show $ getPlayerStateFromGamestate gs client)

-- ----------------------------------------------------------------------------------------------
-- Helper Functions for modifying ServerState
-- ----------------------------------------------------------------------------------------------

addClientToState :: Client ->  MVar ServerState -> IO ()
addClientToState client state = do
    modifyMVar_ state $ \s -> do
        let clientList = addNewClient client s
        return clientList

initGame :: MVar ServerState -> IO ()
initGame state =  do
    gs <- readMVar state
    if isStarted gs then return () else modifyMVar_ state $ \s -> do
        teams <- getTeamsFromClient $ clients s
        initialGamestate <- initializeGameState (getPlayersFromClient $ clients s) teams
        let gamestate = cardAssign initialGamestate
        return s { gameState = gamestate, isStarted = True }

doesClientExist :: Client -> ServerState -> Bool
doesClientExist client (ServerState {clients = cls})  = any ((== fst client) . fst) cls

addNewClient :: Client -> ServerState -> ServerState
addNewClient client state@(ServerState {clients = cls}) = if not (doesClientExist client state)
                            then state { clients = client : cls }
                            else state

removeClient :: Client  -> ServerState -> ServerState
removeClient client state@(ServerState {clients = cls}) = state { clients = filteredClients }
    where
        filteredClients = filter ((/= fst client) . fst) cls

getPlayersFromClient :: [Client] -> [Player]
getPlayersFromClient = map (\(playerName, _) -> Player playerName "" [])

getPlayerStateFromGamestate :: Gamestate -> String -> PlayerState
getPlayerStateFromGamestate gs playerName = getPlayerState gs player
    where
        player = head $ filter ((== playerName) . name) (playerOrder gs)

getTeamsFromClient :: [Client] -> IO (Team, Team)
getTeamsFromClient cls = do
    shuffledClients <- shuffle cls
    let (a, b) = splitAt 2 shuffledClients
    return (Team "one" 0 (fst $ head a, fst $ head $ tail a), Team "two" 0 (fst $ head b, fst $ head $ tail b))

checkIfAllClientsReady :: MVar ServerState -> IO Bool
checkIfAllClientsReady state = do
    s <- readMVar state
    let clientList = clients s
    return $ length clientList == 4

-- ----------------------------------------------------------------------------------------------
-- Game Validation Functions
-- ----------------------------------------------------------------------------------------------

performMove :: Message -> MVar ServerState -> IO Bool
performMove (Message player card) state = do
    st <- readMVar state
    let gs = gameState st
    if player == (name $ turn gs)
        then do
            let (isValid, newGs) = choseCardGs card gs
            putStrLn $ "chose card is valid: " ++ show isValid
            if isValid
                then do
                    let upGs = cardAssign $ checkHandleWin newGs
                    modifyMVar_ state $ \s -> do
                        return s { gameState = upGs }
                    broadcastMessage state
                    return True
                else return False
        else return False