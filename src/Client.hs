{-# LANGUAGE OverloadedStrings #-}
module Client (clientMain) where
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO, newEmptyMVar, takeMVar)
import           Control.Monad       (forever)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import Data.ByteString.Char8 as BLU hiding (getLine, putStrLn)
import qualified Network.WebSockets  as WS
import Network.WebSockets (Headers)
import Brick.BChan
import Constants (port)
import Text.Read (readMaybe)
import Utils (Message(Message))
import UI.Client.InitialClientPage(getClientData)
import UI.Client.ClientPage(showClientUI)
import Objects
import Lens.Micro


--------------------------------------------------------------------------------
application :: String -> WS.ClientApp ()
application name conn = do
    putStrLn "Connected!"
    clientState <- newEmptyMVar
    bc <- newBChan 1
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        -- putStrLn ("Msg: " ++ (T.unpack msg))
        -- putStrLn ("Type: " ++ show(typeOf(T.unpack msg)))
        let mPlayerState = readMaybe $ T.unpack msg :: Maybe PlayerState
        case mPlayerState of
            (Just playerState) -> writeBChan bc playerState
            _ -> return ()
        -- putStrLn ("playerState: " ++ (show playerState))
        
        
    _ <- forkIO $ do
        showClientUI bc clientState

    _ <- forever $ do
        -- putStrLn("In second thread wait")
        card <- takeMVar clientState
        -- putStrLn ("After accesing MVar")
        let message = Message name card 
        -- putStrLn $ "Sending message: " ++ show message
        WS.sendTextData conn (T.pack $ show message)

    WS.sendClose conn ("Bye!" :: Text)

-- | Creates the game name header for the WS request
makeNameHeader :: String -> Headers
makeNameHeader name = [("name", BLU.pack name)]


clientMain :: IO()
clientMain = do
    clientInfo <- getClientData
    let name = T.unpack $ clientInfo^.nameC
    let ip = T.unpack $  clientInfo^.ipC
    let headers = makeNameHeader  name
    withSocketsDo $ WS.runClientWith ip port "/" WS.defaultConnectionOptions headers (application name)
    print ()