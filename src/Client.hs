{-# LANGUAGE OverloadedStrings #-}
module Client where
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import Data.ByteString.Char8 as BLU hiding (getLine, putStrLn)
import qualified Network.WebSockets  as WS
import Network.WebSockets (Headers)
import Constants (host, port)
import Text.Read (readEither)
import Game (Card)
import Utils (Message(Message))


--------------------------------------------------------------------------------
application :: String -> WS.ClientApp ()
application name conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            case (readEither (T.unpack line) :: Either String Card) of
                Left _ -> putStrLn "Invalid card format" >> loop
                Right card -> do
                    let message = Message name card 
                    putStrLn $ "Sending message: " ++ show message
                    WS.sendTextData conn (T.pack $ show message) >> loop
    _ <- loop
    WS.sendClose conn ("Bye!" :: Text)

runClient :: IO ()
runClient = do
    putStrLn "Enter your name:"
    name <- getLine
    let headers = makeNameHeader name 
    withSocketsDo $ WS.runClientWith host port "/" WS.defaultConnectionOptions headers (application name)

-- | Creates the game name header for the WS request
makeNameHeader :: String -> Headers
makeNameHeader name = [("name", BLU.pack name)]
