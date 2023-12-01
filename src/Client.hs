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


--------------------------------------------------------------------------------
application :: WS.ClientApp ()
application conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

runClient :: IO ()
runClient = do
    putStrLn "Enter your name:"
    name <- getLine
    let headers = makeNameHeader name 
    withSocketsDo $ WS.runClientWith host port "/" WS.defaultConnectionOptions headers application 

-- | Creates the game name header for the WS request
makeNameHeader :: String -> Headers
makeNameHeader name = [("name", BLU.pack name)]
