module Lib
    ( someFunc
    ) where
import System.Environment (getArgs)
import Client (runClient)
import Server (runServer)


someFunc :: IO ()
someFunc = do
    putStrLn "Starting application..."
    args  <- getArgs
    if "client" `elem` args 
        then runClient 
        else runServer 