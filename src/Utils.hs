module Utils where
import Data.CaseInsensitive (original)
import qualified Data.ByteString.Char8 as BLU 
import qualified Data.Maybe
import qualified Network.WebSockets as WS
import qualified Game as G 
import Game (initialDeck)
import Data.Text (Text, unpack)
import Text.Read (readEither)

data Message = Message { player :: String, move :: G.Card } deriving (Show, Read)

---------------------------------------------------------------------------------------------------
-- | Parse the headers from a WS request
--   Returns a list of tuples of the form (key, value)
--   Example: [("code", "1234")]
---------------------------------------------------------------------------------------------------
parseHeaders :: WS.Headers -> [(String, String)]
parseHeaders = map (\(key, val) -> (BLU.unpack (original key), BLU.unpack val))

-- | Get the code from the parsed headers
getNameFromHeaders :: [(String, String)] -> String
-- fromMaybe returns the default value if the maybe value is Nothing
getNameFromHeaders headers = Data.Maybe.fromMaybe "" (lookup "name" headers) 

getDefaultEmptyGamestate ::  G.Gamestate 
getDefaultEmptyGamestate =  G.Gamestate [] 0 [] [] (G.Team "" 0 ("",""), G.Team "" 0 ("","")) G.Spades

initializeGameState :: [G.Player] -> (G.Team, G.Team) -> IO G.Gamestate
initializeGameState players teams = do
    deck <- G.shuffle initialDeck
    return (G.Gamestate deck 0 players [] teams G.Spades)

---------------------------------------------------------------------------------------------------
-- Parse message functions
---------------------------------------------------------------------------------------------------

parseMessage :: Text -> Either String Message 
parseMessage msg = case readEither $ unpack msg :: Either String Message of
    Left  _       -> Left "Invalid message format"
    Right message -> Right message
