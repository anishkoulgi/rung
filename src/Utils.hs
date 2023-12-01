module Utils where
import Data.CaseInsensitive (original)
import Data.ByteString.Char8 as BLU hiding (map)
import qualified Data.Maybe
import qualified Network.WebSockets as WS
import qualified Game as G 
import Game (initialDeck)

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

initializeGameState :: [G.Player] -> (G.Team, G.Team) -> G.Gamestate
initializeGameState players teams = G.Gamestate initialDeck 0 players [] teams G.Spades
