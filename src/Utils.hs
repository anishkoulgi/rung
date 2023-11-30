module Utils where
import Data.CaseInsensitive (original)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Data.ByteString.Char8 as BLU hiding (map)
import qualified Data.Maybe
import qualified Network.WebSockets as WS

---------------------------------------------------------------------------------------------------
-- | Parse the headers from a WS request
--   Returns a list of tuples of the form (key, value)
--   Example: [("code", "1234")]
---------------------------------------------------------------------------------------------------
parseHeaders :: WS.Headers -> [(String, String)]
parseHeaders = map (\(key, val) -> (BLU.unpack (original key), BLU.unpack val))

-- | Get the code from the parsed headers
getCodeFromHeaders :: [(String, String)] -> String
-- fromMaybe returns the default value if the maybe value is Nothing
getCodeFromHeaders headers = Data.Maybe.fromMaybe "" (lookup "code" headers) 

-- Generate a random UUID code for the game
generateRandomCode :: IO String
generateRandomCode = do
    toString <$> nextRandom