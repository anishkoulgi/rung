module Utils(parseHeaders, Message(..), getNameFromHeaders, parseMessage,mapWithIndex) where
import Data.CaseInsensitive (original)
import qualified Data.ByteString.Char8 as BLU 
import qualified Data.Maybe
import qualified Network.WebSockets as WS
import Objects
import Data.Text (Text, unpack)
import Text.Read (readEither)

data Message = Message { player :: String, move :: Card } deriving (Show, Read)

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

---------------------------------------------------------------------------------------------------
-- Parse message functions
---------------------------------------------------------------------------------------------------

parseMessage :: Text -> Either String Message 
parseMessage msg = case readEither $ unpack msg :: Either String Message of
    Left  _       -> Left "Invalid message format"
    Right message -> Right message


mapWithIndex :: ((Int,a) -> b) -> [a] -> [b]
mapWithIndex f xs = map f z
    where
        z = zip [0..] xs