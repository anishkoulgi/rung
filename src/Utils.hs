module Utils where
import qualified Network.WebSockets as WS
import Data.ByteString.Char8 as BLU hiding (map)
import Data.CaseInsensitive (original)
import qualified Data.Maybe

port :: Int
port = 3333

host :: String
host = "localhost"

-- | Parse the headers from a WS request
-- | Returns a list of tuples of the form (key, value)
-- | Example: [("code", "1234")]
parseHeaders :: WS.Headers -> [(String, String)]
parseHeaders = map (\(key, val) -> (BLU.unpack (original key), BLU.unpack val))

-- | Get the code from the parsed headers
getCodeFromHeaders :: [(String, String)] -> String
getCodeFromHeaders headers = Data.Maybe.fromMaybe "" (lookup "code" headers)