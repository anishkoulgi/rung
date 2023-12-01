module Utils where
import Data.CaseInsensitive (original)
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
getNameFromHeaders :: [(String, String)] -> String
-- fromMaybe returns the default value if the maybe value is Nothing
getNameFromHeaders headers = Data.Maybe.fromMaybe "" (lookup "name" headers) 