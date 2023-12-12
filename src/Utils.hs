module Utils
    ( parseHeaders
    , Message(..)
    , getNameFromHeaders
    , parseMessage
    , mapWithIndex
    , getCardNumber
    , getSuitUnicode
    , getSuitColor
    , isValidIP
    , isValidCardPs
    )
where
import Data.CaseInsensitive (original)
import qualified Data.ByteString.Char8 as BLU
import qualified Data.Maybe
import qualified Network.WebSockets as WS
import Objects
import Data.Text (Text, unpack, splitOn, pack)
import Text.Read (readEither)
import Text.Read (readMaybe)
import Lens.Micro
import Net.IP as IP

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

---------------------------------------------------------------------------------------------------
-- Card Object conversion functions
---------------------------------------------------------------------------------------------------

getCardNumber :: Value -> String
getCardNumber Ace = "A"
getCardNumber Two = "2"
getCardNumber Three = "3"
getCardNumber Four = "4"
getCardNumber Five = "5"
getCardNumber Six = "6"
getCardNumber Seven = "7"
getCardNumber Eight = "8"
getCardNumber Nine = "9"
getCardNumber Ten = "10"
getCardNumber King = "K"
getCardNumber Queen = "Q"
getCardNumber Jack = "J"


getSuitUnicode :: Suit -> String
getSuitUnicode s = case s of
    Spades   -> [toEnum 0x2660] :: String
    Diamonds ->  [toEnum 0x2666] :: String
    Hearts   -> [toEnum 0x2665] :: String
    _        -> [toEnum 0x2663] :: String

getSuitColor :: Suit -> String
getSuitColor s = case s of
    Spades   -> "black"
    Hearts   -> "red"
    Diamonds -> "red"
    _        -> "black"

getValSuit :: [Card] -> [Suit]
getValSuit crds = case crds of
                 [] -> [Spades .. Diamonds]
                 (c:_) -> [suit c]

checkNoCardPossibleClient :: [Card] -> [Card] -> Bool
checkNoCardPossibleClient plrCards rndCards = any (\card -> suit card `elem` getValSuit rndCards) plrCards

isValidCardPs :: [Card] -> Int -> [Card]  -> Bool
isValidCardPs plrCards idx  rndCards  = not (checkNoCardPossibleClient plrCards rndCards) || suit crd `elem` getValSuit rndCards
    where crd = plrCards !! idx

---------------------------------------------------------------------------------------------------
-- Input validation helper methods
---------------------------------------------------------------------------------------------------
isValidIP :: Text -> Bool
isValidIP ipText =
    (unpack ipText == "localhost") || case IP.decode ipText of
                                        Just _ -> True
                                        Nothing -> False

