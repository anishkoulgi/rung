{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Game where
import Data.Array.IO

import System.Random
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import Prelude hiding (round)

data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq,Show,Enum,Read)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Joker | Queen | King | Ace deriving (Ord,Eq,Show,Enum,Read)
data Card = Card {suit:: Suit, value::Value} deriving (Eq,Show,Read)


class Named a where
    name :: a -> String


data Player = Player {_nameP::String, cards::[Card]} deriving (Eq,Show,Read)
data Team = Team {_nameT::String, points::Int, players::(String,String)} deriving (Eq,Show,Read)

instance Named Player where
  name :: Player -> String
  name = _nameP

instance Named Team where
  name :: Team -> String
  name = _nameT

data Gamestate = Gamestate {remainingCards :: [Card], round::Int, playerOrder::[Player], currentRound::[Card], teams::(Team,Team), trump::Suit} deriving (Show,Read)


-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n = newListArray (1,n)


-- Assign cards to players
assignCards :: State Gamestate Bool
assignCards = state assignCardGs

-- Based on the gamestate , see if assignment is required
-- Checks if the current player doesn't have cards to assign
isAssignmentRequired :: Gamestate -> Bool
isAssignmentRequired gs = null (cards (turn gs))


-- Take a gamestate and return a new one with cards assigned based on the number of rounds
-- Doesn't change the gamestate if assignment is not required
cardAssign :: Gamestate -> Gamestate
cardAssign gs = if isAssignmentRequired gs then snd $ assignCardGs gs else gs

-- Actually Assign cards and if their is some issue so that cards are not assigned , returns False in the Boolean
assignCardGs :: Gamestate -> (Bool,Gamestate)
assignCardGs gs@(Gamestate [] _ _ _ _ _) = (False, gs)
assignCardGs gs@(Gamestate _ _ _ (_:_) _ _) = (False, gs)
assignCardGs gs@(Gamestate remC rnd pls cr tm trmp) = if not flg then (flg,gs) else (flg,Gamestate rmC (rnd+1) plyrs cr tm trmp) where
                                        (flg,(rmC,plyrs))  =  runState (assignCardPs rnd) (remC, pls)

-- Based on the round , returns a state that assignes the card (5 - 3 - 3 - 2)
assignCardPs :: Int -> State ([Card],[Player]) Bool
assignCardPs 0 = state (assignCardPl 5)
assignCardPs 1 = state (assignCardPl 3)
assignCardPs 2 = state (assignCardPl 3)
assignCardPs 3 = state (assignCardPl 2)
assignCardPs _ = state (\x -> (False,x))

-- Given number of cards to assign each player and list of cards and players,
-- Assign each player the card and returns the updated players
assignCardPl :: Int -> ([Card],[Player]) -> (Bool,([Card],[Player]))
assignCardPl _ (cs,[]) = (True,(cs,[]))
assignCardPl n (cs,p:ps) = if n >= length cs then (False,(cs,p:ps)) else (flg,(cc,pp)) where
            newp = Player (name p) (take n cs)
            (flg,(c,plyrs)) = assignCardPl n (drop n cs,ps)
            cc = if flg then c else cs
            pp = if flg then pls else p:ps
            pls = newp:plyrs



initialDeck :: [Card]
initialDeck = [Card st val | st <- [Spades .. Diamonds],val <- [Two .. Ace]]
nipun :: Player
nipun = Player "Nipun" []
anish :: Player
anish = Player "Anish" []
mahesh :: Player
mahesh = Player "Mahesh" []
suresh :: Player
suresh = Player "Suresh" []

allPlayers :: [Player]
allPlayers = [nipun,anish,mahesh,suresh]


-- Based on gamestate, return the player whose turn it is to play
turn :: Gamestate -> Player
turn (Gamestate _ _ pls cr _ _) = pls !! length cr

-- Update the gamestate to chose the chosen card
choseCard :: Card -> State Gamestate Bool
choseCard crd = state (choseCardGs crd)

-- Get valid suits which can be played
getValidSuits :: Gamestate -> [Suit]
getValidSuits (Gamestate _ _ _ [] _ _) = [Spades .. Diamonds]
getValidSuits (Gamestate _ _ _ (c:_) _ _) = [suit c]

-- Check if the current player has no card matching the suit of the game
checkNoCardPossible :: Gamestate -> Bool
checkNoCardPossible gs = any ((\st -> st `elem` getValidSuits gs) . suit) (cards (turn gs))

-- Helper function to replace first occurence of old val with new one in a list
replaceVal :: Eq a => a -> a -> [a] -> [a]
replaceVal old new lst = case lst of
                            [] -> []
                            (l:ls) -> if l == old then new:ls else l:replaceVal old new ls

-- Check if the current card chosen is valid or not
isChosenCardValid :: Card -> Gamestate -> Bool
isChosenCardValid crd gs = elem crd (cards (turn gs)) && (not (checkNoCardPossible gs) || elem (suit crd) (getValidSuits gs))


-- Chose card to update the gamestate, see if we were able to chose card or wrong card was chosen
choseCardGs :: Card -> Gamestate -> (Bool,Gamestate)
choseCardGs crd gs@(Gamestate remC rnd pls cr tm trmp) = if isChosenCardValid crd gs
                                then (True,Gamestate remC rnd (replaceVal oldP newP pls) (cr ++ [crd]) tm trmp)
                                else (False,gs) where
                                 oldP = turn gs
                                 newP = Player (name oldP) (delete crd (cards oldP))

-- Check to see if the game is finished
isFinished :: Gamestate -> Bool
isFinished (Gamestate _ 4 _ _ _ _) =True
isFinished (Gamestate _ _ _ _ (Team _ 7 _,_) _) = True
isFinished (Gamestate _ _ _ _ (_, Team _ 7 _) _) = True
isFinished _ = False


-- Convert card to number based on trump and current suit of the game
cardToNum :: Suit -> Suit -> Card -> Int
cardToNum trmp color crd
  | suit crd == trmp = -100 * (1 + fromEnum (value crd))
  | suit crd == color = -10 * (1 + fromEnum (value crd))
  | otherwise = 0

-- Find which card won based on trump and list of cards
winningCard :: Suit -> [Card] -> Card
winningCard st crds = head sortCrds where
            cToNum = cardToNum st (suit $ head crds)
            sortCrds = sortBy (\a b -> compare (cToNum a) (cToNum b)) crds

-- If the winning player is in the team, increment the points of the team
updateWinningPl :: Player -> Team -> Team
updateWinningPl pl t@(Team x pt (a,b)) = if name pl `elem` [a,b] then Team x (pt+1) (a,b) else t

-- Reorder players based on the player who won the previous game
reorderPL :: Player -> [Player] -> [Player]
reorderPL winPl pls = winPl : (b ++ a) where
           (a,b) = case splitOn [winPl] pls of
                [x,y] -> (x,y)
                _ -> ([],[])

-- >>> reorderPL nipun [mahesh,anish,nipun,suresh]
-- [Player {_nameP = "Nipun", cards = []},Player {_nameP = "Suresh", cards = []},Player {_nameP = "Mahesh", cards = []},Player {_nameP = "Anish", cards = []}]


-- See if the current game is complete and update the gamestate based on that
checkHandleWin :: Gamestate -> Gamestate
checkHandleWin (Gamestate remC rnd o cr@[_,_,_,_] tm trmp) = ns where
               ns = Gamestate remC rnd oo [] (updateWinningPl winningPl ta,updateWinningPl winningPl tb) trmp
               (ta,tb) = tm
               winningCrd = winningCard trmp cr
               oo = reorderPL winningPl o
               winningPl = case elemIndex winningCrd cr of
                                Just x -> o!!x
                                Nothing -> head o

checkHandleWin gg = gg


-- Helpers for testing
team1 :: Team
team1 = Team "one" 0 ("Nipun","Anish")
team2 :: Team
team2 = Team "two" 0 ("Mahesh","Suresh")

-- >>> name (turn initialGameState)
-- "Nipun"

showPts :: (Team,Team) -> String
showPts (a,b) = name a ++ " : " ++ show (points a) ++ " -- " ++ name b ++ " : " ++ show (points b)

-- Run the game on console where each player takes turn for manual testing
runGame :: Gamestate -> IO ()
runGame gs = let vals = cardAssign gs in do {
    print vals;
    print ("Round Number: " ++ show ( round vals));
    print (showPts(teams vals) );
    print (currentRound vals);
    putStrLn ("Chose card player " ++ name (turn vals));
    print (cards (turn vals));
    chosenCard <- getLine;
    runGame $ checkHandleWin $ snd $ choseCardGs (read chosenCard) vals;
}

beforeFinished :: Gamestate
beforeFinished = Gamestate {remainingCards = [Card {suit = Clubs, value = Ace},Card {suit = Clubs, value = Seven},Card {suit = Diamonds, value = Six},Card {suit = Diamonds, value = Nine},Card {suit = Diamonds, value = Five},Card {suit = Spades, value = King},Card {suit = Clubs, value = Joker},Card {suit = Spades, value = Nine}], round = 3, playerOrder = [Player {_nameP = "Anish", cards = [Card {suit = Spades, value = Ten},Card {suit = Hearts, value = Ten}]},Player {_nameP = "Suresh", cards = [Card {suit = Clubs, value = Queen},Card {suit = Clubs, value = Five}]},Player {_nameP = "Nipun", cards = [Card {suit = Hearts, value = Joker},Card {suit = Clubs, value = Two}]},Player {_nameP = "Mahesh", cards = [Card {suit = Diamonds, value = Two},Card {suit = Hearts, value = Nine},Card {suit = Hearts, value = Six}]}], currentRound = [Card {suit = Spades, value = Queen},Card {suit = Spades, value = Four},Card {suit = Spades, value = Joker}], teams = (Team {_nameT = "one", points = 6, players = ("Nipun","Anish")},Team {_nameT = "two", points = 2, players = ("Mahesh","Suresh")}), trump = Spades}

testFinished :: Gamestate
testFinished = Gamestate {remainingCards = [Card {suit = Clubs, value = Ace},Card {suit = Clubs, value = Seven},Card {suit = Diamonds, value = Six},Card {suit = Diamonds, value = Nine},Card {suit = Diamonds, value = Five},Card {suit = Spades, value = King},Card {suit = Clubs, value = Joker},Card {suit = Spades, value = Nine}], round = 3, playerOrder = [Player {_nameP = "Anish", cards = [Card {suit = Spades, value = Ten},Card {suit = Hearts, value = Ten}]},Player {_nameP = "Suresh", cards = [Card {suit = Clubs, value = Queen},Card {suit = Clubs, value = Five}]},Player {_nameP = "Nipun", cards = [Card {suit = Hearts, value = Joker},Card {suit = Clubs, value = Two}]},Player {_nameP = "Mahesh", cards = [Card {suit = Diamonds, value = Two},Card {suit = Hearts, value = Six}]}], currentRound = [], teams = (Team {_nameT = "one", points = 7, players = ("Nipun","Anish")},Team {_nameT = "two", points = 2, players = ("Mahesh","Suresh")}), trump = Spades}

-- >>> isFinished beforeFinished
-- False













