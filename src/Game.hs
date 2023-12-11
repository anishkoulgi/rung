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

import Lens.Micro

import Objects

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
isAssignmentRequired gs = null ((turn gs^.cardsP))


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
assignCardPl _ (remCards,[]) = (True,(remCards,[]))
assignCardPl numCards currentState@(remCards,p:ps)
  | numCards > length remCards = (False,currentState)                                          -- Insufficient cards remaining, return flag as False 
  | otherwise = (flg,(finalRemCards,finalPlayers)) where
            newp = Player (p^.nameP) (p^.idP) (take numCards remCards)                                     -- New player with cards assigned 
            (flg,(updatedRemCards,plyrs)) = assignCardPl numCards (drop numCards remCards,ps)   -- Assign cards to other players
            finalRemCards = if flg then updatedRemCards else remCards
            finalPlayers = if flg then newp:plyrs else p:ps



initialDeck :: [Card]
initialDeck = [Card st val | st <- [Spades .. Diamonds],val <- [Two .. Ace]]
nipun :: Player
nipun = Player "Nipun" "1" []
anish :: Player
anish = Player "Anish" "2" []
mahesh :: Player
mahesh = Player "Mahesh" "3" []
suresh :: Player
suresh = Player "Suresh" "4" []

allPlayers :: [Player]
allPlayers = [nipun,anish,mahesh,suresh]

-- PlayerState to send to the client of player for displaying on UI
getPlayerState :: Gamestate -> Player -> PlayerState
getPlayerState gs pl = PlayerState pl (currentRound gs) (round gs) (teams gs) (trump gs) (turn gs == pl)

-- Based on gamestate, return the player whose turn it is to play
turn :: Gamestate -> Player
turn (Gamestate _ _ pls cr _ _) = pls !! length cr

-- Update the gamestate to chose the chosen card
choseCard :: Card -> State Gamestate Bool
choseCard crd = state (choseCardGs crd)

-- Get valid suits which can be played
getValidSuits :: Gamestate -> [Suit]
getValidSuits g = case currentRound g of
  [] -> [Spades .. Diamonds]
  (c:_) -> [suit c]

-- Check if the current player has no card matching the suit of the game
checkNoCardPossible :: Gamestate -> Bool
checkNoCardPossible gs = any ((\st -> st `elem` getValidSuits gs) . suit) ((turn gs)^.cardsP) -- get the cards of the current player whose "turn" it is and check if they have atleast 1 card in this suit

-- Helper function to replace first occurence of old val with new one in a list
replaceVal :: Eq a => a -> a -> [a] -> [a]
replaceVal old new lst = case lst of
                            [] -> []
                            (l:ls) -> if l == old then new:ls else l:replaceVal old new ls

-- Check if the current card chosen is valid or not
isChosenCardValid :: Card -> Gamestate -> Bool
isChosenCardValid crd gs = elem crd ((turn gs)^.cardsP) && (not (checkNoCardPossible gs) || elem (suit crd) (getValidSuits gs))


-- Chose card to update the gamestate, see if we were able to chose card or wrong card was chosen
choseCardGs :: Card -> Gamestate -> (Bool,Gamestate)
choseCardGs crd gs@(Gamestate remC rnd pls cr tm trmp) = if isChosenCardValid crd gs
                                then (True,Gamestate remC rnd (replaceVal oldP newP pls) (cr ++ [crd]) tm trmp)
                                else (False,gs) where
                                 oldP = turn gs
                                 newP = Player (oldP ^. nameP) (oldP^.idP) (delete crd  (oldP^.cardsP))

-- Check to see if the game is finished
isFinished :: Gamestate -> Bool
isFinished (Gamestate _ rnd _ _ tm _) = fn rnd tm where
  fn 4 _ = True               -- Last round
  fn _ (Team _ 7 _,_) = True  -- Team 1 has 7 points
  fn _ (_, Team _ 7 _) = True -- Team 2 has 7 points
  fn _ _ = False              -- Otherwise game hasn't finished


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
updateWinningPl pl t@(Team x pt (a,b)) = if pl^.nameP `elem` [a,b] then Team x (pt+1) (a,b) else t

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
checkHandleWin (Gamestate remC rnd pOrder cr@[_,_,_,_] tm trmp) = ns where
               ns = Gamestate remC rnd pOrderUpdated [] (updateWinningPl winningPl ta,updateWinningPl winningPl tb) trmp
               (ta,tb) = tm
               winningCrd = winningCard trmp cr
               pOrderUpdated = reorderPL winningPl pOrder
               winningPl = case elemIndex winningCrd cr of
                                Just x -> pOrder!!x
                                Nothing -> head pOrder

checkHandleWin gg = gg


getDefaultEmptyGamestate ::  Gamestate 
getDefaultEmptyGamestate =  Gamestate [] 0 [] [] (Team "" 0 ("",""), Team "" 0 ("","")) Spades

initializeGameState :: [Player] -> (Team, Team) -> IO Gamestate
initializeGameState players teams = do
    deck <- shuffle initialDeck
    return (Gamestate deck 0 players [] teams Spades)

-- Helpers for testing
team1 :: Team
team1 = Team "one" 0 ("Nipun","Anish")
team2 :: Team
team2 = Team "two" 0 ("Mahesh","Suresh")

-- >>> name (turn initialGameState)
-- "Nipun"

showPts :: (Team,Team) -> String
showPts (a,b) = a^.nameT ++ " : " ++ show (a^.pointsT) ++ " -- " ++ b^.nameT ++ " : " ++ show (b^.pointsT)

-- Run the game on console where each player takes turn for manual testing
runGame :: Gamestate -> IO ()
runGame gs = let vals = cardAssign gs in do {
    print vals;
    print ("Round Number: " ++ show ( round vals));
    print (showPts (teams vals) );
    print (currentRound vals);
    putStrLn ("Chose card player " ++ (turn vals)^.nameP);
    print ((turn vals)^.cardsP);
    chosenCard <- getLine;
    runGame $ checkHandleWin $ snd $ choseCardGs (read chosenCard) vals;
}