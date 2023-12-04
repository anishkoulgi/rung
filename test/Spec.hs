import Test.Tasty
import Common
import Game
    ( Card(..),
      Player(..),
      Suit(..),
      Value(..), Gamestate (..), Team (Team), cardAssign, initialDeck)
import Prelude hiding (round)



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


team1 :: Team
team1 = Team "one" 0 ("Nipun","Anish")
team2 :: Team
team2 = Team "two" 0 ("Mahesh","Suresh")

initialGameState :: Gamestate
initialGameState = Gamestate initialDeck 0 allPlayers [] (team1,team2) Spades

cardsAfterRoundZero :: [Card]
cardsAfterRoundZero = drop 20 initialDeck

orderAfter1Assign :: [Player]
orderAfter1Assign = [
  Player {_nameP = "Nipun", _id = "1", cards = [Card {suit = Spades, value = Two},Card {suit = Spades, value = Three},Card {suit = Spades, value = Four},Card {suit = Spades, value = Five},Card {suit = Spades, value = Six}]},
  Player {_nameP = "Anish", _id = "2", cards = [Card {suit = Spades, value = Seven},Card {suit = Spades, value = Eight},Card {suit = Spades, value = Nine},Card {suit = Spades, value = Ten},Card {suit = Spades, value = Joker}]},
  Player {_nameP = "Mahesh", _id = "3", cards = [Card {suit = Spades, value = Queen},Card {suit = Spades, value = King},Card {suit = Spades, value = Ace},Card {suit = Hearts, value = Two},Card {suit = Hearts, value = Three}]},
  Player {_nameP = "Suresh", _id = "4", cards = [Card {suit = Hearts, value = Four},Card {suit = Hearts, value = Five},Card {suit = Hearts, value = Six},Card {suit = Hearts, value = Seven},Card {suit = Hearts, value = Eight}]}
  ]

newGs :: Gamestate
newGs = Gamestate {
  remainingCards = cardsAfterRoundZero, round = 1, 
  playerOrder = allPlayers, currentRound = [], teams = (team1,team2), trump = Spades}


testAssignCard :: Score -> TestTree
testAssignCard sc = testGroup "Assign Card"
            [
              scoreTest (\_ -> remainingCards $ cardAssign initialGameState, (), cardsAfterRoundZero, 1, "round-1-remCards"),
              scoreTest (\_ -> playerOrder $ cardAssign initialGameState, (), orderAfter1Assign, 1, "round-1-playerOrder"),
              scoreTest (\_ -> remainingCards $ cardAssign newGs, (), drop 12 cardsAfterRoundZero,1, "round-2-remCards"),
              scoreTest (\_ -> round $ cardAssign newGs, (), 2, 1,"round-2-number"),
              scoreTest (\_ -> cardAssign $ cardAssign newGs, (), cardAssign newGs, 1,"round-2-repeat")
            ]
        where
        scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
        scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

testChooseCard :: Score -> TestTree
testChooseCard = undefined

testReplaceVal :: Score -> TestTree
testReplaceVal = undefined

testReorderPlayer :: Score -> TestTree
testReorderPlayer = undefined

testCheckHandleWin :: Score -> TestTree
testCheckHandleWin = undefined


main :: IO ()
main = runTests [testAssignCard]


