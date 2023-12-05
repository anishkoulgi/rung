import Test.Tasty
import Common
import Game
    ( Card(..),
      Player(..),
      Suit(..),
      Value(..), Gamestate (..), Team (Team,points), cardAssign, initialDeck, choseCardGs, replaceVal, reorderPL, checkHandleWin, winningCard, choseCard)
import Prelude hiding (round)
import Control.Monad.State (execState)



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
team1 = Team "one" 0 ("Nipun","Mahesh")
team2 :: Team
team2 = Team "two" 0 ("Anish","Suresh")

initialGameState :: Gamestate
initialGameState = Gamestate initialDeck 0 allPlayers [] (team1,team2) Spades

cardsAfterRoundZero :: [Card]
cardsAfterRoundZero = drop 20 initialDeck

orderAfter1Assign :: [Player]
orderAfter1Assign = [
  Player {_nameP = "Nipun", _id = "1", cards = [Card {suit = Spades, value = Two},Card {suit = Spades, value = Three},Card {suit = Spades, value = Four},Card {suit = Spades, value = Five},Card {suit = Spades, value = Six}]},
  Player {_nameP = "Anish", _id = "2", cards = [Card {suit = Spades, value = Seven},Card {suit = Spades, value = Eight},Card {suit = Spades, value = Nine},Card {suit = Spades, value = Ten},Card {suit = Spades, value = Jack}]},
  Player {_nameP = "Mahesh", _id = "3", cards = [Card {suit = Spades, value = Queen},Card {suit = Spades, value = King},Card {suit = Spades, value = Ace},Card {suit = Hearts, value = Two},Card {suit = Hearts, value = Three}]},
  Player {_nameP = "Suresh", _id = "4", cards = [Card {suit = Hearts, value = Four},Card {suit = Hearts, value = Five},Card {suit = Hearts, value = Six},Card {suit = Hearts, value = Seven},Card {suit = Hearts, value = Eight}]}
  ]

orderAfter1Chose :: [Player]
orderAfter1Chose = [
  Player {_nameP = "Nipun", _id = "1", cards = [Card {suit = Spades, value = Two},Card {suit = Spades, value = Three},Card {suit = Spades, value = Five},Card {suit = Spades, value = Six}]},
  Player {_nameP = "Anish", _id = "2", cards = [Card {suit = Spades, value = Seven},Card {suit = Spades, value = Eight},Card {suit = Spades, value = Nine},Card {suit = Spades, value = Ten},Card {suit = Spades, value = Jack}]},
  Player {_nameP = "Mahesh", _id = "3", cards = [Card {suit = Spades, value = Queen},Card {suit = Spades, value = King},Card {suit = Spades, value = Ace},Card {suit = Hearts, value = Two},Card {suit = Hearts, value = Three}]},
  Player {_nameP = "Suresh", _id = "4", cards = [Card {suit = Hearts, value = Four},Card {suit = Hearts, value = Five},Card {suit = Hearts, value = Six},Card {suit = Hearts, value = Seven},Card {suit = Hearts, value = Eight}]}
  ]

newGs :: Gamestate
newGs = Gamestate {
  remainingCards = cardsAfterRoundZero, round = 1,
  playerOrder = allPlayers, currentRound = [], teams = (team1,team2), trump = Spades}

scoreTest :: (Show b, Eq b) => Score -> (a -> b, a, b, Int, String) -> TestTree
scoreTest sc (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

testAssignCard :: Score -> TestTree
testAssignCard sc = testGroup "Assign Card"
            [
              scoreTest sc (\_ -> remainingCards $ cardAssign initialGameState, (), cardsAfterRoundZero, 1, "round-1-remCards"),
              scoreTest sc (\_ -> playerOrder $ cardAssign initialGameState, (), orderAfter1Assign, 1, "round-1-playerOrder"),
              scoreTest sc (\_ -> remainingCards $ cardAssign newGs, (), drop 12 cardsAfterRoundZero,1, "round-2-remCards"),
              scoreTest sc (\_ -> round $ cardAssign newGs, (), 2, 1,"round-2-number"),
              scoreTest sc (\_ -> cardAssign $ cardAssign newGs, (), cardAssign newGs, 1,"round-2-repeat")
            ]

testChooseCard :: Score -> TestTree
testChooseCard sc = testGroup "Choose Card"
            [
              scoreTest sc (\_ ->  choseCardGs (head initialDeck) initialGameState, (), (False,initialGameState),1, "CheckEmpty"),
              scoreTest sc (\_ -> fst $ choseCardGs (head initialDeck) (cardAssign initialGameState), (), True,1, "CheckTrue"),
              scoreTest sc (\_ -> fst $ choseCardGs (initialDeck !! 20) (cardAssign initialGameState), (), False, 1,"CheckFalse"),
              scoreTest sc (\_ -> currentRound $ snd $ choseCardGs (initialDeck !! 2) (cardAssign initialGameState), (), [initialDeck!!2], 1,"CheckCurrentRound"),
              scoreTest sc (\_ -> playerOrder $ snd $ choseCardGs (initialDeck !! 2) (cardAssign initialGameState), (), orderAfter1Chose, 1,"CheckPlayerOrder")
            ]



testReplaceVal :: Score -> TestTree
testReplaceVal sc = testGroup "Replace Val"
            [
              scoreTest sc (\_ -> replaceVal 3 5 inpArr, (), [1,2,5,4,5],1, "ReplaceElemUnique"),
              scoreTest sc (\_ -> replaceVal 3 5 (3:inpArr), (), [5,1,2,3,4,5],1, "ReplaceElemRepeat"),
              scoreTest sc (\_ -> replaceVal 3 5 empArr, (), [],1, "ReplaceElemEmpty"),
              scoreTest sc (\_ -> replaceVal 6 5 inpArr, (), [1,2,3,4,5],1, "ReplaceElemNotFound")
            ] where 
              inpArr :: [Int]
              inpArr = [1,2,3,4,5]
              empArr :: [Int]
              empArr = []

testReorderPlayer :: Score -> TestTree
testReorderPlayer sc = testGroup "Reorder Player"
            [
              scoreTest sc (\_ -> reorderPL nipun allPlayers, (), allPlayers, 1, "FirstElement"),
              scoreTest sc (\_ -> reorderPL anish allPlayers, (), [anish,mahesh,suresh,nipun], 1, "SecondElement"),
              scoreTest sc (\_ -> reorderPL mahesh allPlayers, (), [mahesh,suresh,nipun,anish], 1, "ThirdElement"),
              scoreTest sc (\_ -> reorderPL suresh allPlayers, (), [suresh,nipun,anish,mahesh], 1, "FourthElement"),
              scoreTest sc (\_ -> reorderPL suresh [nipun,anish,mahesh], (), [suresh] , 1, "NoElemFound")
            ]

testWinningCard :: Score -> TestTree
testWinningCard sc = testGroup "Test Winning Card"
            [
              scoreTest sc (\_ -> winningCard Spades [Card Hearts Jack,Card Hearts Three], (), Card Hearts Jack,1, "NormalCase"),
              scoreTest sc (\_ -> winningCard Spades [Card Hearts Jack,Card Spades Three], (), Card Spades Three,1, "TrumpCase"),
              scoreTest sc (\_ -> winningCard Spades [Card Hearts Two,Card Hearts Three,Card Diamonds King], (), Card Hearts Three,1, "NoValidCardCase")
            ]



testCheckHandleWin :: Score -> TestTree
testCheckHandleWin sc = testGroup "Check and Handle Win"
            [
              scoreTest sc (\_ -> checkHandleWin initialGameState, (), initialGameState, 1, "Nobody won"),
              scoreTest sc (\_ -> points $ fst $ teams $ checkHandleWin st, (), 1 , 1,"Team1 Won")
            ]
          where 
            st = execState (do {
              _ <- choseCard (head initialDeck);
              _ <- choseCard (initialDeck!!5) ;
              _ <- choseCard (initialDeck!!10);
              choseCard (initialDeck!!15);
            } ) (cardAssign initialGameState)


main :: IO ()
main = runTests [testAssignCard,testChooseCard,testReplaceVal,testReorderPlayer,testCheckHandleWin,testWinningCard]


