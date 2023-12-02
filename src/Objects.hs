
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Objects (
    Suit (..),
    Value (..),
    Card(..),
    Player(..),
    nameP,
    idP,
    Team(..),
    nameT,
    ClientInfo (..),
    name,
    ip,
    Choice (..),
    Gamestate (..),
    PlayerState (..) 
) where


import qualified Data.Text as T
import Lens.Micro.TH

data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq,Enum,Read)
instance Show Suit where
  show Spades   = [toEnum 0x2660] :: String
  show Hearts   = [toEnum 0x2665] :: String
  show Diamonds = [toEnum 0x2666] :: String
  show Clubs    = [toEnum 0x2663] :: String

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Joker | Queen | King | Ace deriving (Ord,Eq,Show,Enum,Read)
data Card = Card {suit:: Suit, value::Value} deriving (Eq,Show,Read)


data Player = Player {_nameP::String, _idP::String, cards::[Card]} deriving (Eq,Show,Read)
data Team = Team {_nameT::String, points::Int, players::(String,String)} deriving (Eq,Show,Read)

makeLenses ''Player
makeLenses ''Team

data Gamestate = Gamestate {remainingCards :: [Card], round::Int, playerOrder::[Player], currentRound::[Card], teams::(Team,Team), trump::Suit} deriving (Show,Read)

data PlayerState = PlayerState {player::Player, currentRoundCard :: [Card], roundNumber::Int, teamInfo::(Team,Team), trumpSuit:: Suit} deriving (Show,Read)



data Choice = HostMode | ClientMode
            deriving Show

data ClientInfo = 
    ClientInfo {
        _name :: T.Text,
        _ip ::  T.Text
    } deriving (Show)

makeLenses ''ClientInfo

