
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
    cardsP,
    Team(..),
    nameT,
    playersT,
    pointsT,
    ClientInfo (..),
    nameC,
    ipC,
    Choice (..),
    Gamestate (..),
    PlayerState (..) 
) where


import qualified Data.Text as T
import Lens.Micro.TH

data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq,Enum, Show, Read)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord,Eq,Enum,Read, Show)

data Card = Card {suit:: Suit, value::Value} deriving (Eq,Read,Show)

data Player = Player {_nameP::String, _idP::String, _cardsP::[Card]} deriving (Eq,Show,Read)
data Team = Team {_nameT::String, _pointsT::Int, _playersT::(String,String)} deriving (Eq,Show,Read)

makeLenses ''Player
makeLenses ''Team

data Gamestate = Gamestate {remainingCards :: [Card], round::Int, playerOrder::[Player], currentRound::[Card], teams::(Team,Team), trump::Suit} deriving (Show,Read, Eq)

data PlayerState = PlayerState {player::Player, currentRoundCard :: [Card], roundNumber::Int, teamInfo::(Team,Team), trumpSuit:: Suit, currentPlayer :: Bool} deriving (Show,Read)

data Choice = HostMode | ClientMode
            deriving Show

data ClientInfo = 
    ClientInfo {
        _nameC :: T.Text,
        _ipC ::  T.Text
    } deriving (Show)

makeLenses ''ClientInfo

