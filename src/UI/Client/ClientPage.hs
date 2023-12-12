{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module UI.Client.ClientPage (showClientUI) where

import Brick.Main
    ( appDraw
    , appHandleEvent
    , appStartEvent
    , appAttrMap
    , appChooseCursor
    , App(..)
    , neverShowCursor
    , customMainWithVty
    )
import Brick.AttrMap (attrMap, AttrMap, attrName)
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import  Brick.Widgets.Core 
    ( hLimitPercent
    , Padding(..)
    , padBottom
    , padAll
    , padTopBottom
    , padLeftRight
    , str
    , fill
    , strWrap
    , hBox
    , vBox
    , (<+>)
    , (<=>)
    , withAttr
    , vLimit
    )
import Brick.Types as BT
import Brick.Util (on)
import Control.Monad.IO.Class
import Control.Concurrent (MVar, putMVar)
import qualified Graphics.Vty as V

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import System.Exit (exitFailure)

import Objects
import Utils (mapWithIndex, getCardNumber,getSuitUnicode,getSuitColor)

data PlayerStateUI =
    PlayerStateUI {
        _curPlayerState :: PlayerState,
        _idx :: Int,
        _clientMVar :: MVar Card
    }

makeLenses ''PlayerStateUI

logo :: Widget n
logo = vLimit 8 $ C.hCenter $ C.vCenter $ padTopBottom 1 $ str s
    where s = unlines [ "░█████╗░░█████╗░██╗░░░██╗██████╗░████████╗  ██████╗░██╗███████╗░█████╗░███████╗",
                        "██╔══██╗██╔══██╗██║░░░██║██╔══██╗╚══██╔══╝  ██╔══██╗██║██╔════╝██╔══██╗██╔════╝",
                        "██║░░╚═╝██║░░██║██║░░░██║██████╔╝░░░██║░░░  ██████╔╝██║█████╗░░██║░░╚═╝█████╗░░",
                        "██║░░██╗██║░░██║██║░░░██║██╔══██╗░░░██║░░░  ██╔═══╝░██║██╔══╝░░██║░░██╗██╔══╝░░",
                        "╚█████╔╝╚█████╔╝╚██████╔╝██║░░██║░░░██║░░░  ██║░░░░░██║███████╗╚█████╔╝███████╗",
                        "░╚════╝░░╚════╝░░╚═════╝░╚═╝░░╚═╝░░░╚═╝░░░  ╚═╝░░░░░╚═╝╚══════╝░╚════╝░╚══════╝"]


team1Won :: Widget n
team1Won = C.hCenter $ C.vCenter $ padTopBottom 1 $ withAttr(attrName "cyanOnBlack") $ str s
    where s = unlines [ "███████████                                       ████     █████   ███   █████                        ███ ███ ███",
                        "░█░░░███░░░█                                      ░░███    ░░███   ░███  ░░███                        ░███░███░███",
                        "░   ░███  ░   ██████   ██████   █████████████      ░███     ░███   ░███   ░███   ██████  ████████     ░███░███░███",
                        "    ░███     ███░░███ ░░░░░███ ░░███░░███░░███     ░███     ░███   ░███   ░███  ███░░███░░███░░███    ░███░███░███",
                        "    ░███    ░███████   ███████  ░███ ░███ ░███     ░███     ░░███  █████  ███  ░███ ░███ ░███ ░███    ░███░███░███",
                        "    ░███    ░███░░░   ███░░███  ░███ ░███ ░███     ░███      ░░░█████░█████░   ░███ ░███ ░███ ░███    ░░░ ░░░ ░░░ ",
                        "    █████   ░░██████ ░░████████ █████░███ █████    █████       ░░███ ░░███     ░░██████  ████ █████    ███ ███ ███",
                        "       ░░░░░     ░░░░░░   ░░░░░░░░ ░░░░░ ░░░ ░░░░░    ░░░░░         ░░░   ░░░       ░░░░░░  ░░░░ ░░░░░    ░░░ ░░░ ░░░" ]

team2Won :: Widget n
team2Won = C.hCenter $ padTopBottom 1 $ withAttr(attrName "brightRedOnBlack") $ str s
    where s = unlines [ "███████████                                        ████████     █████   ███   █████                        ███ ███ ███",
                        "░█░░░███░░░█                                       ███░░░░███   ░░███   ░███  ░░███                        ░███░███░███",
                        "░   ░███  ░   ██████   ██████   █████████████     ░░░    ░███    ░███   ░███   ░███   ██████  ████████     ░███░███░███",
                        "    ░███     ███░░███ ░░░░░███ ░░███░░███░░███       ███████     ░███   ░███   ░███  ███░░███░░███░░███    ░███░███░███",
                        "    ░███    ░███████   ███████  ░███ ░███ ░███      ███░░░░      ░░███  █████  ███  ░███ ░███ ░███ ░███    ░███░███░███",
                        "    ░███    ░███░░░   ███░░███  ░███ ░███ ░███     ███      █     ░░░█████░█████░   ░███ ░███ ░███ ░███    ░░░ ░░░ ░░░ ",
                        "    █████   ░░██████ ░░████████ █████░███ █████   ░██████████       ░░███ ░░███     ░░██████  ████ █████    ███ ███ ███",
                        "   ░░░░░     ░░░░░░   ░░░░░░░░ ░░░░░ ░░░ ░░░░░    ░░░░░░░░░░         ░░░   ░░░       ░░░░░░  ░░░░ ░░░░░    ░░░ ░░░ ░░░]"]

playerUI :: [Card] -> Int -> Widget n
playerUI cards index = hBox [
    hLimitPercent 90 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        C.hCenter $ hBox $ mapWithIndex(\(currentIdx, card) -> renderCard card (currentIdx == index `mod` length cards))cards,
        fill ' '
    ]]

teamUI :: (Team, Team) -> Widget n
teamUI (team1, team2) = hBox [
    hLimitPercent 25 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        padBottom (Pad 1) $ strWrap "Team 1" <+> strWrap ("Points: " ++ show (team1^.pointsT)),
        str $ fst team1Players,
        str $ snd team1Players,
        fill ' ',
        padBottom (Pad 1) $ strWrap "Team 2" <+> strWrap ("Points: " ++ show (team2^.pointsT)),
         str $ fst team2Players,
         str $ snd team2Players,
         fill ' ']]
    where
        team1Players = team1^.playersT
        team2Players = team2^.playersT

roundUI :: PlayerState -> Widget n
roundUI playerState = hBox [
    hLimitPercent 40 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        C.hCenter $ str rnd,
        fill ' ',
        C.hCenter $ str "Current Player: " <+> currentPlayerN,
        fill ' ',
        C.hCenter $ hBox $ map (\card -> renderCard card False) rndCards,
        fill ' ' ,
        C.hCenter $ str  "Trump: " <+>  renderSuit ts
    ]]
    where
        rnd            = "Round: " ++ show (playerState^.roundNumPS)
        rndCards       = playerState^.curRndCardsPS
        ts             = playerState^.trumpPS
        currentPlayerN = if playerState^.isTurnPS then 
                            withAttr(attrName "greenOnBlack") $ str "YOU"
                         else
                            str (playerState^.curPlrNamePS)
        renderSuit s   = withAttr (attrName attrStr) $ B.border $ padTopBottom 3 $ padLeftRight 2  $ str (enumStr)
            where
                (attrStr, enumStr) = ((getSuitColor s) ++ "OnWhite", getSuitUnicode s)


renderCard :: Card -> Bool-> Widget n
renderCard card flag= withAttr (attrName getCardAttr)  $ B.border  $ padTopBottom 3 $ padLeftRight 2  $ str (getCardNumber (cardVal) ++ getSuitUnicode (cardSuit))
    where
        cardSuit = suit card
        cardVal  = value card
        getCardAttr = (if flag then "green" else (getSuitColor cardSuit)) ++ "OnWhite"


renderGameUI :: PlayerStateUI -> [Widget n]
renderGameUI playerStateUI = [ui]
    where
        playerState    = playerStateUI^.curPlayerState
        ui             = logo <=> C.hCenter (hBox [ currentTeamUI, currentRoundUI, currentPlayerUI])
        currentRoundUI = roundUI playerState
        currentTeamUI  = teamUI (playerState^.teamsPS)
        currentPlayerUI = playerUI (playerStateUI^.curPlayerState.playerPS.cardsP) (playerStateUI^.idx)

renderWinUI :: Int -> [Widget n]
renderWinUI i
    | i == 1    = [team1Won]
    | otherwise = [team2Won]

app :: App PlayerStateUI PlayerState ()
app  =
    App { appDraw =  renderUI
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

renderUI :: PlayerStateUI -> [Widget n]
renderUI playerStateUI = if isOver then renderWinUI winner else renderGameUI playerStateUI
    where
        playerState = playerStateUI^.curPlayerState
        tm = playerState^.teamsPS
        (isOver, winner) = fn tm
        fn (Team _ 7 _ ,  _)         = (True,1)   -- Team 1 has 7 points
        fn (         _ , Team _ 7 _) = (True,2)   -- Team 2 has 7 points
        fn  _                        = (False,-1) -- Ongoing game

appEvent :: BrickEvent () PlayerState -> EventM () PlayerStateUI ()
appEvent (AppEvent newPlayerState)         = curPlayerState .= newPlayerState
appEvent (BT.VtyEvent(V.EvKey V.KEsc   [])) = liftIO exitFailure
appEvent (BT.VtyEvent(V.EvKey V.KLeft  [])) = updateIdx (-)
appEvent (BT.VtyEvent(V.EvKey V.KRight [])) = updateIdx (+)
appEvent (BT.VtyEvent(V.EvKey V.KEnter [])) = selectCard
    where
        selectCard :: EventM () PlayerStateUI ()
        selectCard = do
            currentPlayerStateUI <- get
            let playerCards = currentPlayerStateUI^.curPlayerState.playerPS.cardsP
            let numCards = length playerCards
            _ <- liftIO $ putMVar (currentPlayerStateUI^.clientMVar) (playerCards !! ((currentPlayerStateUI^.idx) `mod` numCards)) 
            return ()
appEvent _ = return ()

updateIdx :: (Int -> Int -> Int) -> EventM () PlayerStateUI ()
updateIdx op = do
    currentPlayerStateUI <- get
    let currentIdx = currentPlayerStateUI^.idx
    let cps = currentPlayerStateUI^.curPlayerState
    let isTurn = cps^.isTurnPS
    let numCards = length (cps^.playerPS.cardsP)
    let newIdx = if isTurn then (currentIdx `op` 1) `mod` numCards else currentIdx
    idx .= newIdx
    return ()

theMap :: AttrMap
theMap = attrMap globalDefault
    [ (attrName "blackOnWhite", V.black `on` V.white)
    , (attrName "redOnWhite",   V.red `on` V.white)
    , (attrName "greenOnWhite", V.green `on` V.white)
    , (attrName "greenOnBlack", V.green `on` V.black)
    , (attrName "cyanOnBlack", V.cyan `on` V.black)
    , (attrName "brightRedOnBlack", V.brightRed `on` V.black)
    ]
    where globalDefault = V.white `on` V.black

showClientUI ::  BChan PlayerState -> MVar Card -> IO ()
showClientUI playerStateBChan cardStateMVar = do
    let initialPlayerStateUI = PlayerStateUI (PlayerState (Player "" "" []) [] 0 (Team "" 0 ("",""), Team "" 0 ("","")) Spades False "") 0 cardStateMVar
    let builder = V.mkVty V.defaultConfig 
    vty <- builder
    (_finalState, _finalVty) <- (customMainWithVty vty builder (Just playerStateBChan) app initialPlayerStateUI)
    return ()