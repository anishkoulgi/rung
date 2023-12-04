{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Client.ClientPage (showClientUI) where

import Brick
import Brick.Main (halt)
import Brick.AttrMap (attrMap, AttrMap, attrName)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , renderForm
  , handleFormEvent
  , focusedFormInputAttr
  , invalidFormInputAttr
  , editTextField
  , (@@=)
  )
import Brick.Focus as F
import Brick.Types as BT
import Brick.Util (on, fg)
import Control.Monad.IO.Class  
import qualified Graphics.Vty as V

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import System.Exit (exitFailure)

import Objects

data PlayerStateUI = 
    PlayerStateUI {
        _psUI :: PlayerState,
        _focusRingUI :: F.FocusRing Card
    }

makeLenses ''PlayerStateUI

logo :: Widget n
logo = C.hCenter $ padTopBottom 1 $ str $ unlines ["░█████╗░░█████╗░██╗░░░██╗██████╗░████████╗  ██████╗░██╗███████╗░█████╗░███████╗",
                                                   "██╔══██╗██╔══██╗██║░░░██║██╔══██╗╚══██╔══╝  ██╔══██╗██║██╔════╝██╔══██╗██╔════╝",
                                                   "██║░░╚═╝██║░░██║██║░░░██║██████╔╝░░░██║░░░  ██████╔╝██║█████╗░░██║░░╚═╝█████╗░░",
                                                   "██║░░██╗██║░░██║██║░░░██║██╔══██╗░░░██║░░░  ██╔═══╝░██║██╔══╝░░██║░░██╗██╔══╝░░",
                                                   "╚█████╔╝╚█████╔╝╚██████╔╝██║░░██║░░░██║░░░  ██║░░░░░██║███████╗╚█████╔╝███████╗",
                                                   "░╚════╝░░╚════╝░░╚═════╝░╚═╝░░╚═╝░░░╚═╝░░░  ╚═╝░░░░░╚═╝╚══════╝░╚════╝░╚══════╝"]


playerUI :: [Card] -> Widget n
playerUI cards = hBox[
    hLimitPercent 40 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        C.hCenter $ hBox $ map renderCard cards,
        fill ' '
    ]]

teamUI :: (Team, Team) -> Widget n
teamUI (team1, team2) = hBox [
    hLimitPercent 25 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        padBottom (Pad 1) $ strWrap "Team 1" <+> strWrap ("Points: " ++ show(team1^.pointsT)),
        str $ fst team1Players, 
        str $ snd team1Players,
        fill ' ',
        padBottom (Pad 1) $ strWrap "Team 2" <+> strWrap ("Points: " ++ show(team2^.pointsT)),
         str $ fst team2Players,
         str $ snd team2Players,
         fill ' ']]
    where
        team1Players = team1^.playersT
        team2Players = team2^.playersT

roundUI :: Int -> [Card] -> Suit -> Widget n
roundUI rnd pCards s = hBox [
    hLimitPercent 35 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        C.hCenter $ str $ "Round: " ++ show rnd,
        fill ' ',
        C.hCenter $ hBox $ map renderCard pCards,
        fill ' ' ,
        C.hCenter $ str  "Trump: " <+>  renderSuit s
    ]]
    where 
        renderSuit Spades = withAttr (attrName "blackOnWhite") $ B.border $ padTopBottom 3 $ padLeftRight 2  $ str ([toEnum 0x2660] :: String)
        renderSuit Diamonds = withAttr (attrName "redOnWhite") $ B.border $ padTopBottom 3 $ padLeftRight 2  $ str ([toEnum 0x2666] :: String)
        renderSuit Hearts = withAttr (attrName "redOnWhite") $ B.border $ padTopBottom 3 $ padLeftRight 2  $ str ([toEnum 0x2665] :: String)
        renderSuit _ = withAttr (attrName "blackOnWhite") $ B.border $ padTopBottom 3 $ padLeftRight 2  $ str ([toEnum 0x2663] :: String)

renderCard :: Card -> Widget n
renderCard card = withAttr (attrName getCardAttr)  $ B.border $ padTopBottom 3 $ padLeftRight 2  $ str (getCardNumber (cardVal) ++ getSuitUnicode(cardSuit))
    where
        cardSuit = suit card
        cardVal  = value card 
        getCardAttr = case cardSuit of
                            Diamonds -> "redOnWhite"
                            Hearts   -> "redOnWhite"
                            Spades   -> "blackOnWhite"
                            _        -> "blackOnWhite"
        getSuitUnicode Spades   = [toEnum 0x2660] :: String
        getSuitUnicode Diamonds = [toEnum 0x2666] :: String
        getSuitUnicode Hearts   = [toEnum 0x2665] :: String
        getSuitUnicode _        = [toEnum 0x2663] :: String

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

        


theMap :: AttrMap
theMap = attrMap globalDefault
    [ (attrName "blackOnWhite", V.black `on` V.white)
    , (attrName "redOnWhite",   V.red `on` V.white)
    ]   
    where globalDefault = V.white `on` V.black

renderUI :: PlayerStateUI -> [Widget n]
renderUI playerStateUI = [ui]
    where
        playerState    = playerStateUI^.psUI
        ui             = (logo <=> (C.hCenter $ hBox[ currentTeamUI, currentRoundUI, playerUI (focusRingToList(playerStateUI^.focusRingUI))]))
        currentRoundUI = roundUI (roundNumber playerState) (currentRoundCard playerState) (trumpSuit playerState)
        currentTeamUI  = teamUI (teamInfo playerState)

appEvent :: BrickEvent () e -> EventM () PlayerStateUI ()
appEvent (BT.VtyEvent (V.EvKey V.KEnter [])) = halt
appEvent (BT.VtyEvent (V.EvKey V.KEsc []))   = liftIO exitFailure
appEvent (BT.VtyEvent( V.EvKey V.KLeft [] )) = focusRingUI %= F.focusNext
appEvent (BT.VtyEvent( V.EvKey V.KRight [] )) = focusRingUI %= F.focusPrev
appEvent _ = return()

app :: App PlayerStateUI e ()
app  =
    App { appDraw =  renderUI
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

showClientUI :: PlayerState -> IO (PlayerState)
showClientUI playerState = do
    let initialPlayerStateUI = PlayerStateUI playerState (F.focusRing $ (player playerState)^.cardsP)
    putStrLn ("Current focus: " ++ (show $ F.focusGetCurrent $ initialPlayerStateUI^.focusRingUI))
    finalState <- (defaultMain app initialPlayerStateUI)
    return $ finalState^.psUI