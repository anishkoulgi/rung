{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Client.ClientPage (showClientUI) where
import Brick
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
import Brick.Focus
  ( focusRingCursor
  )
import Brick.Util (on, fg)
import Control.Monad.IO.Class  

import qualified Data.Text as T

import qualified Graphics.Vty as V

import Lens.Micro.TH

import System.Exit (exitFailure)

import Objects

logo :: Widget n
logo = C.hCenter $ padTopBottom 1 $ str $ unlines ["░█████╗░░█████╗░██╗░░░██╗██████╗░████████╗  ██████╗░██╗███████╗░█████╗░███████╗",
                                                   "██╔══██╗██╔══██╗██║░░░██║██╔══██╗╚══██╔══╝  ██╔══██╗██║██╔════╝██╔══██╗██╔════╝",
                                                   "██║░░╚═╝██║░░██║██║░░░██║██████╔╝░░░██║░░░  ██████╔╝██║█████╗░░██║░░╚═╝█████╗░░",
                                                   "██║░░██╗██║░░██║██║░░░██║██╔══██╗░░░██║░░░  ██╔═══╝░██║██╔══╝░░██║░░██╗██╔══╝░░",
                                                   "╚█████╔╝╚█████╔╝╚██████╔╝██║░░██║░░░██║░░░  ██║░░░░░██║███████╗╚█████╔╝███████╗",
                                                   "░╚════╝░░╚════╝░░╚═════╝░╚═╝░░╚═╝░░░╚═╝░░░  ╚═╝░░░░░╚═╝╚══════╝░╚════╝░╚══════╝"]


teamUI :: Widget n
teamUI = hBox [
    hLimitPercent 25 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        padBottom (Pad 1) $ strWrap "Team 1" <+> strWrap "Points: X",
        str "Member 1", 
        str "Member 2",
        fill ' ',
        padBottom (Pad 1) $ strWrap "Team 2" <+> strWrap "Points: X",
         str "Member 1",
         str "Member 2",
         fill ' ']]

roundUI :: Int -> [Card] -> Suit -> Widget n
roundUI rnd pCards s = hBox [
    hLimitPercent 35 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        C.hCenter $ str $ "Round: " ++ show rnd,
        fill ' ',
        C.hCenter $ hBox $ map renderCard pCards,
        fill ' ' ,
        C.hCenter $ str $ "Trump: " ++ show s
    ]]

getCardSuitAttr :: Card -> String
getCardSuitAttr card = case (suit card) of
    Diamonds -> "redCard"
    Hearts   -> "redCard"
    Spades   -> "blackCard"
    _        -> "blackCard"

renderCard :: Card -> Widget n
renderCard card = withAttr (attrName (getCardSuitAttr card))  $ B.border $ padAll 1 $ str (show card)

initialDeck :: [Card]
initialDeck = [Card st val | st <- [Spades .. Diamonds],val <- [Two]]

sampleRoundUI :: Widget n
sampleRoundUI = roundUI 1 initialDeck Spades

globalDefault :: V.Attr
globalDefault = V.white `on` V.black

theMap :: AttrMap
theMap = attrMap globalDefault
    [ (attrName "blackCard", V.black `on` V.white)
    , (attrName "redCard",   V.red `on` V.white)
    ]   

ui :: Widget n
ui = logo <=> hBox [teamUI, sampleRoundUI]

app :: App () e ()
app =
    App { appDraw =  const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return ()
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

showClientUI :: IO ()
showClientUI = do
    defaultMain app ()
