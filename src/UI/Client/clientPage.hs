{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Client.ClientPage (showClientUI) where
import Brick
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
import Control.Monad.IO.Class  

import qualified Data.Text as T

import qualified Graphics.Vty as V

import Lens.Micro.TH

import System.Exit (exitFailure)

import Objects

logo :: Widget ()
logo = C.hCenter $ padTopBottom 1 $ str $ unlines ["░█████╗░░█████╗░██╗░░░██╗██████╗░████████╗  ██████╗░██╗███████╗░█████╗░███████╗",
                                                   "██╔══██╗██╔══██╗██║░░░██║██╔══██╗╚══██╔══╝  ██╔══██╗██║██╔════╝██╔══██╗██╔════╝",
                                                   "██║░░╚═╝██║░░██║██║░░░██║██████╔╝░░░██║░░░  ██████╔╝██║█████╗░░██║░░╚═╝█████╗░░",
                                                   "██║░░██╗██║░░██║██║░░░██║██╔══██╗░░░██║░░░  ██╔═══╝░██║██╔══╝░░██║░░██╗██╔══╝░░",
                                                   "╚█████╔╝╚█████╔╝╚██████╔╝██║░░██║░░░██║░░░  ██║░░░░░██║███████╗╚█████╔╝███████╗",
                                                   "░╚════╝░░╚════╝░░╚═════╝░╚═╝░░╚═╝░░░╚═╝░░░  ╚═╝░░░░░╚═╝╚══════╝░╚════╝░╚══════╝"]


teamUI :: Widget ()
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

roundUI :: Int -> [Card] -> Suit -> Widget ()
roundUI rnd pCards s = hBox [
    hLimitPercent 35 $ B.border $ padAll 3 $ vBox [
        fill ' ',
        C.hCenter $ str $ "Round: " ++ show rnd,
        fill ' ',
        C.hCenter $ hBox $ map (\card -> B.border $ padAll 1 $ str (show card)) pCards,
        fill ' ' ,
        C.hCenter $ str $ "Trump: " ++ show s
    ]]

initialDeck :: [Card]
initialDeck = [Card st val | st <- [Spades .. Diamonds],val <- [Two]]

sampleRoundUI :: Widget()
sampleRoundUI = roundUI 1 initialDeck Spades

showClientUI :: IO ()
showClientUI = do
    putStrLn ("Initial Deck: " ++ (show initialDeck))
    simpleMain ( logo <=> hBox [teamUI, sampleRoundUI])
