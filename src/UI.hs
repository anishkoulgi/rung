module UI (uiMain) where

-- import Data.Monoid
-- import qualified Graphics.Vty as V

-- import qualified Brick.Main as M
-- import Brick.Types
--   ( Widget
--   , BrickEvent(..)
--   )
-- import Brick.Widgets.Core
--   ( padAll
--   , str
--   )
-- import qualified Brick.Widgets.Dialog as D
-- import qualified Brick.Widgets.Center as C
-- import qualified Brick.AttrMap as A
-- import Brick.Util (on, bg)
-- import qualified Brick.Types as T

-- data Choice = Red | Blue | Green
--             deriving Show

-- data Name =
--     RedButton
--     | BlueButton
--     | GreenButton
--     deriving (Show, Eq, Ord)

-- drawUI :: D.Dialog Choice Name -> [Widget Name]
-- drawUI d = [ui]
--     where
--         ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the dialog body."

-- appEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
-- appEvent (VtyEvent ev) =
--     case ev of
--         V.EvKey V.KEsc [] -> M.halt
--         V.EvKey V.KEnter [] -> M.halt
--         _ -> D.handleDialogEvent ev
-- appEvent _ = return ()

-- initialState :: D.Dialog Choice Name
-- initialState = D.dialog (Just $ str "Title") (Just (RedButton, choices)) 50
--     where
--         choices = [ ("Red",   RedButton,   Red)
--                   , ("Blue",  BlueButton,  Blue)
--                   , ("Green", GreenButton, Green)
--                   ]

-- theMap :: A.AttrMap
-- theMap = A.attrMap V.defAttr
--     [ (D.dialogAttr, V.white `on` V.blue)
--     , (D.buttonAttr, V.black `on` V.white)
--     , (D.buttonSelectedAttr, bg V.yellow)
--     ]

-- theApp :: M.App (D.Dialog Choice Name) e Name
-- theApp =
--     M.App { M.appDraw = drawUI
--           , M.appChooseCursor = M.showFirstCursor
--           , M.appHandleEvent = appEvent
--           , M.appStartEvent = return ()
--           , M.appAttrMap = const theMap
--           }

-- uiMain :: IO ()
-- uiMain = do
--     d <- M.defaultMain theApp initialState
--     putStrLn $ "You chose: " <> show (D.dialogSelection d)


import Game

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

type ResourceName = String

courtPiece :: IO()
courtPiece = do
  initialState <- UIInitialState 
  endState <- defaultMain courtPieceUIApp initialState
  print endState
 

data UIGameState =
  UIInitialState | 
  UIGameState {
    inPlayCards :: [Game.Card],
    userCards :: [Game.Card]
  }
  deriving (Show, Eq)

renderUI :: UIGameState -> [Widget ResourceName]
renderUI uiState = case uiState of
  UIInitialState -> [vBox]

courtPieceUIApp :: App UIGameState e ResourceName
courtPieceUIApp =
  App {
    appDraw = drawUI,
    appChooseCursor = showFirstCursor,
    appHandleEvent = handleEvent,
    appStartEvent = pure,
    appAttrMap = const $ attrMap mempty []
  }


