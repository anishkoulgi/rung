module UI.GameMode (selectionMain) where
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D

import Control.Monad.IO.Class

import qualified Graphics.Vty as V

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import System.Exit (exitFailure)


import UI.InitialClientPage (getClientData)

data Choice = HostMode | ClientMode
            deriving Show

data Name = Host | Client
    deriving (Show, Eq, Ord)

drawUI :: D.Dialog Choice Name -> [Widget Name]
drawUI d = [D.renderDialog d $ C.hCenter $ padAll 1 $ str "Select the mode"]

handleEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
handleEvent (VtyEvent ev) =
    case ev of
        V.EvKey  V.KEnter     _ -> M.halt
        V.EvKey  V.KEsc       _ -> liftIO exitFailure
        V.EvKey (V.KChar 'q') _ -> liftIO exitFailure
        _                       -> D.handleDialogEvent ev
handleEvent _ = return ()

initialState :: D.Dialog Choice Name
initialState = D.dialog (Just $ str "Court Piece") (Just (Host, choices)) 50
    where
        choices = [ ("Host a game",   Host,   HostMode)
                  , ("Join a game",  Client,  ClientMode)
                  ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.black)
    , (D.buttonAttr, V.blue `on` V.black)
    , (D.buttonSelectedAttr, bg V.green)
    ]

modeSelectionApp :: M.App (D.Dialog Choice Name) e Name
modeSelectionApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = handleEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

selectionMain :: IO ()
selectionMain = do
    d <- M.defaultMain modeSelectionApp initialState
    case D.dialogSelection d of
        Just (Client, ClientMode) -> getClientData
        Just (_, _) -> putStrLn "Call Server"
        Nothing -> putStrLn "Never called"
