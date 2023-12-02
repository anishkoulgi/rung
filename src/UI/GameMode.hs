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


import qualified Objects as O

data Name = Host | Client
    deriving (Show, Eq, Ord)

drawUI :: D.Dialog O.Choice Name -> [Widget Name]
drawUI d = [D.renderDialog d $ C.hCenter $ padAll 1 $ str "Select the mode"]

handleEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog O.Choice Name) ()
handleEvent (VtyEvent ev) =
    case ev of
        V.EvKey  V.KEnter     _ -> M.halt
        V.EvKey  V.KEsc       _ -> liftIO exitFailure
        V.EvKey (V.KChar 'q') _ -> liftIO exitFailure
        _                       -> D.handleDialogEvent ev
handleEvent _ = return ()

initialState :: D.Dialog O.Choice Name
initialState = D.dialog (Just $ str "Court Piece") (Just (Host, choices)) 50
    where
        choices = [ ("Host a game",   Host,   O.HostMode)
                  , ("Join a game",  Client,  O.ClientMode)
                  ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.black)
    , (D.buttonAttr, V.blue `on` V.black)
    , (D.buttonSelectedAttr, bg V.green)
    ]

modeSelectionApp :: M.App (D.Dialog O.Choice Name) e Name
modeSelectionApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = handleEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

selectionMain :: IO (O.Choice)
selectionMain = do
    d <- M.defaultMain modeSelectionApp initialState
    return (case D.dialogSelection d of
        Just (Client, O.ClientMode) -> O.ClientMode
        Just (_, _) -> O.HostMode
        Nothing -> O.ClientMode)
