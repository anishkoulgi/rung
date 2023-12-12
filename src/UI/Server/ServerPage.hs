{-# LANGUAGE TemplateHaskell #-}
module UI.Server.ServerPage (serverUI) where

import qualified Brick.AttrMap as A
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
import Brick.BChan
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V
import Brick.Widgets.Core
import Control.Concurrent.MVar (readMVar, MVar)
import Control.Monad (void)
import Brick.Util (fg, on)
import Objects
import Constants (logoStr)
import System.Exit (exitFailure)
import Lens.Micro.TH
import Lens.Micro
import Lens.Micro.Mtl
import Control.Monad.IO.Class

data UIServerState = UIServerState {_serverState :: ServerState}
makeLenses ''UIServerState

logo :: T.Widget n
logo = vLimit 8 $ C.hCenter $ C.vCenter $ padTopBottom 1 $ str s
    where s = unlines logoStr

app :: App  UIServerState ServerState()
app  =
    App { appDraw =  renderUI
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap =  const theMap
        , appChooseCursor = neverShowCursor
        }
renderUI :: UIServerState -> [T.Widget n]
renderUI uiServerState = [logo <=> (C.hCenter $ C.vCenter $ B.border $ padAll 1 $ vBox $ map routine [0..3])]
    where
        state = (uiServerState^.serverState)
        clnts = clients state
        len = length clnts
        routine idx = str ("Player " ++ show(idx+1) ++ ": ") <+> (if len > 0 && idx <= (len - 1) then
                        withAttr(A.attrName "brightGreenOnBlack") $ str $ fst $ clnts !! idx
                      else
                        withAttr(A.attrName "brightRedOnBlack") $ str "Waiting...")

appEvent :: T.BrickEvent () ServerState -> T.EventM () UIServerState ()
appEvent (T.AppEvent newServerState)            = serverState .= newServerState
appEvent (T.VtyEvent(V.EvKey V.KEsc   []))      = liftIO exitFailure
appEvent (T.VtyEvent(V.EvKey (V.KChar 'q') [])) = liftIO exitFailure
appEvent _                                      = return ()

            



theMap :: A.AttrMap
theMap = A.attrMap (V.black `on` V.white) []

serverUI :: BChan ServerState -> IO ()
serverUI serverStateBChan= do 
    putStrLn "In serverpage"
    let initialUIServerState = UIServerState (ServerState [] (Gamestate [] 0 [] [] (Team "" 0 ("",""), Team "" 0 ("","")) Spades) False)
    let builder = V.mkVty V.defaultConfig
    vty <- builder
    (_finalState, _finalVty) <- (customMainWithVty vty builder (Just serverStateBChan) app initialUIServerState)
    return ()
