{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Client.InitialClientPage (getClientData) where

  
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
  , setFieldValid
  , allFieldsValid
  , (@@=)
  )
import Brick.Focus
  ( focusRingCursor
  , focusGetCurrent
  )
import Control.Monad.IO.Class
import qualified Graphics.Vty as V
import Objects
import Constants (logoStr)
import System.Exit (exitFailure)
import Lens.Micro
import Utils (isValidIP)


data Name = NameField
          | IPAddressField
          deriving (Eq, Ord, Show)



mkForm :: ClientInfo -> Form ClientInfo e Name
mkForm = let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
         in newForm [ label "Name" @@=  editTextField nameC NameField (Just 1)
                    , label "Server IP: "  @@= editTextField ipC IPAddressField (Just 1) -- TODO: Add validation for IP
                    ]


logo :: Widget Name
logo = C.hCenter $ padTop (Pad 3) $ str $ unlines logoStr

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.yellow `on` V.black)
  , (invalidFormInputAttr, V.red `on` V.black)
  , (focusedFormInputAttr, V.yellow `on` V.black)
  ]



renderMyForm :: Form ClientInfo e Name -> [Widget Name]
renderMyForm form = [ logo <=> (C.hCenter $ C.vCenter $ B.border $ padAll 1 $ hLimit 50 $ renderForm form)]

formApp :: App (Form ClientInfo e Name) e Name
formApp =
    App { appDraw = renderMyForm
        , appHandleEvent = \ev -> do
            f <- gets formFocus
            case ev of
                VtyEvent (V.EvResize {}) -> return ()
                VtyEvent ( V.EvKey  V.KEsc       _) -> liftIO exitFailure
                VtyEvent ( V.EvKey (V.KChar 'q') _) -> liftIO exitFailure
                VtyEvent (V.EvKey V.KEnter []) 
                  | focusGetCurrent f == Just IPAddressField -> halt
                  | otherwise -> return ()
                _ -> do
                    handleFormEvent ev

                    st <- gets formState
                    modify $ setFieldValid (isValidIP (st^.ipC)) IPAddressField

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

getClientData :: IO ClientInfo
getClientData = do
    let buildVty = do
          v <- V.mkVty V.defaultConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = ClientInfo { _nameC = "", _ipC = ""}
        initialForm = setFieldValid False IPAddressField $ mkForm  initialUserInfo
    initialVty <- buildVty
    finalForm <- customMain initialVty buildVty Nothing formApp initialForm
    if allFieldsValid finalForm then return (formState finalForm) else getClientData




