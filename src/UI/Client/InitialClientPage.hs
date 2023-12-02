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
  , (@@=)
  )
import Brick.Focus
  ( focusRingCursor
  )
import Control.Monad.IO.Class  
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.TH
import Objects
import System.Exit (exitFailure)


data Name = NameField
          | IPAddressField
          deriving (Eq, Ord, Show)



mkForm :: ClientInfo -> Form ClientInfo e Name
mkForm = let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
         in newForm [ label "Name" @@=  editTextField name NameField (Just 1)
                    , label "Server IP: "  @@= editTextField ip IPAddressField (Just 1) -- TODO: Add validation for IP
                    ]


logo :: Widget Name
logo = str $ unlines ["░█████╗░░█████╗░██╗░░░██╗██████╗░████████╗  ██████╗░██╗███████╗░█████╗░███████╗",
                      "██╔══██╗██╔══██╗██║░░░██║██╔══██╗╚══██╔══╝  ██╔══██╗██║██╔════╝██╔══██╗██╔════╝",
                      "██║░░╚═╝██║░░██║██║░░░██║██████╔╝░░░██║░░░  ██████╔╝██║█████╗░░██║░░╚═╝█████╗░░",
                      "██║░░██╗██║░░██║██║░░░██║██╔══██╗░░░██║░░░  ██╔═══╝░██║██╔══╝░░██║░░██╗██╔══╝░░",
                      "╚█████╔╝╚█████╔╝╚██████╔╝██║░░██║░░░██║░░░  ██║░░░░░██║███████╗╚█████╔╝███████╗",
                      "░╚════╝░░╚════╝░░╚═════╝░╚═╝░░╚═╝░░░╚═╝░░░  ╚═╝░░░░░╚═╝╚══════╝░╚════╝░╚══════╝"]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.yellow `on` V.black)
  , (invalidFormInputAttr, V.red `on` V.black)
  , (focusedFormInputAttr, V.yellow `on` V.black)
  ]



renderMyForm :: Form ClientInfo e Name -> [Widget Name]
renderMyForm form = [(C.hCenter $ padTop (Pad 3) $ logo) <=> (C.hCenter $ C.vCenter $ B.border $ padAll 1 $ hLimit 50 $ renderForm form)]

formApp :: App (Form ClientInfo e Name) e Name
formApp =
    App { appDraw = renderMyForm
        , appHandleEvent = \ev -> do
            f <- gets formFocus
            case ev of
                VtyEvent (V.EvResize {}) -> return ()
                VtyEvent ( V.EvKey  V.KEsc       _) -> liftIO exitFailure
                VtyEvent ( V.EvKey (V.KChar 'q') _) -> liftIO exitFailure
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter []) -> halt
                _ -> do
                    handleFormEvent ev

                    -- Example of external validation:
                    -- Require age field to contain a value that is at least 18.
                    -- st <- gets formState
                    -- modify $ setFieldValid (st^.age >= 18) AgeField

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

getClientData :: IO (ClientInfo)
getClientData = do
    let buildVty = do
          v <- V.mkVty V.defaultConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = ClientInfo { _name = ""
                                   , _ip = ""
                                   }
        f = mkForm initialUserInfo
    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing formApp f

    return (formState f')


