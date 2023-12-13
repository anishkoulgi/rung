module UI.Rules (rulesMain) where


import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Core as CO
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V
import Control.Monad (void)
import Brick.Util (fg)


data Name = Logo
          | RulesVP
          deriving(Ord, Show, Eq)


logo :: T.Widget Name
logo = CO.vLimit 10 $ CO.padTopBottom 1 $ C.hCenter $ CO.str s
    where s = unlines [ " ███████████   █████  █████ █████       ██████████  █████████",
                       "░░███░░░░░███ ░░███  ░░███ ░░███       ░░███░░░░░█ ███░░░░░███",
                       " ░███    ░███  ░███   ░███  ░███        ░███  █ ░ ░███    ░░░",
                       " ░██████████   ░███   ░███  ░███        ░██████   ░░█████████",
                       " ░███░░░░░███  ░███   ░███  ░███        ░███░░█    ░░░░░░░░███",
                       " ░███    ░███  ░███   ░███  ░███      █ ░███ ░   █ ███    ░███",
                       " █████   █████ ░░████████   ███████████ ██████████░░█████████",
                       "░░░░░   ░░░░░   ░░░░░░░░   ░░░░░░░░░░░ ░░░░░░░░░░  ░░░░░░░░░"]

drawUi :: () -> [T.Widget Name]
drawUi = const [ui]
    where
        ui =   CO.vBox [logo, CO.fill ' ', C.hCenter $ CO.hLimit 100 $ CO.vLimit 30 rules, CO.fill ' ']
        rules = B.border $ CO.vBox[C.hCenter $ CO.str "Press arrow keys to scroll this page Press Enter or q to Exit.", B.hBorder,CO.viewport RulesVP T.Vertical $ CO.vBox [
            CO.withAttr (A.attrName "underline") (CO.str "Players:") CO.<+> CO.str " 4 players in 2 teams (partners sit opposite each other).",
            CO.str "\n",
            CO.withAttr (A.attrName "underline") (CO.str "Cards:") CO.<+> CO.str " Standard 52-card deck with jokers excluded.",
            CO.str "\n",
            CO.withAttr (A.attrName "underline") (CO.str "Objective:") CO.<+> CO.str " The team that scores 7 points first wins.",
            CO.str "\n",
            CO.withAttr (A.attrName "underline") (CO.str "Gameplay:") CO.<+> CO.str " Standard 52-card deck with jokers excluded",
            CO.str "\n",
            CO.withAttr (A.attrName "underline") (CO.str "Rounds:") CO.<+> CO.str " The game lasts for a maximum of 4 rounds",
            CO.str "\n",
            CO.withAttr (A.attrName "underline") (CO.str "Deal:") CO.<+> CO.str " Each player receives 5 cards in the first round, 3 cards in the second",
            CO.str "\n",
            (CO.str "     ") CO.<+> CO.str " 3 cards in the third round and finally 2 cards in the last round.",
            CO.str "\n",
            CO.withAttr (A.attrName "underline") (CO.str "Trump Suit:") CO.<+> CO.str "The server chooses a random suit to be the trump for the game",
            CO.str "\n",
            CO.str "\n",
            CO.withAttr (A.attrName "underline") (CO.str "Trick Play: "),
            CO.str "\n",
            CO.str "\n",
            CO.withAttr (A.attrName "bold") (CO.str "    1. ") CO.<+> CO.str "Players take turns playing one card each to the trick.",
            CO.str "       The first card played determines the suit for the trick.",
            CO.str "\n",
            CO.withAttr (A.attrName "bold") (CO.str "    2. ") CO.<+> CO.str "Successive players must follow suit if possible.",
            (CO.str "       ") CO.<+> CO.str "If not, they can play any card, including a trump.",
            CO.str "\n",
            CO.withAttr (A.attrName "bold") (CO.str "    3. ") CO.<+> CO.str "The highest card of the leading suit (or the highest trump if no cards of",
            (CO.str "       ") CO.<+> CO.str "the leading suit are played) wins the trick.",
            CO.str "\n",
            CO.withAttr (A.attrName "bold") (CO.str "    4. ") CO.<+> CO.str "The winner of the trick leads the next trick",
            (CO.str "       ") CO.<+> CO.str "and the points of their time is incremented by 1"]]


appEvent :: T.BrickEvent Name e -> T.EventM Name () ()
appEvent (T.VtyEvent (V.EvKey V.KDown []))       = M.vScrollBy (M.viewportScroll RulesVP) 1
appEvent (T.VtyEvent (V.EvKey V.KUp []))         = M.vScrollBy (M.viewportScroll RulesVP) (-1)
appEvent (T.VtyEvent (V.EvKey V.KEsc []))        = M.halt
appEvent (T.VtyEvent (V.EvKey V.KEnter []))      = M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
appEvent (T.VtyEvent (V.EvKey V.KPageDown []))   = M.vScrollBy (M.viewportScroll RulesVP) 30
appEvent (T.VtyEvent (V.EvKey V.KPageUp []))     = M.vScrollBy (M.viewportScroll RulesVP) (-30)
appEvent _ = return ()

app :: M.App () e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theMap
          , M.appChooseCursor = M.neverShowCursor
          }

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [
    (A.attrName "bold", fg V.white `V.withStyle` V.bold),
    (A.attrName "underline", fg V.white `V.withStyle` V.underline)]

rulesMain :: IO ()
rulesMain = void $ M.defaultMain app ()

