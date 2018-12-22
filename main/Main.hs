{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GitLab.Tickets
import GitLab.Users
import GitLab.Common

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Servant.Client

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Network.HTTP.Types.Status
import Data.Default
import Servant.Client

import Brick

import Control.Lens
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
import Brick.Util (fg, on)

import Data.Generics.Product
import GHC.Generics (Generic)

import Control.Monad.IO.Class (liftIO)

import Cursor.Text

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer



tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = False }

gitlabBaseUrl = BaseUrl Https "gitlab.com" 443 ""

gitlabApiBaseUrl = gitlabBaseUrl { baseUrlPath = "api/v4" }

project = ProjectId 13083

listIssueNotes' token = listIssueNotes token Nothing project

data Name = IssueList | Notes | Footer deriving (Show, Ord, Generic, Eq)

main :: IO ()
main = do
  token <- AccessToken . T.strip <$> T.readFile "token"
  mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
  let env = mkClientEnv mgr gitlabApiBaseUrl
  es_raw <- runClientM (getIssue token project) env
  let es = either (error . show) id es_raw
  gui (initialState env token es)




drawUI :: AppState -> [Widget Name]
drawUI l =
    case view (field @"mode") l of
      TicketList -> ticketListWidget l
      IssueView st -> issueViewWidget (view typed l) st

ticketListWidget :: AppState -> [Widget Name]
ticketListWidget l = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case (issues l) ^. (L.listSelectedL) of
              Nothing -> str "-"
              Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ (issues l) ^. (L.listElementsL)
    box = B.borderWithLabel label $
            L.renderList listDrawElement True (issues l)
              <=>
            B.hBorder
              <=>
            footer (view typed l)

    ui = C.vCenter $ vBox [ C.hCenter box
                          ]

errorPage :: Widget n -> Widget n
errorPage reason = str "Error: " <+> reason

issueViewWidget :: FooterMode -> IssuePage -> [Widget Name]
issueViewWidget fm l = [issuePage fm l]

type Handler' s k =
  s
  -> T.BrickEvent Name ()
  -> T.EventM Name (T.Next k)

type Handler s = Handler' s s

globalHandler ::  Handler AppState -> Handler AppState
globalHandler k l re =
 case view typed l of
   FooterInfo -> infoFooterHandler k l re
   FooterGoto tc -> gotoFooterHandler tc k l re

infoFooterHandler :: Handler AppState -> Handler AppState
infoFooterHandler k l re@(T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    V.EvKey (V.KChar 'g') [] ->
      M.continue (set typed (FooterGoto emptyTextCursor) l)
    ev -> k l re

gotoFooterHandler :: TextCursor -> Handler AppState -> Handler AppState
gotoFooterHandler tc k l re@(T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue (resetFooter l)
    V.EvKey V.KEnter [] ->
      case checkGotoInput (rebuildTextCursor tc) of
        Nothing -> M.continue (resetFooter l)
        Just iid -> do
          (liftIO $ loadOneIssue iid l) >>=
            M.continue . resetFooter

    e ->
      handleTextCursorEvent
        (\tc -> k (set typed (FooterGoto tc) l) re)
        tc re

resetFooter :: AppState -> AppState
resetFooter l = (set typed FooterInfo l)


checkGotoInput :: T.Text -> Maybe IssueIid
checkGotoInput t = IssueIid <$> parseMaybe @() decimal t

loadOneIssue :: IssueIid -> AppState -> IO AppState
loadOneIssue iid s = do
  r <- runQuery s (getOneIssue iid)
  loadTicket r s




ticketListHandler :: Handler AppState
ticketListHandler l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] -> ticketListEnter l
    ev -> M.continue
            =<< handleEventLensed l (field @"issues") L.handleListEvent ev
ticketListHandler l _ = M.continue l

issuePageHandler :: Handler IssuePage
issuePageHandler l (T.VtyEvent e) =
  case e of
    ev -> M.continue
            =<< handleEventLensed l (field @"issueNotes") L.handleListEvent ev
issuePageHandler l _ = M.continue l

modeHandler :: Handler AppState
modeHandler l e =
  case view typed l of
    IssueView tl -> liftHandler typed tl IssueView issuePageHandler l e
    TicketList -> ticketListHandler l e

liftHandler
  :: ALens s s a a -- The mode to modify
  -> c        -- Inner state
  -> (c -> a) -- How to inject the new state
  -> Handler c -- Handler for inner state
  -> Handler s
liftHandler l c i h st ev = do
  let update s = set (cloneLens l) (i s) st
  fmap update <$> h c ev

appEvent :: Handler AppState
appEvent =
  globalHandler modeHandler


ticketListEnter :: AppState -> T.EventM Name (T.Next AppState)
ticketListEnter l = do
  let cursorTicket = view (field @"issues" . to L.listSelectedElement) l
  case cursorTicket of
    Nothing -> M.continue l
    Just (_, t) ->
      liftIO (loadTicket t l) >>=
            M.continue

setMode :: Mode -> AppState -> AppState
setMode = set typed

runQuery :: AppState -> (AccessToken -> ProjectId -> ClientM a) -> IO a
runQuery l q = do
  let token   = view (field @"token") l
      reqEnv  = view (field @"reqEnv") l
  res <- runClientM (q token project) reqEnv
  return (either (error . show) id res)

loadTicket :: IssueResp -> AppState -> IO AppState
loadTicket t l = do
  let iid = view (field @"irIid") t
  es_n <- runQuery l (\t p -> listIssueNotes t Nothing p iid)
  return $
    set typed
        (IssueView (IssuePage (L.list Notes (Vec.fromList es_n) 1) t))
        l


listDrawElement :: Bool -> IssueResp -> Widget n
listDrawElement sel IssueResp{..} =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in vLimit 1 $ hBox
        [ hLimit 10 $ padRight Max $ int (unIssueIid irIid)
        , hLimit 50 $ padRight Max $ txt irTitle
        , padLeft (Pad 1) $ txt irState]

issuePage :: FooterMode -> IssuePage -> Widget Name
issuePage fm l =
  joinBorders $ B.border (vBox [metainfo, desc, notesSect
                               , B.hBorder, footer fm])
  where
    IssueResp{..} = view typed l
    notes    = view (field @"issueNotes") l

    boxLabel = int (unIssueIid irIid)

    titleBox = boxLabel
                    <+> padRight (Pad 1) (txt ":") <+> (txtWrap irTitle)

    padMeta = padRight Max . hLimitPercent 50

    metainfo1 = [
                  metaRow "author" (renderAuthor irAuthor)
                , metaRow "state" (txt irState) ]

    metainfo2 = metainfo1

    metainfo =
        vBox [  titleBox
             ,  hBox [ vBox metainfo1
                     , vLimit (length metainfo1 * 2) B.vBorder
                     , vBox metainfo2]
             ]

    desc = (txtWrap irDescription)

    notesSect =
      L.renderList (\_ -> renderNote) True notes


footer m = vLimit 1 $
 case m of
   FooterInfo -> txt "r - reload; g - goto"
   FooterGoto t -> txt "g: " <+> drawTextCursor t

renderAuthor :: User -> Widget n
renderAuthor  = txt . view (field @"userName")

renderNote :: IssueNoteResp -> Widget n
renderNote i = vLimit 6 $ B.hBorder <=>
                (hBox [ noteMeta
                      , B.vBorder
                      , txtWrap (view (field @"inrBody") i)])
  where
    noteMeta =
      hLimitPercent 20 $
        vBox [ padLeft Max (renderAuthor (view (field @"inrAuthor") i))
             , padLeft Max (renderDate (view (field @"inrCreatedAt") i)) ]

metaRow :: T.Text -> Widget n -> Widget n
metaRow label widget = vLimit 2 (B.hBorderWithLabel (txt label))
                                  <=>
                                  (vLimit 1 widget)


initialState :: ClientEnv -> AccessToken -> [IssueResp]
             -> AppState
initialState env token es =
  AppState {
      mode = TicketList
    , footerMode = FooterInfo
    , issues = L.list IssueList (Vec.fromList es) 1
    , reqEnv = env
    , token  = token }

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

data IssuePage = IssuePage {
                  issueNotes :: L.List Name IssueNoteResp,
                  currentIssue :: IssueResp
                  } deriving Generic

data Mode = TicketList | IssueView IssuePage

data FooterMode = FooterInfo | FooterGoto TextCursor deriving Generic


data AppState = AppState { mode :: Mode
                         , footerMode :: FooterMode
                         , issues :: L.List Name IssueResp
                         , reqEnv :: ClientEnv
                         , token  :: AccessToken } deriving Generic

theApp :: M.App AppState () Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

gui :: AppState -> IO ()
gui s = void $ M.defaultMain theApp s

int :: Int -> Widget n
int = str . show

showR :: Show a => a -> Widget n
showR = str . show

renderDate :: T.Text -> Widget a
renderDate = txt

drawTextCursor :: TextCursor -> Widget Name
drawTextCursor tc =
  showCursor Footer (Location (textCursorIndex tc, 0))
    $ txt (rebuildTextCursor tc)

handleTextCursorEvent :: (TextCursor -> EventM Name (Next k))
                      -> Handler' TextCursor k
handleTextCursorEvent k tc e =
    case e of
        VtyEvent ve ->
            case ve of
                V.EvKey key mods ->
                    let mDo func = k . fromMaybe tc $ func tc
                    in case key of
                           V.KChar c -> mDo $ textCursorInsert c
                           V.KLeft -> mDo textCursorSelectPrev
                           V.KRight -> mDo textCursorSelectNext
                           V.KBS -> mDo textCursorRemove
                           V.KHome -> k $ textCursorSelectStart tc
                           V.KEnd -> k $ textCursorSelectEnd tc
                           V.KDel -> mDo textCursorDelete
                           _ -> k tc
                _ -> k tc
        _ -> k tc
