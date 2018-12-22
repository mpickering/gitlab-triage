{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import GitLab.Tickets
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

import Lens.Micro
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
import GHC.Generics

view = flip (^.)


tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = False }

gitlabBaseUrl = BaseUrl Https "gitlab.com" 443 ""

gitlabApiBaseUrl = gitlabBaseUrl { baseUrlPath = "api/v4" }

project = ProjectId 13083

listIssueNotes' token = listIssueNotes token Nothing project

main :: IO ()
main = do
  token <- AccessToken <$> T.readFile "token"
  mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
  let env = mkClientEnv mgr gitlabApiBaseUrl
  es_raw <- runClientM (getIssue token project) env
  let es = either (error . show) id es_raw
  es_n_raw <- runClientM (listIssueNotes token Nothing project ( irIid (es !! 0))) env
  let es_n = either (error . show) id es_n_raw
  gui (initialState env token es es_n)




drawUI :: AppState -> [Widget ()]
drawUI l = [ui']
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case (issues l) ^. (L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ (issues l) ^. (L.listElementsL)
        box = B.borderWithLabel label $
              L.renderList listDrawElement True (issues l)
        ui = C.vCenter $ vBox [ C.hCenter box
                              ]

        ui' = issuePage (L.listElements (issues l) Vec.! 0) (issueNotes l)

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue . (\b -> set (field @"issues") b l)
                =<< L.handleListEvent ev (view (field @"issues") l)
appEvent l _ = M.continue l

listDrawElement :: Bool -> IssueResp -> Widget ()
listDrawElement sel IssueResp{..} =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in vLimit 1 $ hBox
        [ hLimit 10 $ padRight Max $ int (unIssueIid irIid)
        , hLimit 50 $ padRight Max $ txt irTitle
        , padLeft (Pad 1) $ txt irState]

issuePage :: IssueResp -> [IssueNoteResp] -> Widget ()
issuePage IssueResp{..} notes =
  joinBorders $ B.border (vBox [metainfo, desc, notesSect, B.hBorder, footer])
  where
    boxLabel = int (unIssueIid irIid)

    titleBox = boxLabel
                    <+> padRight (Pad 1) (txt ":") <+> (txtWrap irTitle)

    padMeta = padRight Max . hLimitPercent 50

    metainfo1 = [
                  metaRow "author" (showR irAuthor)
                , metaRow "state" (txt irState) ]

    metainfo2 = metainfo1

    metainfo =
        vBox [  titleBox
             ,  hBox [ vBox metainfo1
                     , vLimit (length metainfo1 * 2) B.vBorder
                     , vBox metainfo2]
             ,  B.hBorder
             ]

    desc = (txtWrap irDescription)

    notesSect = vBox (map renderNote notes)

    footer = vLimit 1 $ txt "r - reload"

renderNote :: IssueNoteResp -> Widget ()
renderNote i = B.hBorder <=>
                (hBox [ noteMeta
                      , B.vBorder
                      , txtWrap (view (field @"inrBody") i)])
  where
    noteMeta =
      hLimitPercent 20 $
        vBox [ padLeft Max (showR (view (field @"inrAuthor") i))
             , renderDate (view (field @"inrCreatedAt") i) ]

metaRow :: T.Text -> Widget () -> Widget ()
metaRow label widget = vLimit 2 (B.hBorderWithLabel (txt label))
                                  <=>
                                  (vLimit 1 widget)


initialState :: ClientEnv -> AccessToken -> [IssueResp]
             -> [IssueNoteResp] -> AppState
initialState env token es es_n =
  AppState {
      issues = L.list () (Vec.fromList es) 1
    , issueNotes = es_n
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

data AppState = AppState { issues :: L.List () IssueResp
                         , issueNotes :: [IssueNoteResp]
                         , reqEnv :: ClientEnv
                         , token  :: AccessToken } deriving Generic

theApp :: M.App AppState e ()
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
