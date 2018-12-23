{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import GitLab.Tickets
import GitLab.Users
import GitLab.Common

import qualified Data.Text as T
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Servant.Client

import Data.Default

import Brick
import Brick.Forms

import Control.Lens (view, ALens,  to, set, cloneLens)
import Control.Monad (void, when)
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
import Brick.Util (fg, on)

import Data.Generics.Product
import GHC.Generics (Generic)

import Control.Monad.IO.Class (liftIO)

import Cursor.Text

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

import Data.Yaml (ParseException, decodeFileEither, encodeFile)
import Data.Aeson(ToJSON, FromJSON)
import System.Directory



tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = False }

gitlabBaseUrl :: String -> BaseUrl
gitlabBaseUrl base = BaseUrl Https base 443 "api/v4"

--project = ProjectId 13083

--listIssueNotes' token = listIssueNotes token Nothing project

data Name = IssueList | Notes | Footer | FormArea T.Text
              deriving (Show, Ord, Generic, Eq)

main :: IO ()
main = do
  --deleteConfig
  conf <- readConfig
  st <- selectInitialState conf
  gui st

data OperationalState = OperationalState {
                            mode :: Mode
                          , footerMode :: FooterMode
                          , config :: AppConfig
                          } deriving Generic
-- TODO: Make a separte SetupState type
data MajorMode = Setup (Form UserConfig () Name) FilePath
               | Operational OperationalState deriving Generic

selectInitialState :: Either a UserConfig -> IO AppState
selectInitialState e =
  case e of
    Left {} -> setupState
    Right c -> initialise c

initialise :: UserConfig -> IO AppState
initialise c = do
  mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
  let env = mkClientEnv mgr
              (gitlabBaseUrl (view (field @"url" . to T.unpack) c))
      conf = (AppConfig c env)

  es <- runQuery conf getIssue
  let les = TicketListView (TicketList (L.list IssueList (Vec.fromList es) 1))
      mm = Operational (OperationalState les FooterInfo conf)
  return $ AppState mm

setupState :: IO AppState
setupState = do
  configFile <- getConfigFile
  return $ AppState (Setup setupForm configFile)

drawUI :: AppState -> [Widget Name]
drawUI (AppState a) = case a of
                        Setup form cfg -> drawSetup form cfg
                        Operational o -> drawMain o

drawSetup :: Form UserConfig e Name -> FilePath -> [Widget Name]
drawSetup form cfg = [ui, background]
  where
    background = fill '@'
    formBox = C.center . joinBorders
       . B.border . hLimitPercent 75 . vLimit 5 $ renderForm form

    ui = formBox <=> fileFooter <=> setupFooter

    fileFooter = txt "Config will be written to: " <+> str cfg

    setupFooter =
      vLimit 1 $
        txt "esc - quit; tab - next field; enter - submit"


drawMain :: OperationalState -> [Widget Name]
drawMain l =
    case view (field @"mode") l of
      TicketListView ts -> ticketListWidget l ts
      IssueView st -> issueViewWidget (view typed l) st

ticketListWidget :: OperationalState -> TicketList -> [Widget Name]
ticketListWidget l tl = [ui]
  where
    issues = view (field @"issues") tl
    boxLabel = str "Item " <+> cur <+> str " of " <+> total
    cur = case view L.listSelectedL issues of
              Nothing -> str "-"
              Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ view L.listElementsL issues
    box = B.borderWithLabel boxLabel $
            L.renderList listDrawElement True issues
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

globalHandler ::  Handler OperationalState -> Handler OperationalState
globalHandler k l re =
 case view typed l of
   FooterInfo -> infoFooterHandler k l re
   FooterGoto tc -> gotoFooterHandler tc k l re

infoFooterHandler :: Handler OperationalState -> Handler OperationalState
infoFooterHandler k l re@(T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    V.EvKey (V.KChar 'g') [] ->
      M.continue (set typed (FooterGoto emptyTextCursor) l)
    _ev -> k l re
infoFooterHandler k l re = k l re

gotoFooterHandler :: TextCursor -> Handler OperationalState
                                -> Handler OperationalState
gotoFooterHandler tc k l re@(T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue (resetFooter l)
    V.EvKey V.KEnter [] ->
      case checkGotoInput (rebuildTextCursor tc) of
        Nothing -> M.continue (resetFooter l)
        Just iid -> do
          (liftIO $ loadOneIssue iid l) >>=
            M.continue . resetFooter

    _ ->
      handleTextCursorEvent
        (\tc' -> k (set typed (FooterGoto tc') l) re)
        tc re
gotoFooterHandler _ k l re = k l re

resetFooter :: OperationalState -> OperationalState
resetFooter l = (set typed FooterInfo l)


checkGotoInput :: T.Text -> Maybe IssueIid
checkGotoInput t = IssueIid <$> parseMaybe @() decimal t

loadOneIssue :: IssueIid -> OperationalState -> IO OperationalState
loadOneIssue iid s = do
  r <- runQuery (view typed s) (getOneIssue iid)
  loadTicket r s




ticketListHandler :: TicketList -> Handler OperationalState
ticketListHandler tl l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] -> ticketListEnter tl l
    _ -> do
      res <- L.handleListEvent e (view typed tl)
      M.continue (set typed (TicketListView (TicketList res)) l)
ticketListHandler _ l _ = M.continue l

issuePageHandler :: Handler IssuePage
issuePageHandler l (T.VtyEvent e) =
  case e of
    ev -> M.continue
            =<< handleEventLensed l (field @"issueNotes") L.handleListEvent ev
issuePageHandler l _ = M.continue l

modeHandler :: Handler OperationalState
modeHandler l e =
  case view typed l of
    IssueView tl ->
      liftHandler typed tl IssueView issuePageHandler l e
    TicketListView tl -> ticketListHandler tl l e

-- | liftHandler lifts a handler which only operates on its own state into
-- a larger state. It won't work if the handler needs to modify something
-- from a larger scope.
liftHandler
  :: ALens s s a a -- The mode to modify
  -> c        -- Inner state
  -> (c -> a) -- How to inject the new state
  -> Handler c -- Handler for inner state
  -> Handler s
liftHandler l c i h st ev = do
  let update s = set (cloneLens l) (i s) st
  fmap update <$> h c ev

handleMain :: Handler OperationalState
handleMain =
  globalHandler modeHandler

setupHandler :: SetupForm -> FilePath -> Handler AppState
setupHandler f cfg _ e = do
  f' <- handleFormEvent e f
  let s' = AppState (Setup f' cfg)
  case e of
    T.VtyEvent (V.EvKey V.KEnter []) ->
      if allFieldsValid f'
        then do
                let c = formState f'
                liftIO $ writeConfig c
                liftIO (initialise c) >>= M.continue

        else M.continue s'
    T.VtyEvent (V.EvKey V.KEsc []) -> M.halt s'
    _ -> M.continue s'


appEvent :: Handler AppState
appEvent a@(AppState mm) ev = case mm of
                              Setup f cfg -> setupHandler f cfg a ev
                              Operational o ->
                                liftHandler typed o Operational handleMain a ev



ticketListEnter :: TicketList -> OperationalState -> T.EventM Name (T.Next OperationalState)
ticketListEnter tl o = do
  let cursorTicket = view (field @"issues" . to L.listSelectedElement) tl
  case cursorTicket of
    Nothing -> M.continue o
    Just (_, t) ->
      liftIO (loadTicket t o) >>=
            M.continue

setMode :: Mode -> OperationalState -> OperationalState
setMode = set typed

runQuery :: AppConfig -> (AccessToken -> ProjectId -> ClientM a) -> IO a
runQuery l q = do
  let tok   = view (field @"userConfig" . field @"token") l
      reqEnv  = view (field @"reqEnv") l
      project = view (field @"userConfig" . field @"project") l
  res <- runClientM (q tok project) reqEnv
  return (either (error . show) id res)

loadTicket :: IssueResp -> OperationalState -> IO OperationalState
loadTicket t l = do
  let iid = view (field @"irIid") t
  es_n <- runQuery (view typed l) (\t' p -> listIssueNotes t' Nothing p iid)
  return $
    set typed
        (IssueView (IssuePage (L.list Notes (Vec.fromList es_n) 1) t))
        l


listDrawElement :: Bool -> IssueResp -> Widget n
listDrawElement _ IssueResp{..} =
    vLimit 1 $ hBox
        [ hLimit 6 $ padRight Max $ int (unIssueIid irIid)
        , padRight Max $ txt irTitle
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

footer :: FooterMode -> Widget Name
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
metaRow metaLabel widget = vLimit 2 (B.hBorderWithLabel (txt metaLabel))
                                  <=>
                                  (vLimit 1 widget)



customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    ]

data TicketList = TicketList {
                    issues :: L.List Name IssueResp
                    } deriving Generic

data IssuePage = IssuePage {
                  issueNotes :: L.List Name IssueNoteResp,
                  currentIssue :: IssueResp
                  } deriving Generic

data Mode = TicketListView TicketList | IssueView IssuePage

data FooterMode = FooterInfo | FooterGoto TextCursor deriving Generic


data AppState = AppState { majorMode :: MajorMode
                         } deriving Generic

data AppConfig = AppConfig {
                    userConfig :: UserConfig
                  , reqEnv :: ClientEnv
                  } deriving Generic

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
                V.EvKey key _mods ->
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




data UserConfig = UserConfig
      { token :: AccessToken
      , url :: T.Text
      , project :: ProjectId
      } deriving (Generic, ToJSON, FromJSON)

getConfigFile :: IO FilePath
getConfigFile = getXdgDirectory XdgConfig ".gitlab-triager"


readConfig :: IO (Either ParseException UserConfig)
readConfig = do
  configFile <- getConfigFile
--  putStrLn $ "Reading config from: " ++ configFile
  decodeFileEither configFile

writeConfig :: UserConfig -> IO ()
writeConfig uc = do
  configFile <- getConfigFile
--  putStrLn $ "Writing config to: " ++ configFile
  encodeFile configFile uc

deleteConfig :: IO ()
deleteConfig = do
  configFile <- getConfigFile
  exists <- doesFileExist configFile
--  putStrLn $ "Removing config: " ++ configFile
  when exists (removeFile configFile)

type SetupForm = Form UserConfig () Name

-- TODO: Set invalid input style
setupForm :: Form UserConfig e Name
setupForm = newForm fields
                    (UserConfig (AccessToken "")
                                "gitlab.com"
                                (ProjectId 13083))

  where
    fields = [ row "Token" tokenField
             , row "Base URL" urlField
             , row "Project ID" projectField ]

    row s w = vLimit 1 . ((hLimit 11 $ padLeft Max $ txt s) <+> B.vBorder <+>) @@= w

    tokenField = editField (field @"token" . typed @T.Text)
                           (FormArea "token")
                           (Just 1)
                           id
                           validateToken
                           (txt . T.intercalate "\n")
                           id

    urlField = editTextField (field @"url")
                             (FormArea "url")
                             (Just 1)


    projectField = editShowableField (field @"project" . typed @Int)
                                     (FormArea "project")

    validateToken :: [T.Text] -> Maybe T.Text
    validateToken [t] = if T.length t == 20 then Just t else Nothing
    validateToken _   = Nothing

