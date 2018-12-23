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
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Servant.Client

import Data.Default

import Brick hiding (continue, halt)
import Brick.Forms

import Control.Lens (view, ALens,  to, set, cloneLens, Traversal')
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
import Data.Generics.Sum
import GHC.Generics (Generic)

import Control.Monad.IO.Class (liftIO)

import Cursor.Text

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

import Data.Yaml (ParseException, decodeFileEither, encodeFile)
import Data.Aeson(ToJSON, FromJSON)
import System.Directory

import Control.Monad.Reader

import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys

import qualified Data.ByteString as BS

import Data.List



tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = False }

gitlabBaseUrl :: String -> BaseUrl
gitlabBaseUrl base = BaseUrl Https base 443 "api/v4"

--project = ProjectId 13083

--listIssueNotes' token = listIssueNotes token Nothing project

data Name = IssueList | Notes | Footer | FormArea T.Text | Note (Bool, Int)
          | Metainfo IssueIid
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
      IssueView st -> issueViewWidget (view (typed @FooterMode) l) st

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

type HandlerR' s k =
  s
  -> T.BrickEvent Name ()
  -> ReaderT AppConfig (T.EventM Name) (T.Next k)

type Handler s = Handler' s s

type HandlerR s = HandlerR' s s

halt :: k -> ReaderT AppConfig (T.EventM Name) (T.Next k)
halt = lift . M.halt

continue :: k -> ReaderT AppConfig (T.EventM Name) (T.Next k)
continue = lift . M.continue

globalHandler ::  Handler OperationalState -> Handler OperationalState
globalHandler k l re = do
 case re of
   T.VtyEvent (V.EvResize {}) -> invalidateCache
   _ -> return ()
 case view typed l of
   FooterInfo -> infoFooterHandler k l re
   FooterInput m tc -> inputFooterHandler m tc k l re

infoFooterHandler :: Handler OperationalState -> Handler OperationalState
infoFooterHandler k l re@(T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    V.EvKey (V.KChar 'g') [] ->
      M.continue (set typed (FooterInput Goto emptyTextCursor) l)
    V.EvKey (V.KFun 2) [] ->
      M.continue (set typed (FooterInput Title emptyTextCursor) l)
    _ev -> k l re
infoFooterHandler k l re = k l re

inputFooterHandler :: FooterInputMode
                   -> TextCursor
                   -> Handler OperationalState
                   -> Handler OperationalState
inputFooterHandler m tc k l re@(T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue (resetFooter l)
    V.EvKey V.KEnter [] ->
      dispatchFooterInput m tc l

    _ ->
      handleTextCursorEvent
        (\tc' -> k (set typed (FooterInput m tc') l) re)
        tc re
inputFooterHandler _ _ k l re = k l re

-- | What happens when we press enter in footer input mode
dispatchFooterInput :: FooterInputMode
                    -> TextCursor
                    -> OperationalState
                    -> EventM n (Next OperationalState)
dispatchFooterInput Goto tc l =
  case checkGotoInput (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just iid -> do
      (liftIO $ loadOneIssue iid (view typed l)) >>=
        M.continue . resetFooter . issueView l
dispatchFooterInput Title tc l =
  case checkTitleInput (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just t -> do
      M.continue $ set (issueEdit . field @"eiTitle") (Just t) (resetFooter l)


issueEdit :: Traversal' OperationalState EditIssue
issueEdit = typed @Mode . _Ctor @"IssueView" . field @"updates"


resetFooter :: OperationalState -> OperationalState
resetFooter l = (set typed FooterInfo l)

issueView :: OperationalState -> IssuePage -> OperationalState
issueView l n = set (field @"mode") (IssueView n) l


checkGotoInput :: T.Text -> Maybe IssueIid
checkGotoInput t = IssueIid <$> parseMaybe @() decimal t

checkTitleInput :: T.Text -> Maybe T.Text
checkTitleInput "" = Nothing
checkTitleInput t  = Just t

loadOneIssue :: IssueIid -> AppConfig -> IO IssuePage
loadOneIssue iid ac = do
  r <- runQuery ac (getOneIssue iid)
  loadTicket r ac




ticketListHandler :: TicketList -> Handler OperationalState
ticketListHandler tl l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEnter [] -> ticketListEnter tl l
    _ -> do
      res <- L.handleListEvent e (view typed tl)
      M.continue (set typed (TicketListView (TicketList res)) l)
ticketListHandler _ l _ = M.continue l


-- Events which operate only on internal state
internalIssuePageHandler :: HandlerR IssuePage
internalIssuePageHandler l (T.VtyEvent e) =
  case e of
    V.EvKey (V.KFun 1) [] ->
      let statusEvent =
            toggleStatus (view (typed @IssueResp . field @"irState") l)
      in continue
          (set (typed @EditIssue . field @"eiStatus") (Just statusEvent) l)
    V.EvKey (V.KFun 3) [] ->
      newCommentHandler l
    V.EvKey (V.KFun 4) [] ->
      editDescriptionHandler l

    V.EvKey (V.KFun 10) [] ->
      applyChanges l
    ev -> continue
            =<< lift (handleEventLensed l (field @"issueNotes") L.handleListEvent ev)
internalIssuePageHandler l _ = continue l

editDescriptionHandler :: IssuePage -> ReaderT AppConfig (EventM Name) (Next IssuePage)
editDescriptionHandler ip = do
  let desc = view (typed @IssueResp . field @"irDescription") ip
  lift $ invokeExternalEditor (Just desc) (postCommentAndUpdate ip)
  where
    postCommentAndUpdate :: IssuePage -> Maybe T.Text -> IO IssuePage
    postCommentAndUpdate ip' t =
      case t of
        Nothing -> return ip'
        Just "" -> return ip'
        Just descText  -> do
          return $
            (set (typed @EditIssue . field @"eiDescription") (Just descText) ip)

-- TODO: Should this be queued like the other events?
newCommentHandler :: IssuePage -> ReaderT AppConfig (EventM Name) (Next IssuePage)
newCommentHandler ip = do
  ac <- ask
  lift $ invokeExternalEditor Nothing (postCommentAndUpdate ac ip)
  where
    postCommentAndUpdate :: AppConfig -> IssuePage -> Maybe T.Text -> IO IssuePage
    postCommentAndUpdate ac ip' t =
      case t of
        Nothing -> return ip'
        Just "" -> return ip'
        Just commentText  -> do
          let iid = view (typed @IssueResp . field @"irIid") ip'
              note = CreateIssueNote commentText Nothing
          _ <- runQuery ac (\tok p -> createIssueNote tok Nothing p iid note)
          loadOneIssue iid ac



-- | Events which change the mode
issuePageHandler :: IssuePage -> Handler OperationalState
issuePageHandler tl l e =
  case e of
    _ ->
      liftHandler typed tl IssueView
        (demote (view typed l) internalIssuePageHandler) l e

toggleStatus :: T.Text -> StatusEvent
toggleStatus "closed" = ReopenEvent
toggleStatus "opened" = CloseEvent
toggleStatus s = error $ "Not Handled:" ++ show s

applyChanges :: IssuePage -> ReaderT AppConfig (EventM Name) (Next IssuePage)
applyChanges ip = do
  let iid = view (typed @IssueResp  . field @"irIid") ip
      ei  = view (typed @EditIssue) ip
  ac <- ask
  ir <- liftIO $ runQuery ac (\tok p -> editIssue tok Nothing p iid ei)
  liftIO (loadTicket ir ac) >>= continue

demote :: AppConfig -> HandlerR a -> Handler a
demote ac h a e = runReaderT (h a e) ac

modeHandler :: Handler OperationalState
modeHandler l e =
  case view typed l of
    IssueView tl -> issuePageHandler tl l e
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
      liftIO (loadTicket t (view (typed @AppConfig) o)) >>=
            M.continue . issueView o

setMode :: Mode -> OperationalState -> OperationalState
setMode = set typed

runQuery :: AppConfig -> (AccessToken -> ProjectId -> ClientM a) -> IO a
runQuery l q = do
  let tok   = view (field @"userConfig" . field @"token") l
      reqEnv  = view (field @"reqEnv") l
      project = view (field @"userConfig" . field @"project") l
  res <- runClientM (q tok project) reqEnv
  return (either (error . show) id res)

loadTicket :: IssueResp -> AppConfig -> IO IssuePage
loadTicket t l = do
  let iid = view (field @"irIid") t
  es_n <- runQuery l (\t' p -> listIssueNotes t' Nothing p iid)
  return $ (IssuePage (L.list Notes (Vec.fromList es_n) 6) t noEdits)


listDrawElement :: Bool -> IssueResp -> Widget n
listDrawElement _ IssueResp{..} =
    vLimit 1 $ hBox
        [ hLimit 6 $ padRight Max $ int (unIssueIid irIid)
        , padRight Max $ txt irTitle
        , padLeft (Pad 1) $ txt irState]

issuePage :: FooterMode -> IssuePage -> Widget Name
issuePage fm l =
  B.border (vBox [metainfo, desc, notesSect
                 , updateLog, B.hBorder, footer fm])
  where
    IssueResp{..} = view typed l
    notes    = view (field @"issueNotes") l

    boxLabel = int (unIssueIid irIid)

    titleBox = boxLabel
                    <+> padRight (Pad 1) (txt ":") <+> (txtWrap irTitle)

    metainfo1 = [
                  metaRow "author" (renderAuthor irAuthor)
                , metaRow "state" (txt irState)
                , metaRow "Created" (txt irCreatedAt) ]

    metainfo2 = [ metaRow "Owner" (renderOwners irAssignees)
                , metaRow "Labels" (renderLabels irLabels)
                , metaRow "Updated" (txt irUpdatedAt) ]

    metainfo = cached (Metainfo irIid) $ joinBorders $
        vBox [  titleBox
             ,  hBox [ vBox metainfo1
                     , vLimit (length metainfo1 * 2) B.vBorder
                     , vBox metainfo2]
             , B.hBorder
             ]

    updateLog = renderUpdates (view (field @"updates") l)

    desc = (txtWrap irDescription)

    notesSect =
      L.renderList renderNote True notes

renderOwners :: [User] -> Widget n
renderOwners [] = txt " "
renderOwners us = hBox (intersperse (txt ", ") (map renderAuthor us))

renderLabels :: [T.Text] -> Widget n
renderLabels [] = txt " "
renderLabels us = hBox (intersperse (txt ", ") (map txt us))

renderUpdates :: EditIssue -> Widget Name
renderUpdates e@EditIssue{..} =
  if nullEditIssue e
    then emptyWidget
    else vBox
           $ B.hBorder :
            [txt "Status set to: " <+> showR eiStatus
            , txt "Title set to: " <+> showR eiTitle ]

footer :: FooterMode -> Widget Name
footer m = vLimit 1 $
 case m of
   FooterInfo ->
    txt "r - reload; g - goto; F1 - open/close; F2 - title; F10 - Apply changes"
    <+>
    txt "; F3 - comment; F4 - description"
   FooterInput im t -> txt (formatFooterMode im) <+> drawTextCursor t

formatFooterMode :: FooterInputMode -> T.Text
formatFooterMode m = case m of
                       Goto -> "g: "
                       Title -> "title: "

renderAuthor :: User -> Widget n
renderAuthor  = txt . view (field @"userName")

renderNote :: Bool -> IssueNoteResp -> Widget Name
renderNote sel i = cached (Note (sel, view (field @"inrId") i)) $
      vLimit 6 $ ignoreSel B.hBorder <=>
                  (hBox [ noteMeta
                        , B.vBorder
                        , txtWrap (view (field @"inrBody") i)])
  where
    noteMeta =
      hLimitPercent 20 $
        vBox [ padLeft Max (renderAuthor (view (field @"inrAuthor") i))
             , padLeft Max (renderDate (view (field @"inrCreatedAt") i)) ]

    -- Stop the border rendering in the selection
    ignoreSel = forceAttr L.listAttr


metaRow :: T.Text -> Widget n -> Widget n
metaRow metaLabel widget = vLimit 2 (B.hBorderWithLabel (txt metaLabel))
                                  <=>
                                  (vLimit 1 widget)



customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: AppState -> A.AttrMap
theMap _ = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    , ("default", V.defAttr )
    ]

data TicketList = TicketList {
                    issues :: L.List Name IssueResp
                    } deriving Generic

data IssuePage = IssuePage {
                  issueNotes :: L.List Name IssueNoteResp
                  , currentIssue :: IssueResp
                  , updates :: EditIssue
                  } deriving Generic

data Mode = TicketListView TicketList | IssueView IssuePage deriving Generic

data FooterInputMode = Goto | Title

data FooterMode = FooterInfo | FooterInput FooterInputMode TextCursor deriving Generic


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
          , M.appAttrMap = theMap
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


-- Copied from matterhorn
invokeExternalEditor
  :: Maybe T.Text
  -> (Maybe T.Text -> IO s)
  -> EventM n (Next s)
invokeExternalEditor initialText k = do
    -- If EDITOR is in the environment, write the current message to a
    -- temp file, invoke EDITOR on it, read the result, remove the temp
    -- file, and update the program state.
    --
    -- If EDITOR is not present, fall back to 'vi'.
    mEnv <- liftIO $ Sys.lookupEnv "EDITOR"
    let editorProgram = maybe "vi" id mEnv

    suspendAndResume $ do
      Sys.withSystemTempFile "gitlab-triage.tmp" $ \tmpFileName tmpFileHandle -> do
--         Write the current message to the temp file
        case initialText of
          Nothing -> return ()
          Just t -> do
                       Sys.hPutStr tmpFileHandle $ T.unpack $ t
                       Sys.hClose tmpFileHandle

        -- Run the editor
        status <- Sys.system (editorProgram <> " " <> tmpFileName)

        -- On editor exit, if exited with zero status, read temp file.
        -- If non-zero status, skip temp file read.
        case status of
            Sys.ExitSuccess -> do
                tmpBytes <- BS.readFile tmpFileName
                case T.decodeUtf8' tmpBytes of
                    Left _ -> do
                        error "Failed to decode file contents as UTF-8"
                    Right t -> k (Just t)
            Sys.ExitFailure _ -> k Nothing
