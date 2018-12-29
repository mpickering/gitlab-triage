{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Main(main) where

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

import Control.Lens (view, ALens,  to, set, cloneLens, Traversal', firstOf)
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Util (fg, on)

import Data.Generics.Product
import Data.Generics.Sum

import Control.Monad.IO.Class (liftIO)

import Cursor.Text

import Control.Applicative.Combinators ()

import Control.Monad.Reader

import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys

import qualified Data.ByteString as BS
import qualified Data.Set as S

import Data.List

import Config
import Model
import SetupForm
import Autocomplete
import TicketList
import Common
import Parsers
import Debug.Trace



tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = False }

gitlabBaseUrl :: String -> BaseUrl
gitlabBaseUrl base = BaseUrl Https base 443 "api/v4"

--project = ProjectId 13083

--listIssueNotes' token = listIssueNotes token Nothing project


main :: IO ()
main = do
  --deleteConfig
  conf <- readConfig
  st <- selectInitialState conf
  gui st

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

{- Initialisation -}

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

  labels <- runQuery conf getLabels
  milestones <- runQuery conf getMilestones
  users <- runQuery conf (\tok _ -> getUsers tok)
  les <- TicketListView <$> loadTicketList defaultSearchParams conf
  let mm = Operational (OperationalState les FooterInfo NoDialog
                                         labels milestones users conf)
  return $ AppState mm


setupState :: IO AppState
setupState = do
  configFile <- getConfigFile
  return $ AppState (Setup setupForm configFile)

{-
- Drawing functions
-}


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
    let dialogWindow = drawDialog (view (typed @DialogMode) l)
    in dialogWindow :
      (case view (field @"mode") l of
        TicketListView ts -> drawTicketList l ts
        IssueView st -> drawIssueView (view (typed @FooterMode) l) st)


drawDialog :: DialogMode -> Widget Name
drawDialog NoDialog = emptyWidget
drawDialog (MilestoneDialog ac) = drawAutocompleteDialog ac
drawDialog (OwnerDialog ac) = drawAutocompleteDialog ac
drawDialog (SearchParamsDialog _ _ ac) = drawAutocompleteDialog ac

drawAutocompleteDialog :: (Ord n, Show n) => Autocomplete s n a -> Widget n
drawAutocompleteDialog ac = dBox
  where
    dBox = C.centerLayer . joinBorders
            . B.border . hLimitPercent 75 . vLimitPercent 75 $ dialog

    dialog = drawAutocomplete ac

drawIssueView :: FooterMode -> IssuePage -> [Widget Name]
drawIssueView fm l = [drawIssuePage fm l]


drawIssuePage :: FooterMode -> IssuePage -> Widget Name
drawIssuePage fm l =
  B.border (vBox [metainfo, desc, notesSect
                 , updateLog, B.hBorder, footer fm])
  where
    ir@IssueResp{..} = view typed l
    EditIssue{..} = view (typed @Updates . typed) l
    notes    = view (field @"issueNotes") l

    boxLabel = drawTicketNo ir


    titleBox = boxLabel
                    <+> padRight (Pad 1) (txt ":")
                    <+> (changed txtWrap irTitle eiTitle)

    newState CloseEvent = "closed"
    newState ReopenEvent = "opened"

    metainfo1 = [
                  metaRow "Author" (drawAuthor irAuthor)
                , metaRow "(S)tate" (changed txt irState (newState <$> eiStatus))
                , metaRow "Created" (txt irCreatedAt)
                , metaRow "Updated" (txt irUpdatedAt)
                , metaRow "(M)ilestone" (changed drawMilestone irMilestone eiMilestone)]

    metainfo2 = [ metaRow "(O)wner" (changed drawOwners irAssignees eiAssignees)
                , metaRow "(L)abels" (changed drawLabels irLabels eiLabels)
                , metaRow "Related" (drawRelated (view (field @"links") l))
                , metaRow "(W)eight" (changed drawWeight irWeight eiWeight)
                , metaRow "" (txt " ")
                ]

    metainfo = joinBorders $
        vBox [  titleBox
             ,  hBox [ vBox metainfo1
                     , vLimit (length metainfo1 * 2) B.vBorder
                     , vBox metainfo2]
             , B.hBorder
             ]

    updateLog = drawUpdates (view (field @"updates") l)

    desc = (changed txtWrap irDescription eiDescription)

    notesSect =
      L.renderList drawNote True notes

changed :: (a -> Widget n) -> a -> Maybe a -> Widget n
changed f v Nothing = f v
changed f _ (Just c) = withAttr "changed" $ f c

drawMilestone :: Maybe Milestone -> Widget n
drawMilestone Nothing = txt " "
drawMilestone (Just (Milestone t _)) = txt t

drawWeight :: Maybe Int -> Widget n
drawWeight Nothing = txt " "
drawWeight (Just i) = int i

drawRelated :: [IssueResp] -> Widget n
drawRelated [] = txt " "
drawRelated us = hBox (intersperse (txt ", ")  (map drawTicketNo us))

drawTicketNo :: IssueResp -> Widget n
drawTicketNo ir = txt "#" <+> int (unIssueIid (view (field @"irIid") ir))

drawOwners :: [User] -> Widget n
drawOwners [] = txt " "
drawOwners us = hBox (intersperse (txt ", ") (map drawAuthor us))

drawLabels :: Labels -> Widget n
drawLabels (Labels s) =
  if S.null s
    then txt " "
    else hBox (intersperse (txt ", ") (map txt (S.toList s)))

drawUpdates :: Updates -> Widget Name
drawUpdates (Updates c EditIssue{..}) =
  case catMaybes rows of
    [] -> emptyWidget
    xs -> vBox $ B.hBorder : xs

  where
    rows = [ changeRow "Status" eiStatus showR
           , changeRow "Title" eiTitle showR
           , changeRow "Description" eiDescription (const (txt "changed"))
           , changeRow "Labels" eiLabels drawLabels
           , changeRow "Weight" eiWeight drawWeight
           , changeRow "Owners" eiAssignees drawOwners
           , changeRow "Milestone" eiMilestone drawMilestone
           , changeRow "Comment" c (txt . view (typed @T.Text)) ]

    changeRow :: T.Text -> Maybe a -> (a -> Widget n) -> Maybe (Widget n)
    changeRow name thing f =
      (\a -> hBox [txt name, txt " set to: ", f a]) <$> thing


footer :: FooterMode -> Widget Name
footer m = vLimit 1 $
 case m of
   FooterInfo ->
    txt "r - reload; g - goto; c - comment; d - description; F10 - commit changes"
   FooterInput im t -> txt (formatFooterMode im) <+> drawTextCursor t

formatFooterMode :: FooterInputMode -> T.Text
formatFooterMode m = case m of
                       FGoto -> "g: "
                       FTitle -> "title: "
                       FLabels -> "labels: "
                       FMilestone -> "milestone: "
                       FWeight -> "weight: "

drawAuthor :: User -> Widget n
drawAuthor  = txt . view (field @"userName")

drawNote :: Bool -> IssueNoteResp -> Widget Name
drawNote sel i = cached (Note (sel, view (field @"inrId") i)) $
      vLimit 6 $ ignoreSel B.hBorder <=>
                  (hBox [ noteMeta
                        , B.vBorder
                        , txtWrap (view (field @"inrBody") i)])
  where
    noteMeta =
      hLimitPercent 20 $
        vBox [ padLeft Max (drawAuthor (view (field @"inrAuthor") i))
             , padLeft Max (drawDate (view (field @"inrCreatedAt") i)) ]

    -- Stop the border rendering in the selection
    ignoreSel = forceAttr L.listAttr


metaRow :: T.Text -> Widget n -> Widget n
metaRow metaLabel widget = vLimit 2 (B.hBorderWithLabel (txt metaLabel))
                                  <=>
                                  (vLimit 1 widget)

--------------------------------------------------------------
{- Handlers -}
{-
The event handling strategy

1. First handle any truly global events like resizing. We need to invalidate the
cache when this happens.
2. Handle events which pertain to the footer type
3. Then delegate to handling events in the main application window.
-}

globalHandler ::  Handler OperationalState -> Handler OperationalState
globalHandler k l re = do
 case re of
   T.VtyEvent (V.EvResize {}) -> invalidateCache
   _ -> return ()
 case view typed l of
    NoDialog ->
      case view typed l of
        FooterInfo -> infoFooterHandler k l re
        FooterInput m tc -> inputFooterHandler m tc k l re
    dc -> dialogHandler dc l re

dialogHandler :: DialogMode -> Handler OperationalState
dialogHandler dc l re =
  case re of
      T.VtyEvent (V.EvKey e []) -> case e of
                                     V.KEsc -> M.continue (resetDialog l)
                                     V.KEnter -> dispatchDialogInput dc l
                                     _ -> handleDialogEvent dc l re
      _ -> M.continue l

handleDialogEvent :: DialogMode
                  -> Handler OperationalState
handleDialogEvent dc l re  =
  case dc of
    MilestoneDialog mac -> do_one mac MilestoneDialog
    OwnerDialog mac     -> do_one mac OwnerDialog
    SearchParamsDialog a1 a2 mac -> do_one mac (\mac' -> SearchParamsDialog a1 a2 mac')
    NoDialog            -> error "Handling dialog events when not in dialog mode"
  where
    do_one mac wrap = do
      mac' <- handleAutocompleteEvent mac re
      M.continue (set (typed @DialogMode) (wrap mac') l)



dispatchDialogInput :: DialogMode
                    -> OperationalState
                    -> EventM Name (Next OperationalState)
dispatchDialogInput (MilestoneDialog mac) l =
  let ms = view (field @"milestones") l
      tc = view (field @"autocompleteCursor") mac
  in
  case checkMilestoneInput (rebuildTextCursor tc) ms of
    Nothing  -> M.continue (resetDialog l)
    Just mid -> do
      traceShowM mid
      M.continue $ set (issueEdit . field @"eiMilestone") (Just mid) (resetDialog l)
dispatchDialogInput (OwnerDialog mac) l =
  let ms = view (field @"users") l
      tc = view (field @"autocompleteCursor") mac
  in
  case checkUserInput (rebuildTextCursor tc) ms of
    Nothing  -> M.continue (resetDialog l)
    Just mid -> do
      traceShowM mid
      M.continue $ set (issueEdit . field @"eiAssignees") (Just mid) (resetDialog l)
dispatchDialogInput (SearchParamsDialog check place mac) l = do
  if (T.null rbc)
    then (update Nothing)
    else (case check rbc of
            Nothing  -> M.continue (resetDialog l)
            Just mid -> update (Just mid))
  where
    tc = view (field @"autocompleteCursor") mac
    rbc = rebuildTextCursor tc
    update v = do
      let new_state = set (typed @Mode . _Ctor @"TicketListView"
                       . cloneLens place) v (resetDialog l)
          search_params =
            case firstOf (typed @Mode . _Ctor @"TicketListView" . typed @GetIssuesParams)
                         new_state of
              Just s -> s
              -- This should never happen
              Nothing -> defaultSearchParams

      tl <- liftIO $ loadTicketList search_params (view (typed @AppConfig) new_state)
      M.continue (set typed (TicketListView tl) new_state)
dispatchDialogInput NoDialog _ = error "NoDialog when handling dialog event"



infoFooterHandler :: Handler OperationalState -> Handler OperationalState
infoFooterHandler k l re@(T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'g') [] ->
      M.continue (set typed (FooterInput FGoto emptyTextCursor) l)
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
dispatchFooterInput FGoto tc l =
  case checkGotoInput (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just iid -> do
      (liftIO $ loadByIid iid (view typed l)) >>=
        M.continue . resetFooter . issueView l
dispatchFooterInput FTitle tc l =
  case checkTitleInput (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just t -> do
      M.continue $ set (issueEdit . field @"eiTitle") (Just t) (resetFooter l)
dispatchFooterInput FLabels tc l =
  case checkLabelInput (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just ls -> do
      M.continue $ set (issueEdit . field @"eiLabels") (Just ls) (resetFooter l)
dispatchFooterInput FMilestone tc l =
  let ms = view (field @"milestones") l
  in
  case checkMilestoneInput (rebuildTextCursor tc) ms of
    Nothing  -> M.continue (resetFooter l)
    Just mid -> do
      traceShowM mid
      M.continue $ set (issueEdit . field @"eiMilestone") (Just mid) (resetFooter l)
dispatchFooterInput FWeight tc l =
  case checkWeightInput (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just ls -> do
      M.continue $ set (issueEdit . field @"eiWeight") (Just ls) (resetFooter l)




issueEdit :: Traversal' OperationalState EditIssue
issueEdit = typed @Mode . _Ctor @"IssueView" . field @"updates" . typed @EditIssue

resetDialog :: OperationalState -> OperationalState
resetDialog l = (set typed NoDialog l)

resetFooter :: OperationalState -> OperationalState
resetFooter l = (set typed FooterInfo l)


checkUserInput :: T.Text
               -> [User]
               -> Maybe [User]
checkUserInput t _ | T.null (T.strip t) = Just []
checkUserInput t mr =
  Just $ maybe [] (:[]) (lookupUser (T.strip t) mr)



checkMilestoneInput :: T.Text
                    -> [MilestoneResp]
                    -> Maybe (Maybe Milestone)
checkMilestoneInput t _ | T.null (T.strip t) = Just Nothing
checkMilestoneInput t mr = Just $ lookupMilestone (T.strip t) mr

lookupMilestone :: T.Text -> [MilestoneResp] -> Maybe Milestone
lookupMilestone _ [] = Nothing
lookupMilestone t (m:ms) = if view (field @"mrTitle") m == t
                              then Just $ Milestone
                                        (view (field @"mrTitle") m)
                                        (view (field @"mrId") m)
                              else lookupMilestone t ms


-- Events which operate only on internal state
internalIssuePageHandler :: HandlerR IssuePage
internalIssuePageHandler l (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 's') [] ->
      let
        ini_state = (view (typed @IssueResp . field @"irState") l)
        mod_state = view (typed @Updates . typed @EditIssue . field @"eiStatus") l
        statusEvent = maybe (Just (toggleStatus ini_state))
                            (const Nothing) mod_state
      in continue
          (set (field @"updates" . typed @EditIssue . field @"eiStatus") statusEvent l)
    V.EvKey (V.KChar 'c') [] ->
      newCommentHandler l
    V.EvKey (V.KChar 'd') [] ->
      editDescriptionHandler l
    V.EvKey (V.KFun 10) [] ->
      applyChanges l
    ev -> continue
            =<< lift (handleEventLensed l (field @"issueNotes") L.handleListEvent ev)
internalIssuePageHandler l _ = continue l

editDescriptionHandler :: IssuePage -> ReaderT AppConfig (EventM Name) (Next IssuePage)
editDescriptionHandler ip = do
  let desc = view (typed @IssueResp . field @"irDescription") ip
      mod_desc = view (typed @Updates . typed @EditIssue . field @"eiDescription") ip
      init_desc = fromMaybe desc mod_desc
  lift $ invokeExternalEditor (Just init_desc) (postCommentAndUpdate ip)
  where
    postCommentAndUpdate :: IssuePage -> Maybe T.Text -> IO IssuePage
    postCommentAndUpdate ip' t =
      case t of
        Nothing -> return ip'
        Just "" -> return ip'
        Just descText  -> do
          return $
            (set (field @"updates" . typed @EditIssue . field @"eiDescription")
              (Just descText) ip)

newCommentHandler :: IssuePage -> ReaderT AppConfig (EventM Name) (Next IssuePage)
newCommentHandler ip = do

  let mod_cin = view (typed @Updates . field @"comment") ip
      mod_comment = view (field @"cinBody") <$> mod_cin

  lift $ invokeExternalEditor mod_comment (postCommentAndUpdate ip)
  where
    postCommentAndUpdate :: IssuePage -> Maybe T.Text -> IO IssuePage
    postCommentAndUpdate ip' t =
      case t of
        Nothing -> return ip'
        Just "" -> return ip'
        Just commentText  -> do
          let note = CreateIssueNote commentText Nothing
          return $ set (typed @Updates . field @"comment" ) (Just note) ip'

-- | Events which change the mode
issuePageHandler :: IssuePage -> Handler OperationalState
issuePageHandler ip l e =
  case e of
    (T.VtyEvent (V.EvKey V.KEsc [])) -> do
      tl <- liftIO $ loadTicketList defaultSearchParams (view (typed @AppConfig) l)
      M.continue (set typed (TicketListView tl) l)
    (T.VtyEvent (V.EvKey (V.KChar 't') []))  -> startTitleInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'l') []))  -> startLabelInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'm') []))  -> startMilestoneInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'o') []))  -> startOwnerInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'w') []))  -> startWeightInput ip l
    _ ->
      liftHandler typed ip IssueView
        (demote (view typed l) internalIssuePageHandler) l e

startTitleInput :: IssuePage
                -> OperationalState
                -> EventM Name (Next OperationalState)
startTitleInput tl l =
  let title_ini = view (typed @IssueResp . field @"irTitle") tl
      title_mod = view (typed @Updates . typed @EditIssue . field @"eiTitle") tl
      title_t = fromMaybe title_ini title_mod
  in M.continue (set typed
                 (FooterInput FTitle
                 (fromMaybe emptyTextCursor $ makeTextCursor title_t)) l)

startLabelInput :: IssuePage
                -> OperationalState
                -> EventM Name (Next OperationalState)
startLabelInput tl l =
  let labels_ini = view (typed @IssueResp . field @"irLabels") tl
      labels_mod = view (typed @Updates . typed @EditIssue . field @"eiLabels") tl
      (Labels cur_labels) = fromMaybe labels_ini labels_mod
      labels_t = T.intercalate ", " (S.toList cur_labels)
  in M.continue (set typed
                 (FooterInput FLabels
                 (fromMaybe emptyTextCursor $ makeTextCursor labels_t)) l)

startWeightInput :: IssuePage
                -> OperationalState
                -> EventM Name (Next OperationalState)
startWeightInput tl l =
  let w_ini = view (typed @IssueResp . field @"irWeight") tl
      w_mod = view (typed @Updates . typed @EditIssue . field @"eiWeight") tl
      cur_w = fromMaybe w_ini w_mod
      weight_t = maybe (" ") (T.pack . show) cur_w
  in M.continue (set typed
                 (FooterInput FWeight
                 (fromMaybe emptyTextCursor $ makeTextCursor weight_t)) l)

milestoneAutocomplete :: [MilestoneResp]
                      -> Maybe T.Text
                      -> MilestoneAutocomplete
milestoneAutocomplete ms ini =
  mkAutocomplete
    ms
    (\t s -> filter (\v -> T.toLower t `T.isInfixOf` (T.toLower (view (field @"mrTitle") v))) s)
    (view (field @"mrTitle"))
    ini
    (Dialog (MilestoneName False))
    (Dialog (MilestoneName True))

ownerAutocomplete :: [User]
                      -> Maybe T.Text
                      -> OwnerAutocomplete
ownerAutocomplete us ini =
  mkAutocomplete
    us
    (\t s -> filter (\v -> T.toLower t `T.isInfixOf` (T.toLower (view (field @"userUsername") v))) s)
    (view (field @"userUsername"))
    ini
    (Dialog (OwnerName False))
    (Dialog (OwnerName True))


startMilestoneInput :: IssuePage
                    -> OperationalState
                    -> EventM Name (Next OperationalState)
startMilestoneInput tl l =
  let milestone = view (typed @IssueResp . field @"irMilestone") tl
      milestone_t = (\(Milestone n _) -> n) <$> milestone
      milestones = view (field @"milestones") l
  in M.continue (set typed
                 (MilestoneDialog (milestoneAutocomplete milestones milestone_t))
                 l)

startOwnerInput :: IssuePage
                    -> OperationalState
                    -> EventM Name (Next OperationalState)
startOwnerInput tl l =
  let owner = view (typed @IssueResp . field @"irAssignees") tl
      owner_t = listToMaybe (view (field @"userUsername") <$> owner)
      users = view (field @"users") l
  in M.continue (set typed
                 (OwnerDialog (ownerAutocomplete users owner_t))
                 l)



applyChanges :: IssuePage -> ReaderT AppConfig (EventM Name) (Next IssuePage)
applyChanges ip = do
  let iid = view (typed @IssueResp  . field @"irIid") ip
      (Updates c ei)  = view (typed @Updates) ip
  ac <- ask

  unless (nullEditIssue ei)
         (void $ liftIO $ runQuery ac (\tok p -> editIssue tok Nothing p iid ei))
  liftIO $ forM_ c (\note ->
    runQuery ac (\tok p -> createIssueNote tok Nothing p iid note))
  -- TODO: Make this more precise
  lift invalidateCache
  -- Could save one request here if we use the response from editIssue
  liftIO (loadByIid iid ac) >>= continue

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


-- Main handler which delegates to either the setup or main mode
appEvent :: Handler AppState
appEvent a@(AppState mm) ev =
  case mm of
    Setup f cfg -> setupHandler f cfg a ev
    Operational o -> liftHandler typed o Operational handleMain a ev


{- Text Cursor -}

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



{- Styles -}

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: AppState -> A.AttrMap
theMap _ = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    , ("changed", V.red `on` V.black)
    , ("default", V.defAttr )
    ]



{-
- External Editor
-}


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
