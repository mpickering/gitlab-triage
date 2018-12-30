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
module IssuePage where

import GitLab.Tickets
import GitLab.Users
import GitLab.Common

import qualified Data.Text as T

import Brick hiding (continue, halt)

import Control.Lens (view, set)
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import Brick.Types
  ( Widget
  )

import Data.Generics.Product

import Control.Monad.IO.Class (liftIO)

import Control.Applicative.Combinators ()

import Control.Monad.Reader

import qualified Data.Set as S

import Data.List

import Model
import Autocomplete
import Common
import TextCursor
import Parsers
import ExternalEditor


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
                       FGoto      -> "g: "
                       FGen h _ _ -> h <> ": "


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


---- Handler
--o

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

{-
                       FGoto -> "g: "
                       FTitle -> "title: "
                       FLabels -> "labels: "
                       FMilestone -> "milestone: "
                       FWeight -> "weight: "


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
                       -}

startTitleInput :: IssuePage
                -> OperationalState
                -> EventM Name (Next OperationalState)
startTitleInput tl l =
  let title_ini = view (typed @IssueResp . field @"irTitle") tl
      title_mod = view (typed @Updates . typed @EditIssue . field @"eiTitle") tl
      title_t = fromMaybe title_ini title_mod

      dispatcher = FGen "title" checkTitleInput (field @"eiTitle")
  in M.continue (set typed
                 (FooterInput dispatcher
                 (fromMaybe emptyTextCursor $ makeTextCursor title_t)) l)

startLabelInput :: IssuePage
                -> OperationalState
                -> EventM Name (Next OperationalState)
startLabelInput tl l =
  let labels_ini = view (typed @IssueResp . field @"irLabels") tl
      labels_mod = view (typed @Updates . typed @EditIssue . field @"eiLabels") tl
      (Labels cur_labels) = fromMaybe labels_ini labels_mod
      labels_t = T.intercalate ", " (S.toList cur_labels)

      dispatcher = FGen "label" checkLabelInput (field @"eiLabels")
  in M.continue (set typed
                 (FooterInput dispatcher
                 (fromMaybe emptyTextCursor $ makeTextCursor labels_t)) l)

startWeightInput :: IssuePage
                -> OperationalState
                -> EventM Name (Next OperationalState)
startWeightInput tl l =
  let w_ini = view (typed @IssueResp . field @"irWeight") tl
      w_mod = view (typed @Updates . typed @EditIssue . field @"eiWeight") tl
      cur_w = fromMaybe w_ini w_mod
      weight_t = maybe (" ") (T.pack . show) cur_w

      dispatcher = FGen "weight" checkWeightInput (field @"eiWeight")
  in M.continue (set typed
                 (FooterInput dispatcher
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