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
{-# LANGUAGE NoMonomorphismRestriction #-}
module IssuePage where

import GitLab.Tickets
import GitLab.Users
import GitLab.Common

import qualified Data.Text as T

import Brick hiding (continue, halt)

import Control.Lens (view, set, ALens, cloneLens, iso, to)
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Graphics.Vty as V
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N

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
import qualified Data.Foldable as F

import Data.List

import Model
import Autocomplete
import Common
import TextCursor
import Parsers
import ExternalEditor
import Dialog
import qualified IOList
import TicketList
import Cache


drawIssueView :: FooterMode -> IssuePage -> [Widget Name]
drawIssueView fm l = [drawIssuePage fm l]


drawIssuePage :: FooterMode -> IssuePage -> Widget Name
drawIssuePage fm (IssuePage tl l) =
  B.border (vBox [metainfo, desc, notesSect
                 , updateLog, B.hBorder, footerSect ])
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

    desc = vLimit 20 (changed txtWrap irDescription eiDescription)

    notesSect =
      L.renderList drawNote True notes

    footerSect =  footer (view (field @"issues" . to IOList.curAndLen) tl) fm

changed :: (a -> Widget n) -> a -> Maybe a -> Widget n
changed f v Nothing = f v
changed f _ (Just c) = withAttr "changed" $ f c

drawMilestone :: Maybe MilestoneResp -> Widget n
drawMilestone Nothing = txt " "
drawMilestone (Just mr) = txt (view (field @"mrTitle") mr)

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

footer :: Maybe (Int, Int) -> FooterMode -> Widget Name
footer kn m = vLimit 1 $
 case m of
   FooterMessage t -> txt t
   FooterInfo ->
    (txt "r - reload; g - goto; c - comment; d - description; v - view comment; F10 - commit changes")
    <+>
    (maybe emptyWidget (\(k, n) -> padLeft Max (int (k + 1) <+> txt "/" <+> int n <+> txt "(p/n)")) kn)
   FooterInput im t -> txt (formatFooterMode im) <+> drawTextCursor t



drawAuthor :: User -> Widget n
drawAuthor  = txt . view (field @"userName")

drawNote :: Bool -> IssueNoteResp -> Widget Name
drawNote sel i = cached (Note (sel, view (field @"inrId") i)) $
      vLimit 6 $
        ignoreSel B.hBorder <=>
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
internalIssuePageHandler :: HandlerR IssuePageContents
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
    ev -> continue
            =<< lift (handleEventLensed l (field @"issueNotes") L.handleListEvent ev)
internalIssuePageHandler l _ = continue l

editDescriptionHandler :: IssuePageContents -> ReaderT AppConfig (EventM Name) (Next IssuePageContents)
editDescriptionHandler ip = do
  let desc = view (typed @IssueResp . field @"irDescription") ip
      mod_desc = view (typed @Updates . typed @EditIssue . field @"eiDescription") ip
      init_desc = fromMaybe desc mod_desc
  lift $ invokeExternalEditor (Just init_desc) (postCommentAndUpdate init_desc)
  where
    setDesc d = set (field @"updates" . typed @EditIssue . field @"eiDescription") d ip

    postCommentAndUpdate :: T.Text -> Maybe T.Text -> IO IssuePageContents
    postCommentAndUpdate old_desc t =
      case t of
        Nothing -> return ip
        Just "" -> return ip
        Just descText | T.strip old_desc == T.strip descText -> return ip
        Just descText -> do
          return $ setDesc (Just descText)

newCommentHandler :: IssuePageContents -> ReaderT AppConfig (EventM Name) (Next IssuePageContents)
newCommentHandler ip = do

  let mod_cin = view (typed @Updates . field @"comment") ip
      mod_comment = view (field @"cinBody") <$> mod_cin

  lift $ invokeExternalEditor mod_comment (postCommentAndUpdate ip)
  where
    postCommentAndUpdate :: IssuePageContents -> Maybe T.Text -> IO IssuePageContents
    postCommentAndUpdate ip' t =
      case t of
        Nothing -> return ip'
        Just "" -> return ip'
        Just commentText  -> do
          let note = CreateIssueNote commentText Nothing
          return $ set (typed @Updates . field @"comment" ) (Just note) ip'

-- | Events which change the mode
issuePageHandler :: IssuePage -> Handler OperationalState
issuePageHandler i@(IssuePage tl ip) l e =
  case e of
    (T.VtyEvent (V.EvKey V.KEsc [])) -> do
      M.continue (set typed (TicketListView tl) l)
    (T.VtyEvent (V.EvKey (V.KChar 't') []))  -> startTitleInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'l') []))  -> startLabelInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'm') []))  -> startMilestoneInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'o') []))  -> startOwnerInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'w') []))  -> startWeightInput ip l
    (T.VtyEvent (V.EvKey (V.KChar 'n') []))  -> ticketListEvent IOList.listMoveDown i l
    (T.VtyEvent (V.EvKey (V.KChar 'p') []))  -> ticketListEvent IOList.listMoveUp i l
    (T.VtyEvent (V.EvKey (V.KChar 'v') []))  -> viewComment ip l

    (T.VtyEvent (V.EvKey (V.KFun 10) [])) -> applyChanges (IssuePage tl ip) l
    _ ->
      liftHandler typed ip (IssueView . IssuePage tl)
        (demote (view typed l) internalIssuePageHandler) l e

viewComment :: IssuePageContents
            -> OperationalState
            -> EventM Name (Next OperationalState)
viewComment ip o =
  let notes = view (field @"issueNotes") ip
      el    = L.listSelectedElement notes
  in case el of
       Nothing -> M.continue o
       Just (k, note) ->

        let info = view (field @"inrBody") note
        in M.continue (set typed (InfoDialog info) o)

ticketListEvent :: (IOList.IOListWidget Name IssueResp -> IO (IOList.IOListWidget Name IssueResp))
                  -> IssuePage -> OperationalState
                                  -> EventM Name (Next OperationalState)
ticketListEvent f (IssuePage tl ps) s =  do
  let is = view (field @"issues") tl
  is' <- liftIO $ f is
  let tl' = set (field @"issues") is' tl
  ticketListEnter tl' s


startTitleInput :: IssuePageContents
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

startLabelInput :: IssuePageContents
                -> OperationalState
                -> EventM Name (Next OperationalState)
startLabelInput tl l =
  let labels_ini = view (typed @IssueResp . field @"irLabels") tl
      labels_mod = view (typed @Updates . typed @EditIssue . field @"eiLabels") tl
      (Labels cur_labels) = fromMaybe labels_ini labels_mod
      cur_labels_t = S.toList cur_labels

      place = (typed @Updates . typed @EditIssue . field @"eiLabels" . labelsIso)

  in M.continue $ startMultiDialogIO
                    txtLabels
                    (return . checkLabel)
                    (pureRestrict txtLabels)
                    place
                    cur_labels_t
                    ls tl l
  where
    splitLabels :: Maybe Labels -> [T.Text]
    splitLabels Nothing = []
    splitLabels (Just (Labels ss)) = S.toList ss

    joinLabels :: [T.Text] -> Labels
    joinLabels xs =
      let xs' = filter (\t -> not (T.null (T.strip t))) xs
      in foldMap (Labels . S.singleton) xs'

    labelsIso = iso splitLabels (Just . joinLabels)

    txtLabels :: T.Text -> T.Text
    txtLabels = id

    ls = view (field @"lrName")
          <$> view (field @"labels") l

    checkLabel :: T.Text -> Maybe T.Text
    checkLabel = Just

startWeightInput :: IssuePageContents
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


startMilestoneInput :: IssuePageContents
                    -> OperationalState
                    -> EventM Name (Next OperationalState)
startMilestoneInput ip os =
  let milestones = view (field @"milestones") os
      draw Nothing = ""
      draw (Just m) = view (field @"mrTitle") m

      def = view (typed @IssueResp . field @"irMilestone") ip

      place = (typed @Updates . typed @EditIssue . field @"eiMilestone")

      start_val = fromMaybe def (view place ip)

  in M.continue
      $ startDialogWithDef draw
                 (checkMilestoneInput milestones)
                 place
                 (Just start_val)
                 (map Just milestones)
                 ip os


startOwnerInput :: IssuePageContents
                    -> OperationalState
                    -> EventM Name (Next OperationalState)
startOwnerInput ip os =
  let users = view (field @"users") os
      draw  = T.intercalate ", " . map (view (field @"userUsername"))
      ac = view (typed @AppConfig) os
      def = view (typed @IssueResp . field @"irAssignees") ip
      place = (typed @Updates . typed @EditIssue . field @"eiAssignees")
      start_val = fromMaybe def (view place ip)
  in M.continue $
        startDialogIOWithDef draw
                   (fmap (fmap (:[])) . checkAuthor ac)
                   (\t _ -> map (:[]) <$> restrictAuthor ac t users)
                   place
                   (Just start_val)
                   (map (:[]) users)
                   ip os


applyChanges :: IssuePage -> OperationalState -> EventM Name (Next OperationalState)
applyChanges (IssuePage tl ip) o = do
  let iid = view (typed @IssueResp  . field @"irIid") ip
      (Updates c ei)  = view (typed @Updates) ip
      ac = view (typed @AppConfig) o
      cache = view (field @"cache") ac
  displayError (do
          unless (nullEditIssue ei)
                 (void $ runQuery ac (\tok p -> editIssue tok Nothing p iid ei))
          forM_ c (\note ->
            runQuery ac (\tok p -> createIssueNote tok Nothing p iid note))
          liftIO $ clearCache cache
        -- Could save one request here if we use the response from editIssue
          loadByIid iid ac)
          (\v -> invalidateCache >> M.continue (set typed (IssueView (IssuePage tl v)) o)) o

demote :: AppConfig -> HandlerR a -> Handler a
demote ac h a e = runReaderT (h a e) ac
