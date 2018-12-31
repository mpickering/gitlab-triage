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
module TicketList where

import GitLab.Tickets
import GitLab.Users
import GitLab.Common

import qualified Data.Text as T

import Brick hiding (continue, halt)

import Control.Lens (view, ALens,  to, set, cloneLens)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Brick.Types ( Widget )
import Data.Generics.Product
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except

import Text.Megaparsec
import Control.Applicative.Combinators ()
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Set as S

import Model
import Autocomplete
import Common
import Parsers



ticketListHandler :: TicketList -> Handler OperationalState
ticketListHandler tl l (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt l
    V.EvKey V.KEnter [] -> ticketListEnter tl l
    V.EvKey (V.KChar 's') [] -> M.continue (startStateDialog tl l)
    V.EvKey (V.KChar 'c') [] -> M.continue (startScopeDialog tl l)
    V.EvKey (V.KChar 'l') [] -> M.continue (startLabelDialog tl l)
    V.EvKey (V.KChar 'm') [] -> M.continue (startMilestoneDialog tl l)
    V.EvKey (V.KChar 'a') [] -> M.continue (startAuthorDialog tl l)
    V.EvKey (V.KChar 'o') [] -> M.continue (startOwnerDialog tl l)
    V.EvKey (V.KChar 'w') [] -> M.continue (startWeightDialog tl l)
    _ -> do
      res <- L.handleListEvent e (view typed tl)
      let tl' = set (field @"issues") res tl
      M.continue (set typed (TicketListView tl') l)
ticketListHandler _ l _ = M.continue l

startScopeDialog, startLabelDialog,
  startMilestoneDialog, startAuthorDialog,
  startOwnerDialog, startWeightDialog
  :: TicketList
  -> OperationalState
  -> OperationalState


startScopeDialog =
  startDialog txtScope checkScope (field @"params" . field @"gipScope")
              [AllScope, CreatedByMe, AssignedToMe]
  where
    checkScope (T.toLower -> t) =
      case t of
        "all" -> Just AllScope
        "created by me" -> Just CreatedByMe
        "assigned to me" -> Just AssignedToMe
        _ -> Nothing

startLabelDialog tl l =
  startDialog txtLabelParam checkLabel (field @"params" . field @"gipLabels")
              ([NoLabels, AnyLabel] ++ ls) tl l
  where
    ls = WithLabels . mkLabel . view (field @"lrName")
          <$> view (field @"labels") l

    checkLabel (T.toLower -> t) =
      case t of
        "none" -> Just NoLabels
        "any"  -> Just AnyLabel
        ts     -> WithLabels <$> checkLabelInput ts

startMilestoneDialog tl l =
  startDialog txtMilestoneParam checkMilestone (field @"params" . field @"gipMilestone")
              ([NoMilestone, AnyMilestone] ++ ms) tl l
  where
    ms = WithMilestone . view (field @"mrTitle") <$> view (field @"milestones") l

    checkMilestone t =
      case t of
        "None" -> Just NoMilestone
        "Any"  -> Just AnyMilestone
        _      -> Just (WithMilestone t)

startAuthorDialog tl l =
  startDialog txtAuthorParam checkAuthor (field @"params" . field @"gipAuthor")
              us tl l
  where
    us = view (field @"users") l

    checkAuthor t = lookupUser t us

startOwnerDialog tl l =
  startDialog txtAssigneeParam checkAssignee (field @"params" . field @"gipAssignee")
              ([AssignedNone, AssignedAny] ++ as) tl l
  where
    us = view (field @"users") l

    as = map AssignedTo us

    checkAssignee t =
      case T.toLower t of
        "none" -> Just AssignedNone
        "any"  -> Just AssignedAny
        _      -> AssignedTo <$> lookupUser t us

startWeightDialog tl l =
  startDialog txtWeightParam checkWeight (field @"params" . field @"gipWeight")
            [0..100] tl l
  where
    checkWeight :: T.Text -> Maybe Int
    checkWeight t = parseMaybe @() decimal t



startDialog :: (a -> T.Text)
            -> (T.Text -> Maybe a)
            -> ALens TicketList TicketList (Maybe a) (Maybe a)
            -> [a]
            -> TicketList
            -> OperationalState
            -> OperationalState
startDialog draw check place ini tl l =
  let ini_state = view (cloneLens place) tl
      state_t = draw <$> ini_state

      ac =
        mkAutocomplete
          ini
          (\t s -> filter (\v -> T.toLower t `T.isInfixOf` (T.toLower (draw v))) s)
          draw
          state_t
          (Dialog (MilestoneName False))
          (Dialog (MilestoneName True))

  in set typed (SearchParamsDialog check place ac) l

startStateDialog :: TicketList -> OperationalState -> OperationalState
startStateDialog =
  startDialog txtState
              (checkStateInput . T.toLower)
              (field @"params" . field @"gipState")
              [Open, Closed]
  where
    checkStateInput "open" = Just Open
    checkStateInput "closed" = Just Closed
    checkStateInput _ = Nothing

ticketListEnter :: TicketList
                -> OperationalState
                -> T.EventM Name (T.Next OperationalState)
ticketListEnter tl o = do
  let cursorTicket = view (field @"issues" . to L.listSelectedElement) tl
  case cursorTicket of
    Nothing -> M.continue o
    Just (_, t) -> do
      res <- liftIO (runExceptT (loadByIssueResp t (view (typed @AppConfig) o)) )
      case res of
        Left err -> M.continue (set typed (FooterMessage (T.pack (show err))) o)
        Right v -> M.continue . issueView o $ v

---
--
-- | Draw the ticket list page
drawTicketList :: OperationalState -> TicketList -> [Widget Name]
drawTicketList _ tl = [ui]
  where
    issues = view (field @"issues") tl

    params = view (field @"params") tl

    searchBox = drawSearchBox params

    --total = str $ show $ Vec.length $ view L.listElementsL issues

    box = B.border . vBox $ [
            L.renderList drawTicketRow True issues
            , B.hBorder
            , searchBox
            , B.hBorder
            ]

    ui = C.vCenter $ vBox [ C.hCenter box
                          ]

drawTicketRow :: Bool -> IssueResp -> Widget n
drawTicketRow _ IssueResp{..} =
    vLimit 1 $ hBox
        [ hLimit 6 $ padRight Max $ int (unIssueIid irIid)
        , padRight Max $ txt irTitle
        , padLeft (Pad 1) $ txt irState]

drawSearchBox :: GetIssuesParams -> Widget n
drawSearchBox GetIssuesParams{..} =
  vBox [ row "(S)tate" drawState gipState
       , row "S(c)ope" drawScope gipScope
       , row "(L)abels" drawLabelParam gipLabels
       , row "(M)ilestone" drawMilestoneParam gipMilestone
       , row "(A)uthor" drawAuthorParam gipAuthor
       , row "(O)wner" drawAssigneeParam gipAssignee
       , row "(W)eight" drawWeightParam gipWeight
       ]
  where
    row herald f w = txt herald <+> txt ": " <+> maybe emptyWidget f w

txtState :: StateParam -> T.Text
txtState Open = "Open"
txtState Closed = "Closed"

drawState :: StateParam -> Widget n
drawState = txt . txtState

txtScope :: ScopeParam -> T.Text
txtScope CreatedByMe  = "Created by me"
txtScope AssignedToMe = "Assigned to me"
txtScope AllScope = "All"

drawScope :: ScopeParam -> Widget n
drawScope = txt . txtScope

txtLabelParam :: LabelParam -> T.Text
txtLabelParam l =
  case l of
    WithLabels (Labels ts) -> T.intercalate ", " (S.toList ts)
    NoLabels -> "None"
    AnyLabel -> "Any"

drawLabelParam :: LabelParam -> Widget n
drawLabelParam = txt . txtLabelParam

txtMilestoneParam :: MilestoneParam -> T.Text
txtMilestoneParam m =
  case m of
    WithMilestone t -> t
    NoMilestone  -> "None"
    AnyMilestone -> "Any"

drawMilestoneParam :: MilestoneParam -> Widget n
drawMilestoneParam = txt . txtMilestoneParam

txtAuthorParam :: User -> T.Text
txtAuthorParam u = view (field @"userUsername") u

drawAuthorParam :: User -> Widget n
drawAuthorParam = txt . txtAuthorParam

txtAssigneeParam :: AssigneeParam -> T.Text
txtAssigneeParam a =
  case a of
    AssignedTo u -> view (field @"userUsername") u
    AssignedNone -> "None"
    AssignedAny -> "Any"

drawAssigneeParam :: AssigneeParam -> Widget n
drawAssigneeParam = txt . txtAssigneeParam

txtWeightParam :: Int -> T.Text
txtWeightParam = T.pack . show

drawWeightParam :: Int -> Widget n
drawWeightParam = txt . txtWeightParam
