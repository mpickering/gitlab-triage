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

import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Text as T

import Brick hiding (continue, halt)

import Control.Lens (view, to, set, Iso, iso, over)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
--import qualified Brick.Widgets.List as L
import qualified IOList as L
import qualified Brick.Widgets.Center as C
import Brick.Types ( Widget )
import Data.Generics.Product

import Control.Monad.IO.Class (liftIO)

import Text.Megaparsec
import Control.Applicative.Combinators ()
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Set as S

import Model
import Common
import Dialog
import Data.Semigroup.Foldable

import TextCursor

ticketListHandler :: TicketList -> Handler OperationalState
ticketListHandler tl l (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'q') [V.MCtrl] -> M.halt l
    V.EvKey V.KEnter [] -> ticketListEnter tl l
    V.EvKey (V.KChar 's') [] -> M.continue (startStateDialog tl l)
    V.EvKey (V.KChar 'c') [] -> M.continue (startScopeDialog tl l)
    V.EvKey (V.KChar 'l') [] -> M.continue (startLabelDialog tl l)
    V.EvKey (V.KChar 'm') [] -> M.continue (startMilestoneDialog tl l)
    V.EvKey (V.KChar 'a') [] -> M.continue (startAuthorDialog tl l)
    V.EvKey (V.KChar 'o') [] -> M.continue (startOwnerDialog tl l)
    V.EvKey (V.KChar 'w') [] -> M.continue (startWeightDialog tl l)
    V.EvKey (V.KChar '/') [] -> M.continue (startSearchDialog tl l)
    V.EvKey (V.KChar 'o') [V.MCtrl] -> M.continue =<< (liftIO $ toggleOrder tl l)
    V.EvKey (V.KChar 's') [V.MCtrl] -> M.continue =<< (liftIO $ toggleSort tl l)
    V.EvKey (V.KChar 'r') [] -> M.continue =<< (liftIO $ reload tl l)
    _ -> do
      res <- L.handleListEvent e (view typed tl)
      let tl' = set (field @"issues") res tl
      M.continue (set typed (TicketListView tl') l)
ticketListHandler _ l _ = M.continue l

reload :: TicketList -> OperationalState -> IO OperationalState
reload tl l = do
  let search_params = view (field  @"params") tl
  tl' <- loadTicketList search_params (view (typed @AppConfig) l)
  return $ set typed (TicketListView tl') l

toggleOrder :: TicketList -> OperationalState -> IO OperationalState
toggleOrder tl l = do
  let old_search_params = view (field  @"params") tl
      search_params = over (typed @Sort) toggle old_search_params
      toggle Asc = Desc
      toggle Desc = Asc
  tl' <- loadTicketList search_params (view (typed @AppConfig) l)
  return $ set typed (TicketListView tl') l

toggleSort :: TicketList -> OperationalState -> IO OperationalState
toggleSort tl l = do
  let old_search_params = view (field @"params") tl
      search_params = over (typed @Order) toggle old_search_params
      toggle Created = Updated
      toggle Updated = Created
  tl' <- loadTicketList search_params (view (typed @AppConfig) l)
  return $ set typed (TicketListView tl') l

startScopeDialog, startLabelDialog,
  startMilestoneDialog, startAuthorDialog,
  startOwnerDialog, startWeightDialog, startSearchDialog
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

data LabelOption = NoneOption | AnyOption | LabelOption T.Text

startLabelDialog tl l =
  startMultiDialogIO @TicketList
                     txtLabelOption
                     (return . checkLabel)
                     (pureRestrict txtLabelOption)
                     (field @"params" . field @"gipLabels" . paramIso)
                     (view (field @"params" . field @"gipLabels" . paramIso) tl)
                     ([NoneOption, AnyOption] ++ ls) l
  where
    toOptions :: LabelParam -> [LabelOption]
    toOptions NoLabels = [NoneOption]
    toOptions AnyLabel = [AnyOption]
    toOptions (WithLabels (Labels labs)) = map LabelOption (S.toList labs)

    fromOptions :: LabelOption -> LabelParam
    fromOptions NoneOption = NoLabels
    fromOptions AnyOption  = AnyLabel
    fromOptions (LabelOption t) = WithLabels (mkLabel t)

    paramIso :: Iso (Maybe LabelParam) (Maybe LabelParam) [LabelOption]
                                                          [LabelOption]
    paramIso = iso (maybe [] toOptions) (\os -> case os of
                                                 [] -> Nothing
                                                 (x:xs) -> Just (foldMap1 fromOptions (x :| xs)))

    ls = LabelOption . view (field @"lrName")
          <$> view (field @"labels") l

    checkLabel :: T.Text -> Maybe LabelOption
    checkLabel t =
      case (T.toLower t) of
        "none" -> Just NoneOption
        "any"  -> Just AnyOption
        _      ->  Just (LabelOption t)

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
  startDialogIO txtAuthorParam (checkAuthor ac) (restrictAuthor ac)
                   (field @"params" . field @"gipAuthor")
              us tl l
  where
    us = view (field @"users") l

    ac = view (typed @AppConfig) l



startOwnerDialog tl l =
  startDialogIO txtAssigneeParam checkAssignee
              restrict
              (field @"params" . field @"gipAssignee")
              ini tl l
  where
    us = view (field @"users") l

    as = map AssignedTo us

    ini = ([AssignedNone, AssignedAny] ++ as)

    restrict t _=
      ([AssignedNone, AssignedAny] ++) .
        map AssignedTo <$> restrictAuthor ac t us

    ac = view (typed @AppConfig) l

    checkAssignee t =
      case T.toLower t of
        "none" -> return $ Just AssignedNone
        "any"  -> return $ Just AssignedAny
        _      -> fmap AssignedTo <$> checkAuthor ac t

startWeightDialog tl l =
  startDialog txtWeightParam checkWeight (field @"params" . field @"gipWeight")
            [0..100] tl l
  where
    checkWeight :: T.Text -> Maybe Int
    checkWeight t = parseMaybe @() decimal t

startSearchDialog tl l =
  startDialog id checkSearch (field @"params" . field @"gipSearch")
            [] tl l
  where
    checkSearch :: T.Text -> Maybe T.Text
    checkSearch t = Just t

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
  cursorTicket <- liftIO $ view (field @"issues" . to L.listSelectedElement) tl
  case cursorTicket of
    Nothing -> M.continue o
    Just (_, t) -> do
      displayError (loadByIssueResp t (view (typed @AppConfig) o))
                   (\v -> M.continue . issueView o $ IssuePage tl v)
                   o

---
--
-- | Draw the ticket list page
drawTicketList :: OperationalState -> TicketList -> [Widget Name]
drawTicketList l tl = [ui]
  where
    issues = view (field @"issues") tl

    params = view (field @"params") tl

    searchBox = drawSearchBox params

    footer im =
      case im of
      FooterInput fim t -> txt (formatFooterMode fim) <+> drawTextCursor t
      _ -> txt "g - goto ticket; C-q - quit"

    kAndL = L.curAndLen issues
    numbers = case kAndL of
                Just (k,len) -> hBox [int (k + 1), str "/", int len]
                Nothing -> emptyWidget

    total = numbers <+> drawSearchKeys

    drawSearchKeys = hBox [str "(", drawOrder (view (typed @Sort) params)
                                  , str ","
                                  , drawKey (view (typed @Order) params)
                                  , str ")" ]

    drawOrder Desc = str "↓"
    drawOrder Asc = str "↑"

    drawKey Created = str "C"
    drawKey Updated = str "U"

    box = B.borderWithLabel total . vBox $ [
            L.renderList drawTicketRow True issues
            --, str $ "Loaded Elems:"
            --    ++ (show (length (L.toListPure (view L.listElementsL issues))))
            , B.hBorder
            , searchBox
            , B.hBorder
            , footer (view (typed @FooterMode) l)
            ]

    ui = C.vCenter $ vBox [ C.hCenter box
                          ]

drawTicketRow :: Bool -> IssueResp -> Widget n
drawTicketRow _ IssueResp{..} =
    withAttr state_attr $ vLimit 1 $ hBox
        [ hLimit 6 $ padRight Max $ int (unIssueIid irIid)
        , padRight Max $ txt irTitle ]
--        , padLeft (Pad 1) $ txt irState]
  where
    state_attr = attrName (T.unpack irState)

drawSearchBox :: GetIssuesParams -> Widget n
drawSearchBox GetIssuesParams{..} =
  vBox [ row "(S)tate" drawState gipState
       , row "S(c)ope" drawScope gipScope
       , row "(L)abels" drawLabelParam gipLabels
       , row "(M)ilestone" drawMilestoneParam gipMilestone
       , row "(A)uthor" drawAuthorParam gipAuthor
       , row "(O)wner" drawAssigneeParam gipAssignee
       , row "(W)eight" drawWeightParam gipWeight
       , row "(/)earch" drawSearchParam gipSearch
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

txtLabelOption :: LabelOption -> T.Text
txtLabelOption l =
  case l of
    LabelOption t -> t
    NoneOption -> "None"
    AnyOption -> "Any"


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

drawSearchParam :: T.Text -> Widget n
drawSearchParam = txt
