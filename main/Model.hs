{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Model (module Model, module Config, module Namespace) where

import Namespace
import GitLab.Tickets
import GitLab.Users
import Config

import qualified Brick.Widgets.List as L
import Brick.Forms
import Cursor.Text
import GHC.Generics
import Servant.Client
import Autocomplete
import Control.Lens (ALens)
import Data.Text (Text)

import qualified IOList



data AppState = AppState { majorMode :: MajorMode
                         } deriving Generic

-- TODO: Make a separte SetupState type
data MajorMode = Setup (Form UserConfig () Name) FilePath
               | Operational OperationalState deriving Generic

-- State of the main part of the app, mode specific state
-- is stored in `Mode`.
data OperationalState = OperationalState {
                            mode :: Mode
                          , footerMode :: FooterMode
                          , overlayDialog :: DialogMode
                          , labels :: [LabelResp]
                          , milestones :: [MilestoneResp]
                          , users :: [User]
                          , config :: AppConfig
                          } deriving Generic

data Mode = TicketListView TicketList
          | IssueView IssuePage
          deriving Generic

data TicketList = TicketList {
                    issues :: IOList.IOListWidget Name IssueResp
                    , params :: GetIssuesParams
                    } deriving Generic

data IssuePage = IssuePage {
                  issueNotes :: L.List Name IssueNoteResp
                  , currentIssue :: IssueResp
                  , updates :: Updates
                  , links :: [IssueResp]
                  } deriving Generic

data Updates = Updates { comment :: Maybe CreateIssueNote
                       , metainfo :: EditIssue } deriving Generic


data FooterMode = FooterInfo  -- Display generic info
                | FooterMessage Text
                | FooterInput FooterInputMode TextCursor  -- Accept input
                deriving Generic

data FooterInputMode = FGoto
                      | forall a . FGen Text
                                       (Text -> Maybe a)
                                       (ALens EditIssue EditIssue (Maybe a) (Maybe a))

type MilestoneAutocomplete = Autocomplete [MilestoneResp] Name MilestoneResp
type OwnerAutocomplete     = Autocomplete [User] Name User

type AppAutocomplete a = Autocomplete [a] Name a

data DialogMode where
  NoDialog :: DialogMode
  IssuePageDialog ::
    (Text -> IO (Maybe a)) ->
    (ALens IssuePage IssuePage (Maybe a) (Maybe a)) ->
    (AppAutocomplete a) -> DialogMode
  SearchParamsDialog ::
    (Text -> IO (Maybe a)) ->
    (ALens TicketList TicketList (Maybe a) (Maybe a)) ->
    (AppAutocomplete a) ->
    DialogMode


data AppConfig = AppConfig {
                    userConfig :: UserConfig
                  , reqEnv :: ClientEnv
                  } deriving Generic
