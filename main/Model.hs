{-# LANGUAGE DeriveGeneric #-}
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
                    issues :: L.List Name IssueResp
                    } deriving Generic

data IssuePage = IssuePage {
                  issueNotes :: L.List Name IssueNoteResp
                  , currentIssue :: IssueResp
                  , updates :: EditIssue
                  , links :: [IssueResp]
                  } deriving Generic


data FooterMode = FooterInfo  -- Display generic info
                | FooterInput FooterInputMode TextCursor  -- Accept input
                deriving Generic

data FooterInputMode = FGoto
                     | FTitle
                     | FLabels
                     | FMilestone
                     | FWeight
                     deriving Generic

type MilestoneAutocomplete = Autocomplete [MilestoneResp] Name MilestoneResp
type OwnerAutocomplete     = Autocomplete [User] Name User

data DialogMode = NoDialog
                | MilestoneDialog MilestoneAutocomplete
                | OwnerDialog     OwnerAutocomplete
                deriving Generic


data AppConfig = AppConfig {
                    userConfig :: UserConfig
                  , reqEnv :: ClientEnv
                  } deriving Generic
