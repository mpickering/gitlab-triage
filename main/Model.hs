{-# LANGUAGE DeriveGeneric #-}
module Model (module Model, module Config, module Namespace) where

import Namespace
import GitLab.Tickets
import Config

import qualified Brick.Widgets.List as L
import Brick.Forms
import Cursor.Text
import GHC.Generics
import Servant.Client



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
                          , labels :: [LabelResp]
                          , milestones :: [MilestoneResp]
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
                  } deriving Generic


data FooterMode = FooterInfo  -- Display generic info
                | FooterInput FooterInputMode TextCursor  -- Accept input
                deriving Generic

data FooterInputMode = Goto
                     | Title
                     | FLabels
                     | FMilestone
                     deriving Generic


data AppConfig = AppConfig {
                    userConfig :: UserConfig
                  , reqEnv :: ClientEnv
                  } deriving Generic
