{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module GitLab.Tickets where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client.Free
import GitLab.Common
import GHC.Generics
import GitLab.Users
import qualified Data.ByteString.Char8 as B
import Text.Read

import Debug.Trace

----------------------------------------------------------------------
-- getIssue
----------------------------------------------------------------------

data IssueResp
    = IssueResp { irProjectId :: ProjectId
                , irIid :: IssueIid
                , irMilestone :: Maybe MilestoneResp
                , irAuthor :: User
                , irDescription :: Text
                , irState :: Text
                , irAssignees :: [User]
                , irLabels :: Labels
                , irTitle :: Text
                , irUpdatedAt :: Text
                , irClosedAt :: Maybe Text
                , irCreatedAt :: Text
                , irWeight :: Maybe Int
                }
    deriving (Show, Generic)

{-
[
   {
      "project_id" : 4,
      "milestone" : {
         "due_date" : null,
         "project_id" : 4,
         "state" : "closed",
         "description" : "Rerum est voluptatem provident consequuntur molestias similique ipsum dolor.",
         "iid" : 3,
         "id" : 11,
         "title" : "v3.0",
         "created_at" : "2016-01-04T15:31:39.788Z",
         "updated_at" : "2016-01-04T15:31:39.788Z"
      },
      "author" : {
         "state" : "active",
         "web_url" : "https://gitlab.example.com/root",
         "avatar_url" : null,
         "username" : "root",
         "id" : 1,
         "name" : "Administrator"
      },
      "description" : "Omnis vero earum sunt corporis dolor et placeat.",
      "state" : "closed",
      "iid" : 1,
      "assignees" : [{
         "avatar_url" : null,
         "web_url" : "https://gitlab.example.com/lennie",
         "state" : "active",
         "username" : "lennie",
         "id" : 9,
         "name" : "Dr. Luella Kovacek"
      }],
      "assignee" : {
         "avatar_url" : null,
         "web_url" : "https://gitlab.example.com/lennie",
         "state" : "active",
         "username" : "lennie",
         "id" : 9,
         "name" : "Dr. Luella Kovacek"
      },
      "labels" : [],
      "id" : 41,
      "title" : "Ut commodi ullam eos dolores perferendis nihil sunt.",
      "updated_at" : "2016-01-04T15:31:46.176Z",
      "created_at" : "2016-01-04T15:31:46.176Z",
      "closed_at" : "2016-01-05T15:31:46.176Z",
      "closed_by" : {
         "state" : "active",
         "web_url" : "https://gitlab.example.com/root",
         "avatar_url" : null,
         "username" : "root",
         "id" : 1,
         "name" : "Administrator"
      },
      "user_notes_count": 1,
      "due_date": "2016-07-22",
      "web_url": "http://example.com/example/example/issues/1",
      "confidential": false,
      "weight": null,
      "discussion_locked": false,
      "time_stats": {
         "time_estimate": 0,
         "total_time_spent": 0,
         "human_time_estimate": null,
         "human_total_time_spent": null
      },
   }
]
-}


type Scope = Text

{-
                , irMilestone :: Milestone
                , irAuthor :: Author
                , irDescription :: Text
                , irState :: Text
                , irAssignees :: [Assignee]
                , irLabels :: [Text]
                , irTitle :: Text
                , irUpdatedAt :: Text
                , irClosedAt :: Text
                , irCreatedAt :: Text
                }
                -}


instance FromJSON IssueResp where
    parseJSON = withObject "issue response" $ \o ->
      IssueResp <$> o .: "project_id"
                <*> o .: "iid"
                <*> o .: "milestone"
                <*> o .: "author"
                <*> o .: "description"
                <*> o .: "state"
                <*> o .: "assignees"
                <*> o .: "labels"
                <*> o .: "title"
                <*> o .: "updated_at"
                <*> o .: "closed_at"
                <*> o .: "created_at"
                <*> o .: "weight"

data GetIssuesParams
  = GetIssuesParams
      { gipState :: Maybe StateParam
      , gipLabels :: Maybe LabelParam
      , gipMilestone :: Maybe MilestoneParam
      , gipScope :: Maybe ScopeParam
      , gipAuthor :: Maybe User
      , gipAssignee :: Maybe AssigneeParam
      , gipWeight :: Maybe Int
      , gipSearch :: Maybe Text
      , gipSort :: Sort
      , gipOrder :: Order
      } deriving (Generic, Show)

type Sort = AscDesc

data Order = Created | Updated deriving (Generic, Show)

instance ToHttpApiData Order where
  toQueryParam Created = "created_at"
  toQueryParam Updated = "updated_at"
  toUrlPiece Created = "created_at"
  toUrlPiece Updated = "updated_at"

defaultSearchParams :: GetIssuesParams
defaultSearchParams =
  GetIssuesParams
    Nothing
    Nothing
    Nothing
    (Just AllScope)
    Nothing
    Nothing
    Nothing
    Nothing
    Desc
    Created


data StateParam = Open | Closed deriving (Generic, Show)

instance ToHttpApiData StateParam where
  toQueryParam Open = "opened"
  toQueryParam Closed = "closed"
  toUrlPiece = toQueryParam

data LabelParam = WithLabels Labels | NoLabels | AnyLabel
  deriving (Generic, Show)

instance Semigroup LabelParam where
  NoLabels <> _ = NoLabels
  AnyLabel <> _ = AnyLabel
  _ <> NoLabels = NoLabels
  _ <> AnyLabel = AnyLabel
  WithLabels xs <> WithLabels ys = WithLabels (xs <> ys)


instance ToHttpApiData LabelParam where
    toQueryParam (WithLabels ls) = toQueryParam ls
    toQueryParam NoLabels = "None"
    toQueryParam AnyLabel = "Any"

data MilestoneParam = WithMilestone Text | NoMilestone | AnyMilestone
  deriving (Generic, Show)

instance ToHttpApiData MilestoneParam where
    toQueryParam (WithMilestone t) =  t
    toQueryParam NoMilestone = "None"
    toQueryParam AnyMilestone = "Any"

data ScopeParam = CreatedByMe | AssignedToMe | AllScope
  deriving (Generic, Show)

instance ToHttpApiData ScopeParam where
  toQueryParam CreatedByMe = "created_by_me"
  toQueryParam AssignedToMe = "assigned_to_me"
  toQueryParam AllScope = "all"

data AssigneeParam = AssignedTo User | AssignedNone | AssignedAny
  deriving (Generic, Show)

instance ToHttpApiData AssigneeParam where
  toQueryParam (AssignedTo user) = toQueryParam (userId user)
  toQueryParam AssignedNone = "None"
  toQueryParam AssignedAny  = "Any"


type GetIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> QueryParam "scope" ScopeParam
    :> QueryParam "state" StateParam
    :> QueryParam "labels" LabelParam
    :> QueryParam "milestone" MilestoneParam
    :> QueryParam "author_id" UserId
    :> QueryParam "assignee_id" AssigneeParam
    :> QueryParam "weight" Int
    :> QueryParam "search" Text
    :> QueryParam "order_by" Order
    :> QueryParam "sort" Sort
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> Get '[JSON] ((Headers '[Header "X-Total-Pages" Int
                              , Header "X-Total" Int
                              , Header "X-Page" Int
                              , Header "X-Next-Page" Int
                              , Header "X-Per-Page" Int] [IssueResp]))

data GetIssueHeaders = GetIssueHeaders {
                        total_pages :: Int
                        , total :: Int
                        , page :: Int
                        , next_page :: (Maybe Int)
                        , per_page :: Int } deriving Show


getIssues :: GetIssuesParams
          -> Maybe Int
          -> AccessToken
          -> ProjectId
          -> ClientM (GetIssueHeaders, [IssueResp])
getIssues GetIssuesParams{..} mb_page tok prj = do
  res <- client (Proxy :: Proxy GetIssueAPI) (Just tok) prj
      gipScope
      gipState
      gipLabels
      gipMilestone
      (userId <$> gipAuthor)
      gipAssignee
      gipWeight
      gipSearch
      (Just gipOrder)
      (Just gipSort)
      mb_page
      (Just 100)
  let hs = getHeaders res

      read_header_m s =
        let p = fromMaybe "1" $ lookup s hs
        in readMaybe (B.unpack p) :: Maybe Int

      read_header s =
        case read_header_m s of
              Just r -> r
              Nothing -> error (show s)




      gih = GetIssueHeaders (read_header "X-Total-Pages")
                            (read_header "X-Total")
                            (read_header "X-Page")
                            (read_header_m "X-Next-Page")
                            (read_header "X-Per-Page")
  return (gih, getResponse res)





type GetOneIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Get '[JSON] IssueResp

getOneIssue :: IssueIid -> AccessToken -> ProjectId -> ClientM IssueResp
getOneIssue iid tok prj =
  client (Proxy :: Proxy GetOneIssueAPI) (Just tok) prj iid

----------------------------------------------------------------------
-- createIssue
----------------------------------------------------------------------

data CreateIssue
    = CreateIssue { ciIid :: Maybe IssueIid
                  , ciTitle :: Text
                  , ciLabels :: Maybe Labels
                  , ciCreatedAt :: Maybe UTCTime
                  , ciDescription :: Maybe Text
                  , ciMilestoneId :: Maybe (Maybe MilestoneId)
                  , ciWeight :: Maybe Weight
                  , ciAssignees :: Maybe [UserId]
                  }
                  deriving (Show)

instance ToJSON CreateIssue where
    toJSON CreateIssue{..} = object
        [ "iid" .= ciIid
        , "title" .= ciTitle
        , "labels" .= ciLabels
        , "created_at" .= ciCreatedAt
        , "description" .= ciDescription
        , "milestone_id" .= ciMilestoneId
        , "weight" .= ciWeight
        , "assignee_ids" .= ciAssignees
        ]

type CreateIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> ReqBody '[JSON] CreateIssue
    :> SudoParam
    :> Post '[JSON] IssueResp

createIssue :: AccessToken -> Maybe UserId
            -> ProjectId -> CreateIssue -> ClientM IssueResp
createIssue tok sudo prj ci =
    client (Proxy :: Proxy CreateIssueAPI) (Just tok) prj ci sudo

----------------------------------------------------------------------
-- editIssue
----------------------------------------------------------------------

data EditIssue
    = EditIssue { eiTitle       :: Maybe Text
                , eiDescription :: Maybe Text
                , eiMilestone :: Maybe (Maybe MilestoneResp)
                , eiLabels      :: Maybe Labels
                , eiStatus      :: Maybe StatusEvent
                , eiUpdateTime  :: Maybe UTCTime
                , eiWeight      :: Maybe (Maybe Int)
                , eiAssignees   :: Maybe [User]
                , eiKeywords    :: Maybe [Text]
                }
    deriving (Show, Generic)

noEdits :: EditIssue
noEdits = EditIssue Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                    Nothing Nothing

instance ToJSON EditIssue where
    toJSON EditIssue{..} = object
        $ catMaybes
        [ "title" .=? eiTitle
        , "description" .=? eiDescription
        , "milestone_id" .=? fmap (fmap mrId) (eiMilestone)
        , "labels" .=? eiLabels
        , "state_event" .=? eiStatus
        , "updated_at" .=? eiUpdateTime
        , "weight" .=? eiWeight
        , "assignee_ids" .=? (map userId <$> eiAssignees)
        , "keywords" .=? eiKeywords
        ]

type EditIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> ReqBody '[JSON] EditIssue
    :> SudoParam
    :> Put '[JSON] IssueResp

nullEditIssue :: EditIssue -> Bool
nullEditIssue (EditIssue a b c d e _ g h i) =
    isNothing a && isNothing b && isNothing c && isNothing d &&
    isNothing e && isNothing g && isNothing h &&
    isNothing i
    -- N.B. Ignore update time and labels

editIssue :: AccessToken -> Maybe UserId
          -> ProjectId -> IssueIid -> EditIssue -> ClientM IssueResp
editIssue tok sudo prj iid ei =
    client (Proxy :: Proxy EditIssueAPI) (Just tok) prj iid ei sudo

----------------------------------------------------------------------
-- createIssueNote
----------------------------------------------------------------------

data CreateIssueNote
    = CreateIssueNote { cinBody :: Text
                      , cinCreatedAt :: Maybe UTCTime
                      } deriving Generic

instance ToJSON CreateIssueNote where
    toJSON CreateIssueNote{..} = object
        [ "body" .= cinBody
        , "created_at" .= cinCreatedAt
        ]

data IssueNoteResp
    = IssueNoteResp { inrId :: Int
                    , inrBody :: Text
                    , inrAuthor :: User
                    , inrCreatedAt :: Text
                    , inrUpdatedAt :: Text
                    , inrSystem :: Bool
                    }
    deriving (Show, Generic)

instance FromJSON IssueNoteResp where
    parseJSON = withObject "issue note response" $ \o ->
      IssueNoteResp <$> o .: "id"
                    <*> o .: "body"
                    <*> o .: "author"
                    <*> o .: "created_at"
                    <*> o .: "updated_at"
                    <*> o .: "system"

type CreateIssueNoteAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid :> "notes"
    :> ReqBody '[JSON] CreateIssueNote
    :> SudoParam
    :> Post '[JSON] IssueNoteResp

createIssueNote :: AccessToken
                -> Maybe UserId
                -> ProjectId
                -> IssueIid
                -> CreateIssueNote -> ClientM IssueNoteResp
createIssueNote tok sudo prj iis cin =
    client (Proxy :: Proxy CreateIssueNoteAPI) (Just tok) prj iis cin sudo

----------------------------------------------------------------------
-- listIssueNotes
----------------------------------------------------------------------

data AscDesc = Desc | Asc
  deriving (Ord, Eq, Enum, Bounded, Show)

instance ToHttpApiData AscDesc where
  toQueryParam Asc = "asc"
  toQueryParam Desc = "desc"
  toUrlPiece Asc = "asc"
  toUrlPiece Desc = "desc"

type ListIssueNotesAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid :> "notes"
    :> QueryParam "sort" AscDesc
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> SudoParam
    :> Get '[JSON] [IssueNoteResp]

getNewestIssueNote :: AccessToken
                   -> Maybe UserId
                   -> ProjectId
                   -> IssueIid
                   -> ClientM (Maybe IssueNoteResp)
getNewestIssueNote tok sudo proj iis =
  listToMaybe <$> getNewestIssueNotes tok sudo proj iis 1

getNewestIssueNotes :: AccessToken
                    -> Maybe UserId
                    -> ProjectId
                    -> IssueIid
                    -> Int
                    -> ClientM [IssueNoteResp]
getNewestIssueNotes tok sudo prj iis pagesize =
    client (Proxy :: Proxy ListIssueNotesAPI)
      (Just tok) prj iis (Just Desc) (Just 0) (Just pagesize) sudo

listIssueNotesPage :: AccessToken
                   -> Maybe UserId
                   -> ProjectId
                   -> IssueIid
                   -> Int
                   -> ClientM [IssueNoteResp]
listIssueNotesPage tok sudo prj iis page =
    client (Proxy :: Proxy ListIssueNotesAPI)
      (Just tok) prj iis (Just Asc) (Just page) (Just 100) sudo

listIssueNotes :: AccessToken
               -> Maybe UserId
               -> ProjectId
               -> IssueIid
               -> ClientM [IssueNoteResp]
listIssueNotes tok sudo proj iis =
  go 1
  where
    go n = do
      notes <- listIssueNotesPage tok sudo proj iis n
      case notes of
        [] -> return []
        _ -> do
          moreNotes <- go (succ n)
          return $ notes ++ moreNotes

----------------------------------------------------------------------
-- deleteIssue
----------------------------------------------------------------------

type DeleteIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> SudoParam
    :> Delete '[] NoContent

deleteIssue :: AccessToken -> Maybe UserId -> ProjectId -> IssueIid -> ClientM ()
deleteIssue tok sudo prj iid = void $ client (Proxy :: Proxy DeleteIssueAPI) (Just tok) prj iid sudo

----------------------------------------------------------------------
-- createMilestone
----------------------------------------------------------------------

type CreateMilestoneAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "milestones"
    :> ReqBody '[JSON] CreateMilestone
    :> SudoParam
    :> Post '[JSON] CreateMilestoneResp

data CreateMilestone
    = CreateMilestone { cmTitle :: Text
                      , cmDescription :: Text
                      , cmDueDate :: Maybe UTCTime
                      , cmStartDate :: Maybe UTCTime
                      }
                      deriving (Show)

instance ToJSON CreateMilestone where
    toJSON CreateMilestone{..} = object
        [ "title" .= cmTitle
        , "description" .= cmDescription
        , "due_date" .= cmDueDate
        , "start_date" .= cmStartDate
        ]

data CreateMilestoneResp = CreateMilestoneResp MilestoneId

instance FromJSON CreateMilestoneResp where
    parseJSON = withObject "create milestone response" $ \o -> do
        CreateMilestoneResp <$> o .: "id"

createMilestone :: AccessToken -> Maybe UserId
                -> ProjectId -> CreateMilestone
                -> ClientM MilestoneId
createMilestone tok sudo prj cm = do
    CreateMilestoneResp mid <- client (Proxy :: Proxy CreateMilestoneAPI) (Just tok) prj cm sudo
    return mid

----------------------------------------------------------------------
-- listMilestones
----------------------------------------------------------------------

type ListMilestonesAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "milestones"
    :> QueryParam "per_page" Int
    :> Get '[JSON] [Milestone]

data Milestone = Milestone Text MilestoneId deriving Show

instance FromJSON Milestone where
    parseJSON = withObject "milestone" $ \o -> do
        Milestone <$> o .: "title" <*> o .: "id"

listMilestones :: AccessToken
               -> ProjectId -> ClientM [Milestone]
listMilestones tok prj = client (Proxy :: Proxy ListMilestonesAPI) (Just tok) prj (Just 100)


----------------------------------------------------------------------
-- createIssueLink
----------------------------------------------------------------------

type CreateIssueLinkAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "issue_iid" IssueIid :> "links"
    :> ReqBody '[JSON] CreateIssueLink
    :> SudoParam
    :> Post '[JSON] CreateIssueLinkResp

data CreateIssueLink
    = CreateIssueLink { cilProject :: ProjectId
                      , cilIssue :: IssueIid
                      , cilTargetProject :: ProjectId
                      , cilTargetIssue :: IssueIid
                      }
                      deriving (Show)

instance ToJSON CreateIssueLink where
    toJSON CreateIssueLink{..} = object
        [ "id" .= cilProject
        , "issue_iid" .= cilIssue
        , "target_project_id" .= cilTargetProject
        , "target_issue_iid" .= cilTargetIssue
        ]

data CreateIssueLinkResp = CreateIssueLinkResp IssueLinkId

instance FromJSON CreateIssueLinkResp where
    parseJSON = withObject "create issueLink response" $ \o -> do
        CreateIssueLinkResp <$> o .: "id"

createIssueLink :: AccessToken -> Maybe UserId
                -> ProjectId -> IssueIid
                -> CreateIssueLink
                -> ClientM IssueLinkId
createIssueLink tok sudo prj iid cm = do
    CreateIssueLinkResp mid <- client (Proxy :: Proxy CreateIssueLinkAPI) (Just tok) prj iid cm sudo
    return mid

----------------------------------------------------------------------
-- listIssueLinks
----------------------------------------------------------------------

type ListIssueLinksAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues" :> Capture "iid" IssueIid :> "links"
    :> QueryParam "per_page" Int
    :> Get '[JSON] [IssueResp]

data IssueLink = IssueLink IssueLinkId ProjectId IssueIid

instance FromJSON IssueLink where
    parseJSON = withObject "issueLink" $ \o -> do
        IssueLink <$> o .: "issue_link_id"
                  <*> o .: "project_id"
                  <*> o .: "iid"

listIssueLinks :: AccessToken
               -> ProjectId
               -> IssueIid
               -> ClientM [IssueResp]
listIssueLinks tok prj iid = client (Proxy :: Proxy ListIssueLinksAPI) (Just tok) prj iid (Just 100)

----------------------------------------------------------------------
-- subscribeIssue
----------------------------------------------------------------------

type SubscribeIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "issue_iid" IssueIid :> "subscribe"
    :> ReqBody '[JSON] SubscribeIssue
    :> SudoParam
    :> Post '[JSON] SubscribeIssueResp

data SubscribeIssue
    = SubscribeIssue
        deriving (Show)

instance ToJSON SubscribeIssue where
    toJSON SubscribeIssue = object
        []

data SubscribeIssueResp = SubscribeIssueResp

instance FromJSON SubscribeIssueResp where
    parseJSON = withObject "subscribe response" $ \o -> do
        pure SubscribeIssueResp

subscribeIssue :: AccessToken
                -> Maybe UserId
                -> ProjectId
                -> IssueIid
                -> ClientM ()
subscribeIssue tok sudo prj iid = do
    SubscribeIssueResp <- client (Proxy :: Proxy SubscribeIssueAPI) (Just tok) prj iid SubscribeIssue sudo
    return ()

---
--
--

type GetLabels =
  GitLabRoot :> "projects"
  :> Capture "id" ProjectId :> "labels"
  :> QueryParam "page" Int
  :> QueryParam "per_page" Int
  :> Get '[JSON] ((Headers '[Header "X-Total-Pages" Int
                            , Header "X-Total" Int
                            , Header "X-Page" Int
                            , Header "X-Next-Page" Int
                            , Header "X-Per-Page" Int] [LabelResp]))

data LabelResp = LabelResp { lrName :: Text
                           } deriving Generic

instance FromJSON LabelResp where
  parseJSON = withObject "label response" $ \o -> do
                LabelResp <$> o .: "name"

getLabels :: AccessToken -> ProjectId -> ClientM [LabelResp]
getLabels tok prj
  = do
  let q n = client (Proxy :: Proxy GetLabels) (Just tok) prj (Just n) (Just 100)
  res <- q 1
  let hs = getHeaders res

      read_header_m s =
        let p = fromMaybe "1" $ lookup s hs
        in readMaybe (B.unpack p) :: Maybe Int

      read_header s =
        case read_header_m s of
              Just r -> r
              Nothing -> error (show s)
      loop lim n | lim < n = return []
      loop lim n = (++) <$> (getResponse <$> q n) <*> loop lim (n + 1)


  res2 <- loop 2 (read_header "X-Total-Pages")
  return (getResponse res ++ res2)



type GetMilestones =
  GitLabRoot :> "projects"
  :> Capture "id" ProjectId :> "milestones"
  :> QueryParam "state" Text
  :> Get '[JSON] [MilestoneResp]

data MilestoneResp = MilestoneResp { mrTitle :: Text
                                   , mrDescription :: Text
                                   , mrId :: MilestoneId
                                   } deriving (Generic, Show)

instance FromJSON MilestoneResp where
  parseJSON = withObject "label response" $ \o -> do
                MilestoneResp <$> o .: "title"
                          <*> o .: "description"
                          <*> o .: "id"

getMilestones :: AccessToken -> ProjectId -> ClientM [MilestoneResp]
getMilestones tok prj
  = client (Proxy :: Proxy GetMilestones) (Just tok) prj (Just "active")



