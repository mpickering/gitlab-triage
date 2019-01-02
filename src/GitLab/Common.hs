{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module GitLab.Common where

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
import GHC.Generics
import Control.Monad.Free

type ClientM a = Free ClientF a


(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
key .=? Nothing = Nothing
key .=? Just x = Just $ key .= toJSON x

newtype UserId = UserId { getUserId :: Int }
               deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Weight = Weight Int
               deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype AccessToken = AccessToken Text
                    deriving (Eq, Ord, Show, ToHttpApiData, IsString, Generic, ToJSON, FromJSON)


newtype MilestoneId = MilestoneId Int
                    deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype IssueLinkId = IssueLinkId Int
                    deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype ProjectId = ProjectId Int
                  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData, Generic)

newtype Labels = Labels (S.Set Text)
               deriving (Semigroup, Monoid, Show)


instance ToHttpApiData Labels where
  toQueryParam (Labels s) = T.intercalate "," (S.toList s)

data StatusEvent = CloseEvent | ReopenEvent
                 deriving (Show, Generic)

toggleStatus :: Text -> StatusEvent
toggleStatus "closed" = ReopenEvent
toggleStatus "opened" = CloseEvent
toggleStatus s = error $ "Not Handled:" ++ show s

instance ToJSON StatusEvent where
    toJSON CloseEvent  = "close"
    toJSON ReopenEvent = "reopen"

mkLabel :: Text -> Labels
mkLabel = Labels . S.singleton

instance IsString Labels where
    fromString = mkLabel . T.pack

instance ToJSON Labels where
    toJSON (Labels lbls) = toJSON $ T.intercalate "," (S.toList lbls)

instance FromJSON Labels where
    parseJSON o = Labels . S.fromList <$> (parseJSON @([Text]) o)

newtype IssueIid = IssueIid { unIssueIid :: Int }
                 deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

type GitLabRoot = Header "Private-Token" AccessToken

type SudoParam = QueryParam "sudo" UserId
