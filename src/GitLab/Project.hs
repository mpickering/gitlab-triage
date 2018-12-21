{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Project where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client
import GitLab.Common

data Visibility = Private | Internal | Public
                deriving (Show)

instance ToJSON Visibility where
    toJSON Private  = "private"
    toJSON Internal = "internal"
    toJSON Public   = "public"

newtype SnippetId = SnippetId { getSnippetId :: Int }
                  deriving (Eq, Ord, Show, FromJSON)

----------------------------------------------------------------------
-- createSnippet
----------------------------------------------------------------------

data CreateSnippet
    = CreateSnippet { csTitle :: Text
                    , csFileName :: Text
                    , csDescription :: Maybe Text
                    , csCode :: Text
                    , csVisibility :: Visibility
                    }
    deriving (Show)

instance ToJSON CreateSnippet where
    toJSON CreateSnippet{..} = object
        [ "title" .= csTitle
        , "file_name" .=  csFileName
        , "description" .=  csDescription
        , "code" .= csCode
        , "visibility" .= csVisibility
        ]

data CreateSnippetResp = CreateSnippetResp SnippetId

instance FromJSON CreateSnippetResp where
    parseJSON = withObject "create snippet response" $ \o -> do
        CreateSnippetResp <$> o .: "id"

type CreateSnippetAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "snippets"
    :> SudoParam
    :> ReqBody '[JSON] CreateSnippet
    :> Post '[JSON] CreateSnippetResp

createSnippet :: AccessToken -> Maybe UserId -> ProjectId
              -> CreateSnippet -> ClientM SnippetId
createSnippet tok sudo prj cs = do
    CreateSnippetResp sid <- client (Proxy :: Proxy CreateSnippetAPI) (Just tok) prj sudo cs
    return sid

