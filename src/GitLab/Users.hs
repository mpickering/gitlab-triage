{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module GitLab.Users where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
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
import Servant.API.ResponseHeaders
import GitLab.Common
import GHC.Generics
import qualified Data.ByteString.Char8 as B

----------------------------------------------------------------------
-- createUser
----------------------------------------------------------------------

data CreateUser
    = CreateUser { cuEmail       :: Text
                 , cuUsername    :: Text
                 , cuName        :: Text
                 , cuSkipConfirmation :: Bool
                 }
    deriving (Show)

instance ToJSON CreateUser where
    toJSON CreateUser{..} = object
        [ "email" .= cuEmail
        , "username" .=  cuUsername
        , "name" .=  cuName
        , "reset_password" .= True
        , "skip_confirmation" .= cuSkipConfirmation
        ]

data User = User { userId       :: UserId
                 , userUsername :: Text
                 , userName     :: Text
                 , userEmail    :: Maybe Text
                 }
         deriving (Show, Generic)

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        User <$> o .: "id"
             <*> o .: "username"
             <*> o .: "name"
             <*> o .:? "email"

type CreateUserAPI =
    GitLabRoot :> "users"
    :> ReqBody '[JSON] CreateUser
    :> Post '[JSON] User

createUser :: AccessToken -> CreateUser -> ClientM UserId
createUser tok cu = do
    user <- client (Proxy :: Proxy CreateUserAPI) (Just tok) cu
    return $ userId user



----------------------------------------------------------------------
-- findUserByUsername
----------------------------------------------------------------------

type FindUserByUsernameAPI =
    GitLabRoot :> "users"
    :> QueryParam "username" Text
    :> Get '[JSON] [User]

findUsersByUsername :: AccessToken -> Text -> ClientM [User]
findUsersByUsername tok username =
    client (Proxy :: Proxy FindUserByUsernameAPI) (Just tok) (Just username)

findUserByUsername :: AccessToken -> Text -> ClientM (Maybe User)
findUserByUsername tok username = do
    res <- findUsersByUsername tok username
    return $ case res of
               [] -> Nothing
               [user] -> Just user
               _ -> error $ "Multiple users with id "<>show username

----------------------------------------------------------------------
-- findUserByEmail
----------------------------------------------------------------------

type FindUserByEmailAPI =
    GitLabRoot :> "users"
    :> QueryParam "search" Text
    :> Get '[JSON] [User]

findUserByEmail :: AccessToken -> Text -> ClientM (Maybe User)
findUserByEmail tok email = do
    res <- findUsersByEmail tok email
    return $ case res of
               [] -> Nothing
               [user] -> Just user
               _ -> error $ "Multiple users with email "<>show email

findUsersByEmail :: AccessToken -> Text -> ClientM [User]
findUsersByEmail tok email = do
    client (Proxy :: Proxy FindUserByEmailAPI) (Just tok) (Just email)


----------------------------------------------------------------------
-- getUsers
----------------------------------------------------------------------

type GetUsersAPI =
    GitLabRoot :> "users"
    :> QueryParam "per_page" Int
    :> QueryParam "page" Int
    :> QueryParam "search" Text
    :> QueryParam "username" Text
    :> Get '[JSON] ((Headers '[Header "X-Total-Pages" Int] [User]))

getUsers :: Maybe Text -> Maybe Text -> AccessToken -> ClientM [User]
getUsers usrname search tok = do
  h <- mkReq 1
  let total = fromMaybe "1" $ lookup "X-Total-Pages" (getHeaders h )
      total_n = read (B.unpack total) :: Int
  return $ getResponse h
  where
    mkReq k = client (Proxy :: Proxy GetUsersAPI) (Just tok) (Just 100) (Just k) search usrname

getUserByUsername :: Text -> AccessToken -> ClientM (Maybe User)
getUserByUsername usrname tok =
  listToMaybe <$> getUsers (Just usrname) Nothing tok

{-
  us <- loop 2 2
  return $ getResponse h -- ++ us
  where
    loop k n
     | k > n = return []
     | otherwise = do
          us <- getResponse <$> mkReq k
          uss <- loop (k + 1) n
          return $ us ++ uss
-}





----------------------------------------------------------------------
-- addProjectMember
----------------------------------------------------------------------

type AddProjectMemberAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "members"
    :> ReqBody '[JSON] AddProjectMember
    :> Post '[JSON] Object

data AccessLevel = Guest | Reporter | Developer | Maintainer | Owner

instance ToJSON AccessLevel where
    toJSON l = toJSON $ case l of
                          Guest      -> 10 :: Int
                          Reporter   -> 20
                          Developer  -> 30
                          Maintainer -> 40
                          Owner      -> 50

data AddProjectMember = AddProjectMember UserId AccessLevel

instance ToJSON AddProjectMember where
    toJSON (AddProjectMember uid access) = object
        [ "user_id" .= uid
        , "access_level" .= access
        ]

addProjectMember :: AccessToken -> ProjectId
                 -> UserId -> AccessLevel -> ClientM ()
addProjectMember tok prj uid access = do
    client (Proxy :: Proxy AddProjectMemberAPI) (Just tok) prj
        $ AddProjectMember uid access
    return ()
