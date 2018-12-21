{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.UploadFile where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import Data.Semigroup
import Data.Aeson
import System.Process
import System.Directory
import Servant.Client
import GitLab.Common

----------------------------------------------------------------------
-- uploadFile
----------------------------------------------------------------------

type FileUrl = Text
data UploadFileResp = UploadFileResp FileUrl

instance FromJSON UploadFileResp where
    parseJSON = withObject "upload file response" $ \o ->
      UploadFileResp <$> o .: "url"

uploadFile :: AccessToken -> Maybe UserId -> ProjectId
           -> Text -> BS.ByteString -> ClientM FileUrl
uploadFile (AccessToken tok) sudo (ProjectId prjId) fname cs = do
    liftIO $ createDirectoryIfMissing True "tmp"
    let tmpfile = "tmp/" <> T.unpack fname
        sudoParam = maybe "" (\uid -> "?sudo=" <> show (getUserId uid)) sudo
    liftIO $ BS.writeFile tmpfile cs
    base <- baseUrl <$> ask
    out <- liftIO $ readProcess "curl"
        [ "--request", "POST"
        , "--header", "PRIVATE-TOKEN: " <> T.unpack tok
        , "--form", "file=@" <> tmpfile
        , showBaseUrl base <> "/projects/" <> show prjId <> "/uploads" <> sudoParam
        ]
        ""
    UploadFileResp url <- either (fail . show) return
        $ Data.Aeson.eitherDecode $ BSL.pack out
    liftIO $ removeFile tmpfile
    return url
