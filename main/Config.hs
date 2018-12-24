{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Config where

import System.Directory
import GHC.Generics
import Control.Monad
import qualified Data.Text as T

import GitLab.Common
import Data.Yaml
import Data.Aeson (ToJSON, FromJSON)


data UserConfig = UserConfig
      { token :: AccessToken
      , url :: T.Text
      , project :: ProjectId
      } deriving (Generic, ToJSON, FromJSON)

getConfigFile :: IO FilePath
getConfigFile = getXdgDirectory XdgConfig ".gitlab-triager"


readConfig :: IO (Either ParseException UserConfig)
readConfig = do
  configFile <- getConfigFile
--  putStrLn $ "Reading config from: " ++ configFile
  decodeFileEither configFile

writeConfig :: UserConfig -> IO ()
writeConfig uc = do
  configFile <- getConfigFile
--  putStrLn $ "Writing config to: " ++ configFile
  encodeFile configFile uc

deleteConfig :: IO ()
deleteConfig = do
  configFile <- getConfigFile
  exists <- doesFileExist configFile
--  putStrLn $ "Removing config: " ++ configFile
  when exists (removeFile configFile)
