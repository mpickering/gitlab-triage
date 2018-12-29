module ExternalEditor where

import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys

import qualified Data.ByteString as BS

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class (liftIO)

import Brick


-- Copied from matterhorn
invokeExternalEditor
  :: Maybe T.Text
  -> (Maybe T.Text -> IO s)
  -> EventM n (Next s)
invokeExternalEditor initialText k = do
    -- If EDITOR is in the environment, write the current message to a
    -- temp file, invoke EDITOR on it, read the result, remove the temp
    -- file, and update the program state.
    --
    -- If EDITOR is not present, fall back to 'vi'.
    mEnv <- liftIO $ Sys.lookupEnv "EDITOR"
    let editorProgram = maybe "vi" id mEnv

    suspendAndResume $ do
      Sys.withSystemTempFile "gitlab-triage.tmp" $ \tmpFileName tmpFileHandle -> do
--         Write the current message to the temp file
        case initialText of
          Nothing -> return ()
          Just t -> do
                       Sys.hPutStr tmpFileHandle $ T.unpack $ t
        Sys.hClose tmpFileHandle

        -- Run the editor
        status <- Sys.system (editorProgram <> " " <> tmpFileName)

        -- On editor exit, if exited with zero status, read temp file.
        -- If non-zero status, skip temp file read.
        case status of
            Sys.ExitSuccess -> do
                tmpBytes <- BS.readFile tmpFileName
                case T.decodeUtf8' tmpBytes of
                    Left _ -> do
                        error "Failed to decode file contents as UTF-8"
                    Right t -> k (Just t)
            Sys.ExitFailure _ -> k Nothing
