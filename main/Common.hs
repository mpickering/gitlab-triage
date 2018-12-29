{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Common where

import Control.Monad.Reader
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Brick

import Servant.Client

import Data.Generics.Product
import Control.Lens ( view, set )

import Model
import GitLab.Common
import GitLab.Users
import GitLab.Tickets


{- Handler definitions -}

type Handler' s k =
  s
  -> T.BrickEvent Name ()
  -> T.EventM Name (T.Next k)

type HandlerR' s k =
  s
  -> T.BrickEvent Name ()
  -> ReaderT AppConfig (T.EventM Name) (T.Next k)

type Handler s = Handler' s s

type HandlerR s = HandlerR' s s

halt :: k -> ReaderT AppConfig (T.EventM Name) (T.Next k)
halt = lift . M.halt

continue :: k -> ReaderT AppConfig (T.EventM Name) (T.Next k)
continue = lift . M.continue

runQuery :: AppConfig -> (AccessToken -> ProjectId -> ClientM a) -> IO a
runQuery l q = do
  let tok   = view (field @"userConfig" . field @"token") l
      reqEnv  = view (field @"reqEnv") l
      project = view (field @"userConfig" . field @"project") l
  res <- runClientM (q tok project) reqEnv
  return (either (error . show) id res)

issueView :: OperationalState -> IssuePage -> OperationalState
issueView l n = set (field @"mode") (IssueView n) l

{- Running external queries -}


loadByIid :: IssueIid -> AppConfig -> IO IssuePage
loadByIid iid ac = do
  r <- runQuery ac (getOneIssue iid)
  loadByIssueResp r ac

loadByIssueResp :: IssueResp -> AppConfig -> IO IssuePage
loadByIssueResp t l = do
  let iid = view (field @"irIid") t
  es_n <- runQuery l (\t' p -> listIssueNotes t' Nothing p iid)
  links <- runQuery l (\tok prj -> listIssueLinks tok prj iid)
  let emptyUpdates = Updates Nothing noEdits
  return $ (IssuePage (L.list Notes (Vec.fromList es_n) 6) t emptyUpdates links)

lookupUser :: T.Text -> [User] -> Maybe User
lookupUser _ [] = Nothing
lookupUser t (m:ms) = if view (field @"userUsername") m == t
                              then Just m
                              else lookupUser t ms


{- drawing helpers -}

int :: Int -> T.Widget n
int = str . show

showR :: Show a => a -> T.Widget n
showR = str . show

drawDate :: T.Text -> T.Widget a
drawDate = txt
