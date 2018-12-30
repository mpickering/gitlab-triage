{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Common where

import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Data.Default
import Control.Lens ( to )

import Control.Monad.Reader
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Brick

import Servant.Client

import Data.Generics.Product
import Control.Lens ( view, set, ALens, cloneLens )

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


-- | liftHandler lifts a handler which only operates on its own state into
-- a larger state. It won't work if the handler needs to modify something
-- from a larger scope.
liftHandler
  :: ALens s s a a -- The mode to modify
  -> c        -- Inner state
  -> (c -> a) -- How to inject the new state
  -> Handler c -- Handler for inner state
  -> Handler s
liftHandler l c i h st ev = do
  let update s = set (cloneLens l) (i s) st
  fmap update <$> h c ev

halt :: k -> ReaderT AppConfig (T.EventM Name) (T.Next k)
halt = lift . M.halt

continue :: k -> ReaderT AppConfig (T.EventM Name) (T.Next k)
continue = lift . M.continue

runQuery :: AppConfig -> (AccessToken -> ProjectId -> ClientM a) -> IO a
runQuery l q = do
  let tok   = view (field @"userConfig" . field @"token") l
      reqEnv'  = view (field @"reqEnv") l
      project' = view (field @"userConfig" . field @"project") l
  res <- runClientM (q tok project') reqEnv'
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
  links' <- runQuery l (\tok prj -> listIssueLinks tok prj iid)
  let emptyUpdates = Updates Nothing noEdits
  return $ (IssuePage (L.list Notes (Vec.fromList es_n) 6) t emptyUpdates links')

tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = False }

gitlabBaseUrl :: String -> BaseUrl
gitlabBaseUrl base = BaseUrl Https base 443 "api/v4"

initialise :: UserConfig -> IO AppState
initialise c = do
  mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
  let env = mkClientEnv mgr
              (gitlabBaseUrl (view (field @"url" . to T.unpack) c))
      conf = (AppConfig c env)

  cur_labels <- runQuery conf getLabels
  cur_milestones <- runQuery conf getMilestones
  cur_users <- runQuery conf (\tok _ -> getUsers tok)
  les <- TicketListView <$> loadTicketList defaultSearchParams conf
  let mm = Operational (OperationalState les FooterInfo NoDialog
                                         cur_labels cur_milestones cur_users conf)
  return $ AppState mm

loadTicketList :: GetIssuesParams -> AppConfig -> IO TicketList
loadTicketList sp conf = do
  es <- runQuery conf (getIssues sp)
  return $ TicketList (L.list IssueList (Vec.fromList es) 1) sp

---



{- drawing helpers -}

int :: Int -> T.Widget n
int = str . show

showR :: Show a => a -> T.Widget n
showR = str . show

drawDate :: T.Text -> T.Widget a
drawDate = txt
