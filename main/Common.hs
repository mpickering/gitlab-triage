{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common where

import qualified Servant.Client.Internal.HttpClient as I
import qualified Network.HTTP.Client                as HTTP
import Servant.Client.Core.Internal.Request
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Data.Default
import Control.Lens ( to, firstOf, lens)

import Control.Monad.Reader
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Brick
import qualified IOList
import qualified Data.Set as S

import Servant.Client.Free
import qualified Servant.Client as H

import Data.Generics.Product
import Control.Lens ( view, set, ALens, cloneLens )

import Model
import GitLab.Common
import GitLab.Users
import GitLab.Tickets
import Cache

import System.IO
import Control.Monad.Trans.Except
import Control.Monad.Free
import Control.Monad.Error.Class

import Data.Hashable
import Data.Maybe


toClientM :: ClientM a -> H.ClientM a
toClientM c = iterM alg c
  where
    alg :: ClientF (H.ClientM a) -> H.ClientM a
    alg (Throw err) = throwError err
    alg (StreamingRequest {}) = error "streaming"
    alg (RunRequest req k) =
      (I.performRequest req ) >>= k

runClientM :: ClientM a -> H.ClientEnv -> IO (Either ServantError a)
runClientM = H.runClientM . toClientM

runClientWithCache ::
                    MCache HTTP.Request (Either ServantError Response)
                    -> ClientM a
                    -> H.ClientEnv
                    -> IO (Either ServantError a)
runClientWithCache mcache c e = iterM alg (fmap Right c)
  where
    alg :: ClientF (IO (Either ServantError a)) -> IO (Either ServantError a)
    alg (RunRequest req k) = do
      -- We convert it because this version has a Show instance..
      let req' = I.requestToClientRequest (H.baseUrl e) req
          act = I.runClientM (I.performRequest req) e
      -- Don't cache POST or PUT requests for instance
      res <-
        if requestMethod req == "GET"
          then lookupOrInsertCache req' act mcache
          else act
      case res of
        Left err -> return (Left err)
        Right v  -> k v
    alg (Throw err) = (return (Left err))
    alg (StreamingRequest {}) = error "streaming"

instance Ord (HTTP.Request) where
  compare t1 t2 = compare (show t1) (show t2)

instance Eq HTTP.Request where
  (==) t1 t2 = show t1 == show t2

instance Hashable HTTP.Request where
  hashWithSalt i r = hashWithSalt i (show r)







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

runQuery :: AppConfig -> (AccessToken -> ProjectId -> ClientM a)
         -> ExceptT ServantError IO a
runQuery l q = do
  let tok   = view (field @"userConfig" . field @"token") l
      reqEnv'  = view (field @"reqEnv") l
      project' = view (field @"userConfig" . field @"project") l
      mcache = view (field @"cache") l
  ExceptT $ runClientWithCache mcache (q tok project') reqEnv'

runQueryPaginate :: forall a . AppConfig
                 -> (Maybe Int -> AccessToken -> ProjectId
                                              -> ClientM (GetIssueHeaders, [a]))
                 -> ExceptT ServantError IO (IOList.IOList a)
runQueryPaginate ac q = do
  (hs, inis) <- ExceptT $ runClientM (q Nothing tok project') reqEnv'
  liftIO (print hs)
  let io_list =
        case next_page hs of
          Nothing -> IOList.nil
          Just p  -> loop (total hs - per_page hs) p 10
  return (inis `IOList.concatILPure` io_list)
  where
    tok   = view (field @"userConfig" . field @"token") ac
    reqEnv'  = view (field @"reqEnv") ac
    project' = view (field @"userConfig" . field @"project") ac

    loop :: Int -> Int -> Int -> IOList.IOList a
    loop rem_n cur tot | cur > tot = IOList.nil
                       | otherwise = IOList.IOList rem_n $ IOList.ILLoad $ do
                    res <- runClientM (q (Just cur) tok project') reqEnv'
                    case res of
                      Left _ -> return (IOList.nil)
                      Right (gih, as) ->
                        return
                          (as `IOList.concatILPure`
                            case next_page gih of
                              Nothing -> IOList.nil
                              Just p  -> loop (rem_n - per_page gih) p tot)



displayError :: ExceptT ServantError IO a
             -> (a -> T.EventM n (T.Next OperationalState))
             -> OperationalState ->  T.EventM n (T.Next OperationalState)
displayError act k o = do
  res <- liftIO (runExceptT act)
  case res of
    Left err -> M.continue (set typed (FooterMessage (T.pack (show err))) o)
    Right v -> k v

issueView :: OperationalState -> IssuePage -> OperationalState
issueView l n = set (field @"mode") (IssueView n) l

{- Running external queries -}


loadByIid :: IssueIid -> AppConfig -> ExceptT ServantError IO (IssuePageContents)
loadByIid iid ac = do
  r <- runQuery ac (getOneIssue iid)
  loadByIssueResp r ac

loadByIssueResp :: IssueResp -> AppConfig -> ExceptT ServantError IO (IssuePageContents)
loadByIssueResp t l = do
  let iid = view (field @"irIid") t
  es_n <- runQuery l (\t' p -> listIssueNotes t' Nothing p iid)
  links' <- runQuery l (\tok prj -> listIssueLinks tok prj iid)
  let emptyUpdates = Updates Nothing noEdits
  return $ (IssuePageContents (L.list Notes (Vec.fromList es_n) 6) t emptyUpdates links')

tlsSettings :: TLSSettings
tlsSettings = def { settingDisableCertificateValidation = False }

gitlabBaseUrl :: String -> BaseUrl
gitlabBaseUrl base = BaseUrl Https base 443 "api/v4"

initialise :: UserConfig -> IO AppState
initialise c = do
  mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
  mcache <- newCache 100000000
  let env = H.mkClientEnv mgr
              (gitlabBaseUrl (view (field @"url" . to T.unpack) c))
      conf = (AppConfig c env mcache)

  cur_labels <- defaultEither [] $ runQuery conf getLabels
  cur_milestones <- defaultEither [] $ runQuery conf getMilestones
  cur_users <- defaultEither []
                $ runQuery conf (\tok _ -> getUsers Nothing Nothing tok)
  cur_tickets <- loadTicketList defaultSearchParams conf
  let les = TicketListView cur_tickets
  let mm = Operational (OperationalState les FooterInfo NoDialog
                                         cur_labels cur_milestones cur_users conf)
  return $ AppState mm

-- Used when we can't recover
unsafeEither :: IO (Either ServantError a) -> IO a
unsafeEither act = either (error . show) id <$> act

defaultEither :: a -> ExceptT ServantError IO a -> IO a
defaultEither d act = do
  v <- runExceptT act
  case v of
    Left err -> hPutStr stderr (show err) >> return d
    Right res -> return res

loadTicketList :: GetIssuesParams -> AppConfig -> IO TicketList
loadTicketList sp conf = do
  es <- defaultEither IOList.nil $ runQueryPaginate conf (getIssues sp)
  --let es = IOList.nil
  return $ (TicketList (IOList.list IssueList es 1 50) sp)

--
getTicketListContext :: OperationalState -> TicketList
getTicketListContext o =
  case view (typed @Mode) o of
    TicketListView tl -> tl
    IssueView (IssuePage tl _) -> tl



{-
  case firstOf (types @TicketList) o of
    Just tl -> tl
    Nothing -> error "ticketListContext: Should be non-empty traversal"
    -}


checkAuthor :: AppConfig -> T.Text -> IO (Maybe User)
checkAuthor ac t =
      defaultEither Nothing $
      runQuery ac (\tok _ -> getUserByUsername t tok)

-- API calls on empty string take ages
restrictAuthor :: AppConfig -> T.Text -> [User] -> IO [User]
restrictAuthor _ "" us = return us
restrictAuthor ac t _ =
  defaultEither [] $ runQuery ac
                              (\tok _ -> getUsers Nothing (Just t) tok)

restrictLabel :: AppConfig -> T.Text -> [LabelParam] -> IO [LabelParam]
restrictLabel _ "" us = return us
restrictLabel ac t us =  return us
--  defaultEither [] $ runQuery ac (\tok _ -> getL

---

{- drawing helpers -}

int :: Int -> T.Widget n
int = str . show

showR :: Show a => a -> T.Widget n
showR = str . show

drawDate :: T.Text -> T.Widget a
drawDate = txt

formatFooterMode :: FooterInputMode -> T.Text
formatFooterMode m = case m of
                       FGoto {}   -> "g: "
                       FGen h _ _ -> h <> ": "

