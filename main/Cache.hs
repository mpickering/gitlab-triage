{- Language NoImplicitPrelude #-}
module Cache where

import Data.Time.Clock.POSIX

import Data.HashPSQ

import Prelude hiding (lookup)

import Control.Concurrent.MVar

import Data.Hashable



data Cache k v = Cache Int (HashPSQ k Int v)

type MCache k v = MVar (Cache k v)

newCache :: Int -> IO (MCache k v)
newCache timeout = newMVar (Cache timeout empty)

lookupOrInsertCache :: (Ord k, Hashable k) => k
                                           -> IO v
                                           -> MCache k v
                                           -> IO v
lookupOrInsertCache key act cache = do
  modifyMVar cache (\(Cache t q) -> do
    time <- round `fmap` getPOSIXTime
    case lookup key q of
      Nothing -> do
        v <- act
        time <- round `fmap` getPOSIXTime
        return ((Cache t (insert key time v q), v))
      Just (p, v) -> do
        return ((Cache t q), v))

insertCache :: (Ord k, Hashable k) => k -> v -> MCache k v -> IO ()
insertCache k v psq =
  modifyMVar_ psq (\(Cache t q) -> do
    time <- round `fmap` getPOSIXTime
    return (Cache t (insert k time v q)))

refreshCache :: (Ord k, Hashable k) => MCache k v -> IO ()
refreshCache psq =
  modifyMVar_ psq (\(Cache timeout q) -> do
    time <- round `fmap` getPOSIXTime
    -- First evict anything older than the timeout
    let lim = time - timeout
        (_, q') = atMostView lim q
    return (Cache timeout q'))





