{-# LANGUAGE DeriveGeneric #-}
module Namespace where

import GitLab.Common
import qualified Data.Text as T
import GHC.Generics


data Name = IssueList | Notes | Footer | FormArea T.Text | Note (Bool, Int)
          | Metainfo IssueIid | Dialog DialogName
              deriving (Show, Ord, Generic, Eq)

data DialogName = MilestoneName Bool | OwnerName Bool deriving (Show, Ord, Generic, Eq)
