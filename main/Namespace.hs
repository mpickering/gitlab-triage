{-# LANGUAGE DeriveGeneric #-}
module Namespace where

import GitLab.Common
import qualified Data.Text as T
import GHC.Generics


data Name = IssueList | Notes | Footer | FormArea T.Text | Note (Bool, Int)
          | Metainfo IssueIid
              deriving (Show, Ord, Generic, Eq)
