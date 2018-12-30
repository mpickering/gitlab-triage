{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Parsers where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Combinators ()
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Generics.Product
import Control.Lens (view)

import qualified Data.Set as S

import GitLab.Common
import GitLab.Users
import GitLab.Tickets


checkLabelInput :: T.Text -> Maybe Labels
checkLabelInput t =
  Labels . S.fromList <$> parseMaybe @() (sepBy plabel (string ",")) t
  where
--    label :: ParsecT () T.Text Identity T.Text
    plabel = T.pack <$> (space *> (some alphaNumChar) <* space)

checkGotoInput :: T.Text -> Maybe IssueIid
checkGotoInput t = IssueIid <$> parseMaybe @() decimal t

checkWeightInput :: T.Text -> Maybe (Maybe Int)
checkWeightInput "" = Just Nothing
checkWeightInput t = Just <$> parseMaybe @() decimal t

checkTitleInput :: T.Text -> Maybe T.Text
checkTitleInput "" = Nothing
checkTitleInput t  = Just t

checkUserInput :: [User]
               -> T.Text
               -> Maybe [User]
checkUserInput _ t | T.null (T.strip t) = Just []
checkUserInput mr t =
  Just $ maybe [] (:[]) (lookupUser (T.strip t) mr)


checkMilestoneInput :: [MilestoneResp]
                    -> T.Text
                    -> Maybe (Maybe MilestoneResp)
checkMilestoneInput _ t | T.null (T.strip t) = Just Nothing
checkMilestoneInput mr t = Just <$> lookupMilestone (T.strip t) mr

lookupUser :: T.Text -> [User] -> Maybe User
lookupUser _ [] = Nothing
lookupUser t (m:ms) = if view (field @"userUsername") m == t
                              then Just m
                              else lookupUser t ms

lookupMilestone :: T.Text -> [MilestoneResp] -> Maybe MilestoneResp
lookupMilestone _ [] = Nothing
lookupMilestone t (m:ms) = if view (field @"mrTitle") m == t
                              then Just m
                              else lookupMilestone t ms

{-
-
                                    Milestone
                                        (view (field @"mrTitle") m)
                                        (view (field @"mrId") m)
                                        -}
