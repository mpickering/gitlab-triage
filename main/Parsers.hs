{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Combinators ()
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Set as S

import GitLab.Common


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
