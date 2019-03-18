{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Dialog where

import Data.Maybe

import qualified Data.Text as T

import Control.Lens

import Data.Generics.Product

import Model
import Autocomplete

class SelectDialog s where
    selectDialog ::
      (T.Text -> IO (Maybe a)) ->
      (ASetter s s [a] [a]) ->
      (AppAutocomplete a) -> DialogMode

instance SelectDialog TicketList where
  selectDialog = SearchParamsDialog

instance SelectDialog IssuePageContents where
  selectDialog = IssuePageDialog


startDialogX :: forall s a . SelectDialog s
            => Bool
            -> (a -> T.Text)
            -> (T.Text -> IO (Maybe a))
            -> (T.Text -> [a] -> IO [a])
            -> ASetter s s [a] [a]
            -> [a]
            -> [a]
            -> OperationalState
            -> OperationalState
startDialogX multi draw check restrict place ini_ac ini l =
  let ini_state = ini_ac
      state_t = fmap draw ini_state

      ac =
        mkMultiAutocompleteIO multi
          ini
          restrict
          draw
          state_t
          (Dialog (MilestoneName False))
          (Dialog (MilestoneName True))


  in set typed (selectDialog @s check place ac) l

startDialogIO :: SelectDialog s
            => (a -> T.Text)
            -> (T.Text -> IO (Maybe a))
            -> (T.Text -> [a] -> IO [a])
            -> ALens s s (Maybe a) (Maybe a)
            -> [a]
            -> s
            -> OperationalState
            -> OperationalState
startDialogIO draw check restrict place ini s l =
  startDialogIOWithDef draw check restrict (cloneLens place) (view (cloneLens place) s) ini l

startDialogIOWithDef :: SelectDialog s
            => (a -> T.Text)
            -> (T.Text -> IO (Maybe a))
            -> (T.Text -> [a] -> IO [a])
            -> ASetter s s (Maybe a) (Maybe a)
            -> Maybe a
            -> [a]
            -> OperationalState
            -> OperationalState
startDialogIOWithDef draw check restrict place ini_inp ini =
                               startDialogX False draw check restrict
                                            (cloneSetter place . unsafeIso)
                                            (maybeToList ini_inp)
                                            ini
  where
    unsafeIso :: Iso (Maybe a) (Maybe a) [a] [a]
    unsafeIso = iso maybeToList listToMaybe

startMultiDialogIO :: SelectDialog s
            => (a -> T.Text)
            -> (T.Text -> IO (Maybe a))
            -> (T.Text -> [a] -> IO [a])
            -> ASetter s s [a] [a]
            -> [a]
            -> [a]
            -> OperationalState
            -> OperationalState
startMultiDialogIO = startDialogX True

pureRestrict :: Monad m => (a -> T.Text) -> T.Text -> [a] -> m [a]
pureRestrict draw t s =
  return $ filter (\v -> T.toLower t `T.isInfixOf` (T.toLower (draw v))) s

startDialog :: SelectDialog s
            => (a -> T.Text)
            -> (T.Text -> Maybe a)
            -> ALens s s (Maybe a) (Maybe a)
            -> [a]
            -> s
            -> OperationalState
            -> OperationalState
startDialog draw check =
  startDialogIO draw (return . check) (pureRestrict draw)

startDialogWithDef :: SelectDialog s
            => (a -> T.Text)
            -> (T.Text -> Maybe a)
            -> ASetter s s (Maybe a) (Maybe a)
            -> Maybe a
            -> [a]
            -> OperationalState
            -> OperationalState
startDialogWithDef draw check =
  startDialogIOWithDef draw (return . check) (pureRestrict draw)
