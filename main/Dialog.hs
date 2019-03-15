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

import GitLab.Tickets
import GitLab.Users
import GitLab.Common

import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.Text as T

import Brick hiding (continue, halt)

import Control.Lens
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
--import qualified Brick.Widgets.List as L
import qualified IOList as L
import qualified Brick.Widgets.Center as C
import Brick.Types ( Widget )
import Data.Generics.Product

import Control.Monad.IO.Class (liftIO)

import Text.Megaparsec
import Control.Applicative.Combinators ()
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.Set as S

import Model
import Autocomplete
import Common
import Parsers

import TextCursor

import Debug.Trace

class SelectDialog s where
    selectDialog ::
      (T.Text -> IO (Maybe a)) ->
      (ALens s s (Maybe (NonEmpty a)) (Maybe (NonEmpty a))) ->
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
            -> ALens s s (Maybe (NonEmpty a)) (Maybe (NonEmpty a))
            -> [a]
            -> s
            -> OperationalState
            -> OperationalState
startDialogX multi draw check restrict place ini tl l =
  let ini_state = view (cloneLens place) tl
      state_t = fmap draw <$> ini_state

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
startDialogIO draw check restrict place = startDialogX False draw check restrict
                                            (cloneLens place . unsafeIso)
  where
    unsafeIso :: Iso (Maybe a) (Maybe a) (Maybe (NonEmpty a)) (Maybe (NonEmpty a))
    unsafeIso = iso (fmap (\a -> (a :| []))) (fmap (\(a :| _) -> a))

startMultiDialogIO :: SelectDialog s
            => (a -> T.Text)
            -> (T.Text -> IO (Maybe a))
            -> (T.Text -> [a] -> IO [a])
            -> ALens s s (Maybe (NonEmpty a)) (Maybe (NonEmpty a))
            -> [a]
            -> s
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
