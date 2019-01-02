{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveAnyClass #-}
module Autocomplete(mkAutocomplete
                   , mkAutocompleteIO
                   , Autocomplete(..)
                   , drawAutocomplete
                   , handleAutocompleteEvent) where

import Brick hiding (continue, halt)

import Control.Lens (set)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Cursor.Text
import Data.Text(Text)

import Data.Generics.Product
import GHC.Generics
import Control.Lens

import Control.Monad.IO.Class (liftIO)

data Autocomplete s n a
      = Autocomplete { autocompleteState :: s
                     , autocompleteMatches :: Text -> s -> IO [a]
                     , autocompleteToText :: a -> Text
                     , autocompleteCursor :: TextCursor
                     , autocompleteCursorFocus :: n
                     , autocompleteList :: L.List n a
                     } deriving Generic

mkAutocomplete :: [a]
               -> (Text -> [a] -> [a])
               -> (a -> Text)
               -> Maybe Text
               -> n
               -> n
               -> Autocomplete [a] n a
mkAutocomplete s m  = mkAutocompleteIO s (\t s' -> return (m t s'))

mkAutocompleteIO :: [a]
               -> (Text -> [a] -> IO [a])
               -> (a -> Text)
               -> Maybe Text
               -> n
               -> n
               -> Autocomplete [a] n a
mkAutocompleteIO s m tt ini ns1 ns2 = Autocomplete s
                                m
                                tt
                                (fromMaybe emptyTextCursor $ (ini >>= makeTextCursor))
                                ns1
                                (L.list ns2 (Vec.fromList s) 1)

drawAutocomplete ::
  forall s n a .
  (Show n, Ord n)
  => Autocomplete s n a
  -> Widget n
drawAutocomplete Autocomplete{..} =
  vBox [ drawTextCursor autocompleteCursorFocus autocompleteCursor
       , matchesBox ]
  where
    matchesBox = L.renderList renderItem True autocompleteList

    renderItem :: Bool -> a -> Widget n
    renderItem _ = txt . autocompleteToText

newCursor :: Text -> TextCursor
newCursor = fromMaybe emptyTextCursor . makeTextCursor

handleAutocompleteEvent
  :: (Ord n, Show n)
  => Autocomplete s n a
  -> BrickEvent n e
  -> EventM n (Autocomplete s n a)
handleAutocompleteEvent ac (VtyEvent e) = do
  ac1 <- T.handleEventLensed ac (field @"autocompleteList") L.handleListEvent e
  ac2 <- T.handleEventLensed ac1 (field @"autocompleteCursor") handleTextCursorEvent e
  case e of
        V.EvKey key _mods ->
          case key of
            V.KChar '\t' ->
              case L.listSelectedElement (autocompleteList ac2) of
                Nothing -> return ac2
                Just (_, a) ->
                  return $ set (field @"autocompleteCursor")
                              (newCursor (autocompleteToText ac a)) ac2
            V.KChar {} -> liftIO $ updateAutocompleteItems ac2
            V.KBS -> liftIO $ updateAutocompleteItems ac2
            V.KDel -> liftIO $ updateAutocompleteItems ac2
            _ -> return ac2
        _ -> return ac2
handleAutocompleteEvent ac _ = return ac

updateAutocompleteItems :: Autocomplete s n a -> IO (Autocomplete s n a)
updateAutocompleteItems ac@Autocomplete{..} = do
  let new_txt = rebuildTextCursor autocompleteCursor
  matches <- Vec.fromList <$> autocompleteMatches new_txt autocompleteState
  return $ over (field @"autocompleteList" ) (L.listReplace matches (Just 0)) ac

{- Text Cursor by Tom Kerokove -}

drawTextCursor :: n -> TextCursor -> Widget n
drawTextCursor n tc =
  showCursor n (Location (textCursorIndex tc, 0))
    $ case (rebuildTextCursor tc) of
        "" -> txt " " -- So rendering always has a fixed height
        t  -> txt t

handleTextCursorEvent :: V.Event -> TextCursor -> EventM n TextCursor
handleTextCursorEvent e tc  = do
            case e of
                V.EvKey key _mods ->
                    let mDo func = return . fromMaybe tc $ func tc
                    in case key of
                           V.KChar c -> mDo $ textCursorInsert c
                           V.KLeft -> mDo textCursorSelectPrev
                           V.KRight -> mDo textCursorSelectNext
                           V.KBS -> mDo textCursorRemove
                           V.KDel -> mDo textCursorDelete
                           _ -> return tc
                _ -> return tc




