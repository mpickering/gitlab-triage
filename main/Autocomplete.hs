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
                   , mkMultiAutocompleteIO
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
import Data.Generics.Sum
import GHC.Generics
import Control.Lens

import Control.Monad.IO.Class (liftIO)

-- Could give this a type parameter to differentiate between the singleton
-- and n-ary cases.
data Autocomplete s n a
      = Autocomplete { autocompleteState :: s
                     , autocompleteMatches :: Text -> s -> IO [a]
                     , autocompleteToText :: a -> Text
                     , autocompleteCursor :: TextCursor
                     , autocompleteCursorFocus :: n
                     , autocompleteList :: L.List n a
                     , autocompleteItems :: Maybe [Text] -- Nothing if we just allow 1 item
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
mkAutocompleteIO = mkMultiAutocompleteIO False

mkMultiAutocompleteIO ::
               Bool
               -> [a]
               -> (Text -> [a] -> IO [a])
               -> (a -> Text)
               -> Maybe Text
               -> n
               -> n
               -> Autocomplete [a] n a
mkMultiAutocompleteIO multi s m tt ini ns1 ns2 = Autocomplete s
                                m
                                tt
                                (fromMaybe emptyTextCursor $ (ini >>= makeTextCursor))
                                ns1
                                (L.list ns2 (Vec.fromList s) 1)
                                (if multi then Just [] else Nothing)



drawAutocomplete ::
  forall s n a .
  (Show n, Ord n)
  => Autocomplete s n a
  -> Widget n
drawAutocomplete Autocomplete{..} =
  vBox [ textRow
       , matchesBox ]
  where
    textRow = acItems <+>
                drawTextCursor autocompleteCursorFocus autocompleteCursor

    acItems = case autocompleteItems of
                Nothing -> emptyWidget
                Just acs -> hBox (map one_ac_item acs)

    one_ac_item = padRight (Pad 1) . withAttr "selected" . txt

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
  case e of
    V.EvKey key _mods ->
      case key of
        V.KChar ',' -> liftIO $ updateAutocompleteItems (saveSelection ac)
        V.KChar '\t' ->
          case L.listSelectedElement (autocompleteList ac) of
            Nothing -> return ac
            Just (_, a) ->
              return $ set (field @"autocompleteCursor")
                           (newCursor (autocompleteToText ac a)) ac
        _ -> handleListAndCursorEvent ac (VtyEvent e)
handleAutocompleteEvent ac _ = return ac


handleListAndCursorEvent
  :: (Ord n, Show n)
  => Autocomplete s n a
  -> BrickEvent n e
  -> EventM n (Autocomplete s n a)
handleListAndCursorEvent ac (VtyEvent e) = do
  ac1 <- T.handleEventLensed ac (field @"autocompleteList") L.handleListEvent e
  ac2 <- T.handleEventLensed ac1 (field @"autocompleteCursor") handleTextCursorEvent e
  case e of
        V.EvKey key _mods ->
          case key of
            V.KChar {} -> liftIO $ updateAutocompleteItems ac2
            V.KBS -> liftIO $ updateAutocompleteItems ac2
            V.KDel -> liftIO $ updateAutocompleteItems ac2
            _ -> return ac2
        _ -> return ac2
handleListAndCursorEvent ac _ = return ac

updateAutocompleteItems :: Autocomplete s n a -> IO (Autocomplete s n a)
updateAutocompleteItems ac@Autocomplete{..} = do
  let new_txt = rebuildTextCursor autocompleteCursor
  matches <- Vec.fromList <$> autocompleteMatches new_txt autocompleteState
  return $ over (field @"autocompleteList" ) (L.listReplace matches (Just 0)) ac

saveSelection :: Autocomplete s n a -> Autocomplete s n a
saveSelection ac@Autocomplete{..} =
  let new_txt = rebuildTextCursor autocompleteCursor
  in case autocompleteItems of
       Nothing -> ac
       Just {} ->
        over (field @"autocompleteItems" . _Ctor @"Just") (++ [new_txt])
              $ set (field @"autocompleteCursor") emptyTextCursor
              $ ac

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




