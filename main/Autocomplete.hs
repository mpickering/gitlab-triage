{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Brick hiding (continue, halt)
import Brick.Forms

import Control.Lens (view, ALens,  to, set, cloneLens, Traversal')
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Util (fg, on)

import Cursor.Text
import Data.Text(Text, isPrefixOf)
import qualified Data.Vector as Vec

import Data.Generics.Product
import GHC.Generics
import Control.Lens

data Autocomplete s n a
      = Autocomplete { autocompleteState :: s
                     , autocompleteParse :: Text -> Maybe a
                     , autocompleteMatches :: Text -> s -> [a]
                     , autocompleteToText :: a -> Text
                     , autocompleteCursor :: TextCursor
                     , autocompleteCursorFocus :: n
                     , autocompleteList :: L.List n a
                     } deriving Generic

l = ["abcde", "abc", "def"]
testAutocomplete = Autocomplete l
                                Just
                                (\t s -> filter (t `isPrefixOf`) s)
                                id
                                emptyTextCursor
                                False
                                (L.list True (Vec.fromList l) 1)

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
            V.KChar {} -> return $ updateAutocompleteItems ac2
            V.KBS -> return $ updateAutocompleteItems ac2
            V.KDel -> return $ updateAutocompleteItems ac2
            _ -> return ac2
        _ -> return ac2

updateAutocompleteItems :: Autocomplete s n a -> Autocomplete s n a
updateAutocompleteItems ac@Autocomplete{..} =
  let txt = rebuildTextCursor autocompleteCursor
      matches = Vec.fromList (autocompleteMatches txt autocompleteState)
  in set (field @"autocompleteList" . sets modifyListElements) matches ac

modifyListElements :: Foldable t => (t e -> t e) -> L.GenericList n t e -> L.GenericList n t e
modifyListElements els l =
  over L.listElementsL els
    . set L.listSelectedL (Just 0) $ l

theMap :: s -> A.AttrMap
theMap _ = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    , ("default", V.defAttr )
    ]

handle
  :: (Ord n, Show n)
  => Autocomplete s n a
  -> BrickEvent n e
  -> EventM n (Next (Autocomplete s n a))
handle s e1@(VtyEvent e) = do
  case e of
    V.EvKey V.KEsc [] -> M.halt s
    _ -> handleAutocompleteEvent s e1 >>= M.continue
handle s _ = M.continue s

app :: App (Autocomplete [Text] Bool Text) () Bool
app = App (\ac -> [drawAutocomplete ac])
                  showFirstCursor
                  handle
                  return
                  theMap



main = defaultMain app  testAutocomplete



modifyAutocompleteState :: (s -> s) -> Autocomplete s n a -> Autocomplete s n a
modifyAutocompleteState f a = a { autocompleteState = f (autocompleteState a) }




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




