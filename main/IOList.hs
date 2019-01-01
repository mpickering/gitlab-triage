{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable#-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
-- | This module provides a scrollable list type like the one provided by
-- the main library but with support for lazily loading list items. Even
-- though the items are lazily loaded, we must still statically know the
-- size of the list so that the rendering works correctly.
--
-- This is meant to be used where the data you want to display in a list is
-- provided by a paginated API.
module IOList(IOListWidget(..), handleListEvent
              , renderList
              , renderListWithIndex, list,

              -- Constructing IOLists
              IOList(..), IOListL(..),
              nil, cons, concatILPure, fromList,

              -- List accessors
              listSelectedElement,
              listReplace,
              listRemove,
              listInsert,


              -- Helpers
              lengthIL, nullIL, concatIL, toListPure,

              --
              listElementsL,
              verify
              ) where

import Prelude hiding (reverse, splitAt)

import Control.Monad.Trans.State (evalState, get, put)

import Control.Monad

import Control.Lens hiding (List, imap, uncons, cons)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Graphics.Vty (Event(..), Key(..))
import GHC.Generics (Generic)


import Control.Monad.IO.Class (liftIO)

import Brick.Types
import Brick.Main (lookupViewport)
import Brick.Widgets.Core
import Brick.Util (clamp)
import Brick.AttrMap

data IOListWidget n e =
    List { listElements :: !(IOList e)
         -- ^ The list's sequence of elements.
         , listSelected :: !(Maybe Int)
         -- ^ The list's selected element index, if any.
         , listName :: n
         -- ^ The list's name.
         , listItemHeight :: Int
         -- ^ The height of an individual item in the list.
         , listBufferSize :: Int
         -- ^ How many items to preload into memory, should be the same as
         -- the vertical size of the list.
         } deriving (Functor, Generic)


instance Functor IOList where
  fmap f (IOList n l) = IOList n (fmap f l)

instance Functor IOListL where
  fmap _ ILNil = ILNil
  fmap f (ILCons x l) = ILCons (f x) (fmap f l)
  fmap f (ILLoad act) = ILLoad (fmap (fmap f) act)

-- A list with delayed embedded IO actions but a statically known length.
data IOList e = IOList Int (IOListL e)

nil :: IOList e
nil = IOList 0 ILNil

cons :: e -> IOList e -> IOList e
cons v (IOList n l) = IOList (n + 1) (ILCons v (IOList n l))

data IOListL e where
  ILNil :: IOListL e
  ILCons :: e -> IOList e -> IOListL e
  ILLoad :: IO (IOList e) -> IOListL e

indexIL :: IOList e -> Int -> IO (Maybe e)
indexIL l 0 = fmap fst <$> uncons l
indexIL l n = do
  ml <- uncons l
  case ml of
    Nothing -> return Nothing
    Just (_, l') -> indexIL l' (n - 1)

splitAt :: Int -> IOList e -> IO ([e], IOList e)
splitAt 0 l = return ([], l)
splitAt n l = do
  ml <- uncons l
  case ml of
    Nothing -> return ([], nil)
    Just (v, l') -> do
      (ts, ds) <- splitAt (n - 1) l'
      return (v : ts, ds)


uncons :: IOList e -> IO (Maybe (e, IOList e))
uncons (IOList _ k) = unconsL k

unconsL :: IOListL e -> IO (Maybe (e, IOList e))
unconsL ILNil = return Nothing
unconsL (ILCons e l) = return (Just (e, l))
unconsL (ILLoad act) = do
  l <- act
  uncons l

fromList :: [a] -> IOList a
fromList [] = IOList 0 ILNil
fromList (e:es) = cons e (fromList es)


instance Named (IOListWidget n e) n where
    getName = listName

force :: Int -> IOList e -> IO (IOList e)
force k l | k <= 0 = return l
force k (IOList _ l) = force' k l

force' :: Int -> IOListL e -> IO (IOList e)
force' _ ILNil = return nil
force' k (ILCons v l) = cons v <$> (force (k - 1) l)
force' k (ILLoad act) = do
  res <- act
  force k res

concatILPure :: [e] -> IOList e -> IOList e
concatILPure [] l = l
concatILPure (e:es) l = cons e (concatILPure es l)

concatIL :: IOList e -> IOList e -> IO (IOList e)
concatIL (IOList _ l1) l2 = concatIL' l1 l2

concatIL' :: IOListL e -> IOList e -> IO (IOList e)
concatIL' ILNil l = return l
concatIL' (ILCons e l1) l2 = cons e <$> (concatIL l1 l2)
concatIL' (ILLoad act) l2 = do
  l1 <- act
  concatIL l1 l2

lengthIL :: IOList e -> Int
lengthIL (IOList n _) = n

nullIL :: IOList e -> Bool
nullIL = (== 0) . lengthIL

suffixLenses ''IOListWidget

-- Verify that the reported length is the same as the actual length
verify :: IOList e -> IO (Maybe String)
verify (IOList n e) = verify' n e

verify' :: Int -> IOListL e -> IO (Maybe String)
verify' n ILNil = return $ if n == 0 then Nothing else (Just "Nil not length 0")
verify' n (ILCons _ (IOList n' l)) =
  if n == n' + 1 then verify' n' l else return (Just (show ("cons", n, n')))
verify' n (ILLoad act) = do
  (IOList n' l) <- act
  if n == n' then verify' n' l else return (Just (show ("load", n, n')))

-- | Handle events for list cursor movement.  Events handled are:
--
-- * Up (up arrow key)
-- * Down (down arrow key)
-- * Page Up (PgUp)
-- * Page Down (PgDown)
-- * Go to first element (Home)
-- * Go to last element (End)
handleListEvent :: (Ord n)
                => Event
                -> IOListWidget n e
                -> EventM n (IOListWidget n e)
handleListEvent e theList =
    case e of
        EvKey KUp [] -> liftIO $ listMoveUp theList
        EvKey KDown [] -> liftIO $ listMoveDown theList
        EvKey KHome [] -> liftIO $ listMoveTo 0 theList
        EvKey KEnd [] -> liftIO $ listMoveTo (lengthIL $ listElements theList) theList
        EvKey KPageDown [] -> listMovePageDown theList
        EvKey KPageUp [] -> listMovePageUp theList
        _ -> return theList

-- | The top-level attribute used for the entire list.
listAttr :: AttrName
listAttr = "list"

-- | The attribute used only for the currently-selected list item when
-- the list does not have focus. Extends 'listAttr'.
listSelectedAttr :: AttrName
listSelectedAttr = listAttr <> "selected"

-- | The attribute used only for the currently-selected list item when
-- the list has focus. Extends 'listSelectedAttr'.
listSelectedFocusedAttr :: AttrName
listSelectedFocusedAttr = listSelectedAttr <> "focused"

-- | Construct a list in terms of container 't' with element type 'e'.
list :: n
     -- ^ The list name (must be unique)
     -> IOList e
     -- ^ The initial list contents
     -> Int
     -- ^ The list item height in rows (all list item widgets must be
     -- this high).
     -> Int
     -> IOListWidget n e
list name es h b =
    let selIndex = if nullIL es then Nothing else Just 0
        safeHeight = max 1 h
        safeBuffer = max 0 b
    in List es selIndex name safeHeight safeBuffer

-- | Render a list using the specified item drawing function.
--
-- Evaluates the underlying container up to, and a bit beyond, the
-- selected element. The exact amount depends on available height
-- for drawing and 'listItemHeight'. At most, it will evaluate up to
-- element @(i + h + 1)@ where @i@ is the selected index and @h@ is the
-- available height.
--
-- Note that this function renders the list with the 'listAttr' as
-- the default attribute and then uses 'listSelectedAttr' as the
-- default attribute for the selected item if the list is not focused
-- or 'listSelectedFocusedAttr' otherwise. This is provided as a
-- convenience so that the item rendering function doesn't have to be
-- concerned with attributes, but if those attributes are undesirable
-- for your purposes, 'forceAttr' can always be used by the item
-- rendering function to ensure that another attribute is used instead.
renderList :: (Ord n, Show n)
           => (Bool -> e -> Widget n)
           -- ^ Rendering function, True for the selected element
           -> Bool
           -- ^ Whether the list has focus
           -> IOListWidget n e
           -- ^ The List to be rendered
           -> Widget n
           -- ^ rendered widget
renderList drawElem = renderListWithIndex $ const drawElem

-- | Like 'renderList', except the render function is also provided with
-- the index of each element.
--
-- Has the same evaluation characteristics as 'renderList'.
renderListWithIndex :: (Ord n, Show n)
                    => (Int -> Bool -> e -> Widget n)
                    -- ^ Rendering function, taking index, and True for
                    -- the selected element
                    -> Bool
                    -- ^ Whether the list has focus
                    -> IOListWidget n e
                    -- ^ The List to be rendered
                    -> Widget n
                    -- ^ rendered widget
renderListWithIndex drawElem foc l =
    withDefAttr listAttr $
    drawListElements foc l drawElem

imap :: (Traversable t) => (Int -> a -> b) -> t a -> t b
imap f xs =
    let act = traverse (\a -> get >>= \i -> put (i + 1) $> f i a) xs
    in evalState act 0

toListPure :: IOList e -> [e]
toListPure (IOList _ l) = toListPure' l

-- Gather up the current elements
toListPure' :: IOListL e -> [e]
toListPure' ILNil = []
toListPure' (ILCons e l) = e : toListPure l
toListPure' (ILLoad _) = []

slicePure :: Int -> Int -> IOList e -> [e]
slicePure i n = take n . drop i . toListPure

-- | Draws the list elements.
--
-- Evaluates the underlying container up to, and a bit beyond, the
-- selected element. The exact amount depends on available height
-- for drawing and 'listItemHeight'. At most, it will evaluate up to
-- element @(i + h + 1)@ where @i@ is the selected index and @h@ is the
-- available height.
drawListElements :: (Ord n, Show n)
                 => Bool
                 -> IOListWidget n e
                 -> (Int -> Bool -> e -> Widget n)
                 -> Widget n
drawListElements foc l drawElem =
    Widget Greedy Greedy $ do
        c <- getContext

        -- Take (numPerHeight * 2) elements, or whatever is left
        let es = slicePure start (numPerHeight * 2) (l^.listElementsL)

            idx = fromMaybe 0 (l^.listSelectedL)

            start = max 0 $ idx - numPerHeight + 1

            -- The number of items to show is the available height
            -- divided by the item height...
            initialNumPerHeight = (c^.availHeightL) `div` (l^.listItemHeightL)
            -- ... but if the available height leaves a remainder of
            -- an item height then we need to ensure that we render an
            -- extra item to show a partial item at the top or bottom to
            -- give the expected result when an item is more than one
            -- row high. (Example: 5 rows available with item height
            -- of 3 yields two items: one fully rendered, the other
            -- rendered with only its top 2 or bottom 2 rows visible,
            -- depending on how the viewport state changes.)
            numPerHeight = initialNumPerHeight +
                           if initialNumPerHeight * (l^.listItemHeightL) == c^.availHeightL
                           then 0
                           else 1

            off = start * (l^.listItemHeightL)

            drawnElements = flip imap es $ \i e ->
                let j = i + start
                    isSelected = Just j == l^.listSelectedL
                    elemWidget = drawElem j isSelected e
                    selItemAttr = if foc
                                  then withDefAttr listSelectedFocusedAttr
                                  else withDefAttr listSelectedAttr
                    makeVisible = if isSelected
                                  then visible . selItemAttr
                                  else id
                in makeVisible elemWidget

        render $ viewport (l^.listNameL) Vertical $
                 translateBy (Location (0, off)) $
                 vBox $ drawnElements

-- | Insert an item into a list at the specified position.
--
-- Complexity: the worse of 'splitAt' and `<>` for the container type.
--
-- @
-- listInsert for 'List': O(n)
-- listInsert for 'Seq.Seq': O(log(min(i, lengthIL n - i)))
-- @
listInsert :: Int
           -- ^ The position at which to insert (0 <= i <= size)
           -> e
           -- ^ The element to insert
           -> IOListWidget n e
           -> IO (IOListWidget n e)
listInsert pos e l = do
    let es = l^.listElementsL
        newSel = case l^.listSelectedL of
            Nothing -> 0
            Just s -> if pos <= s
                      then s + 1
                      else s
    (front, back) <- splitAt pos es
    let new = concatILPure front (cons e back)
    return $ l & listSelectedL .~ Just newSel
               & listElementsL .~ new

-- | Remove an element from a list at the specified position.
--
-- Applies 'splitAt' two times: first to split the structure at the
-- given position, and again to remove the first element from the tail.
-- Consider the asymptotics of `splitAt` for the container type when
-- using this function.
--
-- Complexity: the worse of 'splitAt' and `<>` for the container type.
--
-- @
-- listRemove for 'List': O(n)
-- listRemove for 'Seq.Seq': O(log(min(i, n - i)))
-- @
listRemove :: Int
           -- ^ The position at which to remove an element (0 <= i <
           -- size)
           -> IOListWidget n e
           -> IO (IOListWidget n e)
listRemove pos l | nullIL (l^.listElementsL) = return l
                 | pos >= lengthIL (l ^. listElementsL) = return l
                 | otherwise = do
    let es = l^.listElementsL
        newSel = case l^.listSelectedL of
            Nothing -> 0
            Just s | pos == 0 -> 0
                   | pos == s -> pos - 1
                   | pos  < s -> s - 1
                   | otherwise -> s
    (front, rest) <- splitAt pos es
    (_, back)     <- splitAt 1 rest
    let es' = front `concatILPure` back
    return $ l & listSelectedL .~ (if nullIL es' then Nothing else Just newSel)
               & listElementsL .~ es'


-- | Replace the contents of a list with a new set of elements and
-- update the new selected index. If the list is empty, empty selection
-- is used instead. Otherwise, if the specified selected index (via
-- 'Just') is not in the list bounds, zero is used instead.
--
-- Complexity: same as 'splitAt' for the container type.
listReplace :: IOList e
            -> Maybe Int
            -> IOListWidget n e
            -> IOListWidget n e
listReplace es idx l =
    let l' = l & listElementsL .~ es
        es_n = lengthIL es
        newSel = if nullIL es then Nothing else inBoundsOrZero <$> idx
        inBoundsOrZero i
            | i < es_n = i
            | otherwise = 0
    in l' & listSelectedL .~ newSel

-- | Move the list selected index up by one. (Moves the cursor up,
-- subtracts one from the index.)
listMoveUp :: IOListWidget n e
           -> IO (IOListWidget n e)
listMoveUp = listMoveBy (-1)

-- | Move the list selected index up by one page.
listMovePageUp
  :: (Ord n)
  => IOListWidget n e -> EventM n (IOListWidget n e)
listMovePageUp = listMoveByPages (-1::Double)

-- | Move the list selected index down by one. (Moves the cursor down,
-- adds one to the index.)
listMoveDown :: IOListWidget n e
             -> IO (IOListWidget n e)
listMoveDown = listMoveBy 1

-- | Move the list selected index down by one page.
listMovePageDown :: (Ord n)
                 => IOListWidget n e
                 -> EventM n (IOListWidget n e)
listMovePageDown = listMoveByPages (1::Double)

-- | Move the list selected index by some (fractional) number of pages.
listMoveByPages :: (Ord n, Fractional m, RealFrac m)
                => m
                -> IOListWidget n e
                -> EventM n (IOListWidget n e)
listMoveByPages pages theList = do
    v <- lookupViewport (theList^.listNameL)
    case v of
        Nothing -> return theList
        Just vp -> do
            let nElems = round $ pages * fromIntegral (vp^.vpSize._2) /
                                 fromIntegral (theList^.listItemHeightL)
            liftIO $ listMoveBy nElems theList

-- | Move the list selected index.
--
-- If the current selection is @Just x@, the selection is adjusted by
-- the specified amount. The value is clamped to the extents of the list
-- (i.e. the selection does not "wrap").
--
-- If the current selection is @Nothing@ (i.e. there is no selection)
-- and the direction is positive, set to @Just 0@ (first element),
-- otherwise set to @Just (lengthIL - 1)@ (last element).
--
-- Complexity: same as 'splitAt' for the container type.
--
-- @
-- listMoveBy for 'List': O(1)
-- listMoveBy for 'Seq.Seq': O(log(min(i,n-i)))
-- @
listMoveBy :: Int
           -> IOListWidget n e
           -> IO (IOListWidget n e)
listMoveBy amt l = do
    let target = case l ^. listSelectedL of
            Nothing
                | amt > 0 -> 0
                | otherwise -> lengthIL (l ^. listElementsL) - 1
            Just i -> max 0 (amt + i)  -- don't be negative
    listMoveTo target l

listMoveTo :: Int
           -> IOListWidget n e
           -> IO (IOListWidget n e)
listMoveTo pos l = do
    let len = lengthIL (l ^. listElementsL)
        i = if pos < 0 then len - pos else pos
    {-
    v <- verify (l ^. listElementsL)
    case v of
      Just err -> error err
      Nothing -> return ()
      -}
    (f, t) <- splitAt i (l ^. listElementsL)  -- split at i
    let newSel = clamp 0 (if nullIL t then lengthIL (l ^. listElementsL) - 1
                                      else i) i
    -- Buffer, should be the same as the vertical size of the list
    t' <- force (l ^. listBufferSizeL) t
    return $ l & (listSelectedL .~ if not (nullIL (l ^. listElementsL))
                                    then Just newSel
                                    else Nothing )
               & listElementsL .~ (f `concatILPure` t')


-- | Return a list's selected element, if any.
--
-- Only evaluates as much of the container as needed.
--
-- Complexity: same as 'splitAt' for the container type.
--
-- @
-- listSelectedElement for 'List': O(1)
-- listSelectedElement for 'Seq.Seq': O(log(min(i, n - i)))
-- @
listSelectedElement :: IOListWidget n e
                    -> IO (Maybe (Int, e))
listSelectedElement l = do
    let sel = l ^. listSelectedL
    case sel of
      Nothing -> return Nothing
      Just sel_n -> do
        el <- indexIL (l ^. listElementsL) sel_n
        return ((,) <$> sel <*> el)

