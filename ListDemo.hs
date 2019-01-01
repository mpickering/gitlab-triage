{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import Control.Monad.IO.Class (liftIO)

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified IOList as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

import Data.List

import qualified IOList
import IOList (IOList(..), IOListL(..))

testIOList :: IOList Int
testIOList = IOList.concatILPure ([0..9]) (gen 9)

testIOListW = IOList.list () testIOList 1 5


gen :: Int -> (IOList Int)
gen 0 = IOList.nil
gen m = IOList (m * 10) (loop 0)
  where
    loop :: Int -> IOListL Int
--    loop 10 v = gen (m - 1) (n + 10)
    loop k = ILLoad
      (return ([0..9] `IOList.concatILPure` gen (m - 1)))

drawUI :: (Show a) => L.IOListWidget () a -> [Widget ()]
drawUI l = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ IOList.lengthIL $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 5 $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , str $ "Loaded Elems:" ++ (show (length (IOList.toListPure (l ^. IOList.listElementsL))))
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: IOList.IOListWidget () Int
         -> T.BrickEvent () e
         -> T.EventM () (T.Next (IOList.IOListWidget () Int))
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue =<< L.handleListEvent ev l
appEvent l _ = M.continue l

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

initialState = testIOListW

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (IOList.IOListWidget () Int) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
