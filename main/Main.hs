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
module Main(main) where

import GitLab.Tickets

import qualified Data.Text as T

import Brick hiding (continue, halt)
import Brick.Forms

import Control.Lens (view, set, cloneLens, Traversal', firstOf)
import Control.Monad (void)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Types ( Widget )
import Brick.Util (fg, on)

import Data.Generics.Product
import Data.Generics.Sum

import Control.Monad.IO.Class (liftIO)

import Cursor.Text

import Control.Applicative.Combinators ()

import Config
import Model
import SetupForm
import Autocomplete
import TicketList
import Common
import IssuePage
import Parsers
import TextCursor
import SetupMode

main :: IO ()
main = do
  --deleteConfig
  conf <- readConfig
  st <- selectInitialState conf
  gui st

theApp :: M.App AppState () Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = theMap
          }

gui :: AppState -> IO ()
gui s = void $ M.defaultMain theApp s

{- Initialisation -}

selectInitialState :: Either a UserConfig -> IO AppState
selectInitialState e =
  case e of
    Left {} -> setupState
    Right c -> initialise c


setupState :: IO AppState
setupState = do
  configFile <- getConfigFile
  return $ AppState (Setup setupForm configFile)

{- Drawing functions -}

drawUI :: AppState -> [Widget Name]
drawUI (AppState a) = case a of
                        Setup form cfg -> drawSetup form cfg
                        Operational o -> drawMain o

drawMain :: OperationalState -> [Widget Name]
drawMain l =
    let dialogWindow = drawDialog (view (typed @DialogMode) l)
    in dialogWindow :
      (case view (field @"mode") l of
        TicketListView ts -> drawTicketList l ts
        IssueView st -> drawIssueView (view (typed @FooterMode) l) st)


drawDialog :: DialogMode -> Widget Name
drawDialog NoDialog = emptyWidget
drawDialog (IssuePageDialog _ _ ac) = drawAutocompleteDialog ac
drawDialog (SearchParamsDialog _ _ ac) = drawAutocompleteDialog ac

drawAutocompleteDialog :: (Ord n, Show n) => Autocomplete s n a -> Widget n
drawAutocompleteDialog ac = dBox
  where
    dBox = C.centerLayer . joinBorders
            . B.border . hLimitPercent 75 . vLimitPercent 75 $ dialog

    dialog = drawAutocomplete ac



--------------------------------------------------------------
{- Handlers -}
{-
The event handling strategy

1. First handle any truly global events like resizing. We need to invalidate the
cache when this happens.
2. Handle events which pertain to the footer type
3. Then delegate to handling events in the main application window.
-}

handleMain :: Handler OperationalState
handleMain =
  globalHandler modeHandler

-- Main handler which delegates to either the setup or main mode
appEvent :: Handler AppState
appEvent a@(AppState mm) ev =
  case mm of
    Setup f cfg -> setupHandler f cfg a ev
    Operational o -> liftHandler typed o Operational handleMain a ev

globalHandler ::  Handler OperationalState -> Handler OperationalState
globalHandler k l re = do
 case re of
   T.VtyEvent (V.EvResize {}) -> invalidateCache
   _ -> return ()
 case view typed l of
    NoDialog ->
      case view typed l of
        FooterMessage _ -> infoFooterHandler k l re
        FooterInfo -> infoFooterHandler k l re
        FooterInput m tc -> inputFooterHandler m tc k l re
    dc -> dialogHandler dc l re

dialogHandler :: DialogMode -> Handler OperationalState
dialogHandler dc l re =
  case re of
      T.VtyEvent (V.EvKey e []) -> case e of
                                     V.KEsc -> M.continue (resetDialog l)
                                     V.KEnter -> dispatchDialogInput dc l
                                     _ -> handleDialogEvent dc l re
      _ -> M.continue l

handleDialogEvent :: DialogMode
                  -> Handler OperationalState
handleDialogEvent dc l re  =
  case dc of
    IssuePageDialog a1 a2 mac     -> do_one mac (\mac' -> IssuePageDialog a1 a2 mac')
    SearchParamsDialog a1 a2 mac -> do_one mac (\mac' -> SearchParamsDialog a1 a2 mac')
    NoDialog            -> error "Handling dialog events when not in dialog mode"
  where
    do_one mac wrap = do
      mac' <- handleAutocompleteEvent mac re
      M.continue (set (typed @DialogMode) (wrap mac') l)


-- TODO: This duplication is a bit unsatisfactory and also the set is
-- partial because it relies on being in specific mode.
dispatchDialogInput :: DialogMode
                    -> OperationalState
                    -> EventM Name (Next OperationalState)
dispatchDialogInput (IssuePageDialog check place mac) l =
  let tc = view (field @"autocompleteCursor") mac
  in
  case check (rebuildTextCursor tc) of
    Nothing  -> M.continue (resetDialog l)
    Just mid -> do
      M.continue $ set (typed @Mode . _Ctor @"IssueView" . cloneLens place)
                       (Just mid) (resetDialog l)
dispatchDialogInput (SearchParamsDialog check place mac) l = do
  if (T.null rbc)
    then (update Nothing)
    else (case check rbc of
            Nothing  -> M.continue (resetDialog l)
            Just mid -> update (Just mid))
  where
    tc = view (field @"autocompleteCursor") mac
    rbc = rebuildTextCursor tc
    update v = do
      let new_state = set (typed @Mode . _Ctor @"TicketListView"
                       . cloneLens place) v (resetDialog l)
          search_params =
            case firstOf (typed @Mode . _Ctor @"TicketListView" . typed @GetIssuesParams)
                         new_state of
              Just s -> s
              -- This should never happen
              Nothing -> defaultSearchParams

      tl <- liftIO $ loadTicketList search_params (view (typed @AppConfig) new_state)
      M.continue (set typed (TicketListView tl) new_state)
dispatchDialogInput NoDialog _ = error "NoDialog when handling dialog event"



infoFooterHandler :: Handler OperationalState -> Handler OperationalState
infoFooterHandler k l re@(T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'g') [] ->
      M.continue (set typed (FooterInput FGoto emptyTextCursor) l)
    _ev -> k l re
infoFooterHandler k l re = k l re

inputFooterHandler :: FooterInputMode
                   -> TextCursor
                   -> Handler OperationalState
                   -> Handler OperationalState
inputFooterHandler m tc k l re@(T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.continue (resetFooter l)
    V.EvKey V.KEnter [] -> dispatchFooterInput m tc l
    _ ->
      handleTextCursorEvent
        (\tc' -> k (set typed (FooterInput m tc') l) re)
        tc re
inputFooterHandler _ _ k l re = k l re

-- | What happens when we press enter in footer input mode
dispatchFooterInput :: FooterInputMode
                    -> TextCursor
                    -> OperationalState
                    -> EventM n (Next OperationalState)
dispatchFooterInput FGoto tc l =
  case checkGotoInput (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just iid -> do
      displayError (loadByIid iid (view typed l))
                   (M.continue . resetFooter . issueView l)
                   l
dispatchFooterInput (FGen _ check place)  tc l =
  case check (rebuildTextCursor tc) of
    Nothing -> M.continue (resetFooter l)
    Just ls -> do
      M.continue $ set (issueEdit . cloneLens place) (Just ls) (resetFooter l)


issueEdit :: Traversal' OperationalState EditIssue
issueEdit = typed @Mode . _Ctor @"IssueView" . field @"updates" . typed @EditIssue

resetDialog :: OperationalState -> OperationalState
resetDialog l = (set typed NoDialog l)

resetFooter :: OperationalState -> OperationalState
resetFooter l = (set typed FooterInfo l)

modeHandler :: Handler OperationalState
modeHandler l e =
  case view typed l of
    IssueView tl -> issuePageHandler tl l e
    TicketListView tl -> ticketListHandler tl l e


{- Styles -}

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: AppState -> A.AttrMap
theMap _ = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    , ("changed", V.red `on` V.black)
    , ("default", V.defAttr )
    ]



