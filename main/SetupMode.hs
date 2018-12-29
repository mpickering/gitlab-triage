{-# LANGUAGE OverloadedStrings #-}
module SetupMode where

import Brick hiding (continue, halt)
import Brick.Forms

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Types ( Widget )

import Control.Monad.IO.Class (liftIO)

import Control.Applicative.Combinators ()

import Config
import Model
import SetupForm
import Common

drawSetup :: Form UserConfig e Name -> FilePath -> [Widget Name]
drawSetup form cfg = [ui, background]
  where
    background = fill '@'
    formBox = C.center . joinBorders
       . B.border . hLimitPercent 75 . vLimit 5 $ renderForm form

    ui = formBox <=> fileFooter <=> setupFooter

    fileFooter = txt "Config will be written to: " <+> str cfg

    setupFooter =
      vLimit 1 $
        txt "esc - quit; tab - next field; enter - submit"

setupHandler :: SetupForm -> FilePath -> Handler AppState
setupHandler f cfg _ e = do
  f' <- handleFormEvent e f
  let s' = AppState (Setup f' cfg)
  case e of
    T.VtyEvent (V.EvKey V.KEnter []) ->
      if allFieldsValid f'
        then do
                let c = formState f'
                liftIO $ writeConfig c
                liftIO (initialise c) >>= M.continue

        else M.continue s'
    T.VtyEvent (V.EvKey V.KEsc []) -> M.halt s'
    _ -> M.continue s'
