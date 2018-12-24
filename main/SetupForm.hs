{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module SetupForm where


import Brick.Forms
import Brick
import qualified Brick.Widgets.Border as B
import Data.Generics.Product

import qualified Data.Text as T

import GitLab.Common
import Namespace
import Config

type SetupForm = Form UserConfig () Name

-- TODO: Set invalid input style
setupForm :: Form UserConfig e Name
setupForm = newForm fields
                    (UserConfig (AccessToken "")
                                "gitlab.com"
                                (ProjectId 13083))

  where
    fields = [ row "Token" tokenField
             , row "Base URL" urlField
             , row "Project ID" projectField ]

    row s w = vLimit 1 . ((hLimit 11 $ padLeft Max $ txt s) <+> B.vBorder <+>) @@= w

    tokenField = editField (field @"token" . typed @T.Text)
                           (FormArea "token")
                           (Just 1)
                           id
                           validateToken
                           (txt . T.intercalate "\n")
                           id

    urlField = editTextField (field @"url")
                             (FormArea "url")
                             (Just 1)


    projectField = editShowableField (field @"project" . typed @Int)
                                     (FormArea "project")

    validateToken :: [T.Text] -> Maybe T.Text
    validateToken [t] = if T.length t == 20 then Just t else Nothing
    validateToken _   = Nothing
