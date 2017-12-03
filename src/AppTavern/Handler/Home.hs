{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module AppTavern.Handler.Home where

import Text.Julius (RawJS (..))
import Data.Text.Conversions

import AppTavern.Import
import AppTavern.Handler.Api ()

import qualified AppTavern.Api as Api
import qualified AppTavern.Api.V0 as V0

getHomeR :: Handler Html
getHomeR = do
  apps <- Api.api'GetApps () (Api.GetApps 0 100)
  defaultLayout $ do
    let jsFormId = "js-commentForm" :: Text
    let jsFormTextareaId = "js-createCommentTextarea" :: Text
    aDomId <- newIdent
    setTitle "App Tavern"
    $(widgetFile "homepage")
