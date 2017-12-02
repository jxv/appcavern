{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Julius (RawJS (..))

import qualified AppTavern.Api.V0 as V0

getHomeR :: Handler Html
getHomeR = do
  apps <- fmap (fmap entityVal) $ runDB $ selectList [] []
  defaultLayout $ do
    let jsFormId = "js-commentForm" :: Text
    let jsFormTextareaId = "js-createCommentTextarea" :: Text
    aDomId <- newIdent
    setTitle "App Tavern"
    $(widgetFile "homepage")
