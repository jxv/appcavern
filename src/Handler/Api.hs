{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Api where

import Import
import Fluid.Server
import Fluid.Endpoint

import qualified AppCavern.Api as Api
import qualified AppCavern.Api.V0 as V0

import AppCavern.App

getApiR :: Handler Value
getApiR = return Api.api'spec

postApiR :: Handler Value
postApiR = do
  v <- (requireJsonBody :: Handler Value)
  let handlerMap = Api.api'handlerMap (\() -> defHooks) ()
  runFluid handlerMap v

instance ServiceThrower Handler
instance Api.Api'Thrower Handler

instance V0.Api'Service () Handler where
  api'Hello () h = do
    let msg = "Hello, " `mappend` (V0.helloTarget h)
    liftIO $ putStrLn msg
    return msg

  api'AddComment () V0.AddComment{V0.addCommentMessage=msg} = runDB $ insert_ $ Comment msg Nothing

  api'AddApp = addApp
  api'GetApps = getApps
