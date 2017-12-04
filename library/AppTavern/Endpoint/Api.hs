{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AppTavern.Endpoint.Api where

import Fluid.Server
import Fluid.Endpoint

import qualified AppTavern.Api as Api
import qualified AppTavern.Api.V0 as V0
import AppTavern.Import
import AppTavern.App

getApiR :: Handler Value
getApiR = return Api.api'spec

unlimited :: Limits
unlimited = Limits Nothing Nothing Nothing Nothing

postApiR :: Handler Value
postApiR = do
  v <- (requireJsonBody :: Handler Value)
  let handlerMap = Api.api'handlerMap (\() -> defHooks {sandboxLimits = \_ -> return unlimited}) ()
  runFluid handlerMap v

instance ServiceThrower Handler
instance V0.Api'Thrower Handler

instance V0.Api'Service () Handler where
  api'Hello () h = do
    let msg = "Hello, " `mappend` (V0.helloTarget h)
    liftIO $ putStrLn msg
    return msg
  api'AddApp = addApp
  api'GetApps = getApps
  api'CountApps = countApps
