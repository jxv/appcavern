-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module
module AppCavern.Api.V0
  ( api'version
  , api'pull
  , api'handler
  , api'spec
  , Api'Thrower(..)
  , Api'Service(..)
  , Hello(..)
  , AddComment(..)
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Except as M
import qualified Data.IORef as IO
import qualified Data.String as P (IsString)

import qualified Fluid.Imports as R
import qualified Fluid.Server as C



--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
api'version :: C.Version
api'version = C.Version 0 0

api'pull :: C.Pull
api'pull = C.Pull "http" "127.0.0.1" "/api" 8080

--------------------------------------------------------
-- Interfaces
--------------------------------------------------------

-- Thrower
class C.ServiceThrower m => Api'Thrower m where
  api'throw :: () -> m a
  api'throw = C.serviceThrow P.. R.toJSON P.. C.toVal

-- Service
class P.Monad m => Api'Service meta m where
  api'Hello :: meta -> Hello -> m R.Text
  api'AddComment :: meta -> AddComment -> m ()

instance Api'Service meta m => Api'Service meta (M.ExceptT C.Response m) where
  api'Hello _meta = M.lift  P.. api'Hello _meta
  api'AddComment _meta = M.lift  P.. api'AddComment _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Struct: Hello
data Hello = Hello
  { helloTarget :: R.Text
  } deriving (P.Show, P.Eq)

-- Struct: AddComment
data AddComment = AddComment
  { addCommentMessage :: R.Text
  } deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

--------------------------------------------------------
-- Request handling
--------------------------------------------------------

-- Handler
api'handler
  :: (Api'Service meta m, R.MonadIO m, R.MonadCatch m)
  => (xtra -> C.Hooks m () meta)
  -> xtra
  -> C.Request
  -> m (P.Either C.Response C.Response)
api'handler _hooksBuilder xtra C.Request{meta,query} = R.catch
  (M.runExceptT P.$ do
    meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
    let _hooks = _hooksBuilder xtra
    xformMeta <- M.lift P.$ C.metaMiddleware _hooks meta'
    envRef <- R.liftIO C.emptyEnv
    variableBaseCount <- R.liftIO (R.size P.<$> IO.readIORef envRef)
    _limits <- M.lift P.$ C.sandboxLimits _hooks xformMeta
    let _limits' = _limits
          { C.variables = P.fmap (P.+ variableBaseCount) (C.variables _limits)
          }
    _serviceCallCountRef <- R.liftIO (IO.newIORef 0)
    _lambdaCountRef <- R.liftIO (IO.newIORef 0)
    _exprCountRef <- R.liftIO (IO.newIORef 0)
    let evalConfig = C.EvalConfig
          { C.limits = _limits'
          , C.langServiceCallCount = _serviceCallCountRef
          , C.langLambdaCount = _lambdaCountRef
          , C.langExprCount = _exprCountRef
          , C.apiCall = api'ApiCall xformMeta
          }
    query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
    vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
    P.return P.$ C.Response'Success (R.toJSON vals) _limits)
  (\(C.ThrownValue _err) -> P.return P.. P.Left P.$ C.Response'Error (C.ResponseError'Service _err))

-- API
api'ApiCall :: (Api'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
api'ApiCall meta' apiCall' = case C.parseApiCall api'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow (C.RuntimeError'UnrecognizedCall P.$ C.apiCallName apiCall')
  P.Just x' -> case x' of
    Api'Api'Hello a' -> C.toVal P.<$> api'Hello meta' a'
    Api'Api'AddComment a' -> C.toVal P.<$> api'AddComment meta' a'

-- API Parser
api'ApiParser :: C.ApiParser Api'Api
api'ApiParser = C.ApiParser
  { hollow = R.empty
  , struct = R.fromList
     [ ("Hello", v Api'Api'Hello)
     , ("AddComment", v Api'Api'AddComment)
     ]
  , enumeration = R.empty
  , wrap = R.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data Api'Api
  = Api'Api'Hello Hello
  | Api'Api'AddComment AddComment
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.ToVal Hello where
  toVal Hello
    { helloTarget
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("target", C.toVal helloTarget)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Hello
      P.<$> C.getMember _m "target"
    _ -> P.Nothing

instance R.ToJSON Hello where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Hello where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal AddComment where
  toVal AddComment
    { addCommentMessage
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("message", C.toVal addCommentMessage)
    ]

instance C.FromVal AddComment where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> AddComment
      P.<$> C.getMember _m "message"
    _ -> P.Nothing

instance R.ToJSON AddComment where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON AddComment where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

api'spec :: R.Value
api'spec = v
  where P.Just v = R.decode "{\"fluid\":{\"major\":0,\"minor\":0},\"pull\":{\"protocol\":\"http\",\"name\":\"Api\",\"host\":\"127.0.0.1\",\"meta\":\"Unit\",\"path\":\"/api\",\"port\":8080,\"error\":\"Unit\"},\"schema\":{\"Hello\":{\"m\":[{\"target\":\"String\"}],\"o\":\"String\"},\"AddComment\":{\"m\":[{\"message\":\"String\"}],\"o\":\"Unit\"}},\"version\":{\"major\":0,\"minor\":0}}"

