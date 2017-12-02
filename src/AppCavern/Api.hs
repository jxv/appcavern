-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module
module AppCavern.Api
  ( api'handlerMap
  , api'spec
  , V0.Api'Service(..)
  , V0.Api'Thrower(..)
  , V0.api'pull
  , V0.UserId(..)
  , V0.Url(..)
  , V0.AppId(..)
  , V0.Hello(..)
  , V0.AddComment(..)
  , V0.User(..)
  , V0.Date(..)
  , V0.AppSpec(..)
  , V0.App(..)
  , V0.AddApp(..)
  , V0.GetApps(..)
  , V0.Device(..)
  , V0.Author(..)
  , V0.Author'Name'Members(..)
  , V0.Author'User'Members(..)
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R

import qualified AppCavern.Api.V0 as V0
  ( Api'Service(..)
  , Api'Thrower(..)
  , api'handler
  , api'version
  , api'pull
  , api'spec
  , UserId(..)
  , Url(..)
  , AppId(..)
  , Hello(..)
  , AddComment(..)
  , User(..)
  , Date(..)
  , AppSpec(..)
  , App(..)
  , AddApp(..)
  , GetApps(..)
  , Device(..)
  , Author(..)
  , Author'Name'Members(..)
  , Author'User'Members(..)
  )

api'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.Api'Service meta0 m
    )
  => (xtra -> C.Hooks m () meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
api'handlerMap hooks0 xtra = R.fromList
    [ (0, (0, V0.api'handler hooks0 xtra))
    ]

api'spec :: R.Value
api'spec = R.toJSON
  [ V0.api'spec
  ]
