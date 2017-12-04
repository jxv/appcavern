{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AppTavern.Service where

import Fluid.Server

import qualified AppTavern.Api as Api
import qualified AppTavern.Api.V0 as V0
import AppTavern.Import

instance ServiceThrower Handler
instance V0.Api'Thrower Handler
