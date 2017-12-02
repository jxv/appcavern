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
  api'AddApp :: meta -> AddApp -> m AppId
  api'GetApps :: meta -> GetApps -> m [App]

instance Api'Service meta m => Api'Service meta (M.ExceptT C.Response m) where
  api'Hello _meta = M.lift  P.. api'Hello _meta
  api'AddComment _meta = M.lift  P.. api'AddComment _meta
  api'AddApp _meta = M.lift  P.. api'AddApp _meta
  api'GetApps _meta = M.lift  P.. api'GetApps _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: UserId
newtype UserId = UserId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Wrap: Url
newtype Url = Url R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Wrap: AppId
newtype AppId = AppId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText,  P.Show)

-- Struct: Hello
data Hello = Hello
  { helloTarget :: R.Text
  } deriving (P.Show, P.Eq)

-- Struct: AddComment
data AddComment = AddComment
  { addCommentMessage :: R.Text
  } deriving (P.Show, P.Eq)

-- Struct: User
data User = User
  { userName :: R.Text
  , userGithub :: (P.Maybe Url)
  } deriving (P.Show, P.Eq)

-- Struct: Date
data Date = Date
  { dateYear :: P.Int
  , dateMonth :: P.Int
  , dateDay :: P.Int
  } deriving (P.Show, P.Eq)

-- Struct: AppSpec
data AppSpec = AppSpec
  { appSpecName :: R.Text
  , appSpecSubtitle :: R.Text
  , appSpecDevice :: Device
  , appSpecInfo :: R.Text
  , appSpecAuthors :: [Author]
  , appSpecPorters :: [Author]
  , appSpecPage :: (P.Maybe Url)
  , appSpecImg :: Url
  , appSpecLink :: Url
  } deriving (P.Show, P.Eq)

-- Struct: App
data App = App
  { appId :: AppId
  , appName :: R.Text
  , appSubtitle :: R.Text
  , appDevice :: Device
  , appInfo :: R.Text
  , appAuthors :: [Author]
  , appPorters :: [Author]
  , appPage :: (P.Maybe Url)
  , appReleased :: Date
  , appImg :: Url
  , appLink :: Url
  } deriving (P.Show, P.Eq)

-- Struct: AddApp
data AddApp = AddApp
  { addAppSpec :: AppSpec
  } deriving (P.Show, P.Eq)

-- Struct: GetApps
data GetApps = GetApps
  { getAppsStart :: P.Int
  , getAppsSize :: P.Int
  } deriving (P.Show, P.Eq)

-- Enumeration: Device
data Device
  = Device'Gcw0 
  deriving (P.Show, P.Eq)

-- Enumeration: Author
data Author
  = Author'Name Author'Name'Members
  | Author'User Author'User'Members
  deriving (P.Show, P.Eq)

data Author'Name'Members = Author'Name'Members
  { author'NameName :: R.Text
  } deriving (P.Show, P.Eq)

data Author'User'Members = Author'User'Members
  { author'UserIdent :: UserId
  , author'UserUser :: User
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
    Api'Api'AddApp a' -> C.toVal P.<$> api'AddApp meta' a'
    Api'Api'GetApps a' -> C.toVal P.<$> api'GetApps meta' a'

-- API Parser
api'ApiParser :: C.ApiParser Api'Api
api'ApiParser = C.ApiParser
  { hollow = R.empty
  , struct = R.fromList
     [ ("Hello", v Api'Api'Hello)
     , ("AddComment", v Api'Api'AddComment)
     , ("AddApp", v Api'Api'AddApp)
     , ("GetApps", v Api'Api'GetApps)
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
  | Api'Api'AddApp AddApp
  | Api'Api'GetApps GetApps
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.ToVal UserId where
  toVal (UserId _w) = C.toVal _w

instance C.FromVal UserId where
  fromVal _v = UserId P.<$> C.fromVal _v

instance R.ToJSON UserId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON UserId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Url where
  toVal (Url _w) = C.toVal _w

instance C.FromVal Url where
  fromVal _v = Url P.<$> C.fromVal _v

instance R.ToJSON Url where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Url where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal AppId where
  toVal (AppId _w) = C.toVal _w

instance C.FromVal AppId where
  fromVal _v = AppId P.<$> C.fromVal _v

instance R.ToJSON AppId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON AppId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

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

instance C.ToVal User where
  toVal User
    { userName
    , userGithub
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal userName)
    , ("github", C.toVal userGithub)
    ]

instance C.FromVal User where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> User
      P.<$> C.getMember _m "name"
      P.<*> C.getMember _m "github"
    _ -> P.Nothing

instance R.ToJSON User where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON User where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Date where
  toVal Date
    { dateYear
    , dateMonth
    , dateDay
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("year", C.toVal dateYear)
    , ("month", C.toVal dateMonth)
    , ("day", C.toVal dateDay)
    ]

instance C.FromVal Date where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Date
      P.<$> C.getMember _m "year"
      P.<*> C.getMember _m "month"
      P.<*> C.getMember _m "day"
    _ -> P.Nothing

instance R.ToJSON Date where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Date where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal AppSpec where
  toVal AppSpec
    { appSpecName
    , appSpecSubtitle
    , appSpecDevice
    , appSpecInfo
    , appSpecAuthors
    , appSpecPorters
    , appSpecPage
    , appSpecImg
    , appSpecLink
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal appSpecName)
    , ("subtitle", C.toVal appSpecSubtitle)
    , ("device", C.toVal appSpecDevice)
    , ("info", C.toVal appSpecInfo)
    , ("authors", C.toVal appSpecAuthors)
    , ("porters", C.toVal appSpecPorters)
    , ("page", C.toVal appSpecPage)
    , ("img", C.toVal appSpecImg)
    , ("link", C.toVal appSpecLink)
    ]

instance C.FromVal AppSpec where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> AppSpec
      P.<$> C.getMember _m "name"
      P.<*> C.getMember _m "subtitle"
      P.<*> C.getMember _m "device"
      P.<*> C.getMember _m "info"
      P.<*> C.getMember _m "authors"
      P.<*> C.getMember _m "porters"
      P.<*> C.getMember _m "page"
      P.<*> C.getMember _m "img"
      P.<*> C.getMember _m "link"
    _ -> P.Nothing

instance R.ToJSON AppSpec where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON AppSpec where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal App where
  toVal App
    { appId
    , appName
    , appSubtitle
    , appDevice
    , appInfo
    , appAuthors
    , appPorters
    , appPage
    , appReleased
    , appImg
    , appLink
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("id", C.toVal appId)
    , ("name", C.toVal appName)
    , ("subtitle", C.toVal appSubtitle)
    , ("device", C.toVal appDevice)
    , ("info", C.toVal appInfo)
    , ("authors", C.toVal appAuthors)
    , ("porters", C.toVal appPorters)
    , ("page", C.toVal appPage)
    , ("released", C.toVal appReleased)
    , ("img", C.toVal appImg)
    , ("link", C.toVal appLink)
    ]

instance C.FromVal App where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> App
      P.<$> C.getMember _m "id"
      P.<*> C.getMember _m "name"
      P.<*> C.getMember _m "subtitle"
      P.<*> C.getMember _m "device"
      P.<*> C.getMember _m "info"
      P.<*> C.getMember _m "authors"
      P.<*> C.getMember _m "porters"
      P.<*> C.getMember _m "page"
      P.<*> C.getMember _m "released"
      P.<*> C.getMember _m "img"
      P.<*> C.getMember _m "link"
    _ -> P.Nothing

instance R.ToJSON App where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON App where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal AddApp where
  toVal AddApp
    { addAppSpec
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("spec", C.toVal addAppSpec)
    ]

instance C.FromVal AddApp where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> AddApp
      P.<$> C.getMember _m "spec"
    _ -> P.Nothing

instance R.ToJSON AddApp where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON AddApp where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal GetApps where
  toVal GetApps
    { getAppsStart
    , getAppsSize
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("start", C.toVal getAppsStart)
    , ("size", C.toVal getAppsSize)
    ]

instance C.FromVal GetApps where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> GetApps
      P.<$> C.getMember _m "start"
      P.<*> C.getMember _m "size"
    _ -> P.Nothing

instance R.ToJSON GetApps where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON GetApps where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Device where
  toVal = \case
    Device'Gcw0 -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Gcw0" P.Nothing

instance C.FromVal Device where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("Gcw0", P.Nothing) -> P.Just Device'Gcw0
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Device where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Device where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Author where
  toVal = \case
    Author'Name Author'Name'Members
      { author'NameName
      } -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Name" P.$ P.Just P.$ R.fromList
      [ ("name", C.toVal author'NameName)
      ]
    Author'User Author'User'Members
      { author'UserIdent
      , author'UserUser
      } -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "User" P.$ P.Just P.$ R.fromList
      [ ("ident", C.toVal author'UserIdent)
      , ("user", C.toVal author'UserUser)
      ]

instance C.FromVal Author where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("Name", P.Just _m') -> Author'Name P.<$> (Author'Name'Members
          P.<$> C.getMember _m' "name"
        )
      ("User", P.Just _m') -> Author'User P.<$> (Author'User'Members
          P.<$> C.getMember _m' "ident"
          P.<*> C.getMember _m' "user"
        )
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Author where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Author where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

api'spec :: R.Value
api'spec = v
  where P.Just v = R.decode "{\"fluid\":{\"major\":0,\"minor\":0},\"pull\":{\"protocol\":\"http\",\"name\":\"Api\",\"host\":\"127.0.0.1\",\"meta\":\"Unit\",\"path\":\"/api\",\"port\":8080,\"error\":\"Unit\"},\"schema\":{\"Hello\":{\"m\":[{\"target\":\"String\"}],\"o\":\"String\"},\"AddComment\":{\"m\":[{\"message\":\"String\"}],\"o\":\"Unit\"},\"Device\":[\"Gcw0\"],\"UserId\":\"String\",\"User\":{\"m\":[{\"name\":\"String\"},{\"github\":{\"n\":\"Option\",\"p\":\"Url\"}}]},\"Date\":{\"m\":[{\"year\":\"Int\"},{\"month\":\"Int\"},{\"day\":\"Int\"}]},\"Author\":[{\"tag\":\"Name\",\"m\":[{\"name\":\"String\"}]},{\"tag\":\"User\",\"m\":[{\"ident\":\"UserId\"},{\"user\":\"User\"}]}],\"Url\":\"String\",\"AppId\":\"String\",\"AppSpec\":{\"m\":[{\"name\":\"String\"},{\"subtitle\":\"String\"},{\"device\":\"Device\"},{\"info\":\"String\"},{\"authors\":{\"n\":\"List\",\"p\":\"Author\"}},{\"porters\":{\"n\":\"List\",\"p\":\"Author\"}},{\"page\":{\"n\":\"Option\",\"p\":\"Url\"}},{\"img\":\"Url\"},{\"link\":\"Url\"}]},\"App\":{\"m\":[{\"id\":\"AppId\"},{\"name\":\"String\"},{\"subtitle\":\"String\"},{\"device\":\"Device\"},{\"info\":\"String\"},{\"authors\":{\"n\":\"List\",\"p\":\"Author\"}},{\"porters\":{\"n\":\"List\",\"p\":\"Author\"}},{\"page\":{\"n\":\"Option\",\"p\":\"Url\"}},{\"released\":\"Date\"},{\"img\":\"Url\"},{\"link\":\"Url\"}]},\"AddApp\":{\"m\":[{\"spec\":\"AppSpec\"}],\"o\":\"AppId\"},\"GetApps\":{\"m\":[{\"start\":\"Int\"},{\"size\":\"Int\"}],\"o\":{\"n\":\"List\",\"p\":\"App\"}}},\"version\":{\"major\":0,\"minor\":0}}"

