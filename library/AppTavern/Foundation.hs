{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module AppTavern.Foundation where

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

import Yesod.Auth.Dummy

import Yesod.Auth.OpenId (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import AppTavern.Import.NoFoundation
import AppTavern.DB (UserId, User(..), Unique(..))

data App = App
  { appSettings :: AppSettings
  , appStatic :: Static
  , appConnPool :: ConnectionPool
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

mkYesodData "App" $(parseRoutesFile "routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

type DB a = forall (m :: * -> *).
  (MonadIO m, Functor m) => ReaderT SqlBackend m a

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root
  makeSessionBackend _ = Just <$> defaultClientSessionBackend 120 "client_session_key.aes"
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    _muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute
    (title, parents) <- breadcrumbs
    let menuItems =
          [ NavbarLeft $ MenuItem
            { menuItemLabel = "Home"
            , menuItemRoute = HomeR
            , menuItemAccessCallback = True
            }
          ]
    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  authRoute _ = Nothing
  isAuthorized HomeR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized ApiR _ = return Authorized
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
  shouldLog app _source level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError
  makeLogger = return . appLogger

instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb  _ = return ("home", Nothing)

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  redirectToReferer _ = True
  authenticate creds = runDB $ do
    x <- getBy $ UniqueUser $ credsIdent creds
    case x of
      Just (Entity uid _) -> return $ Authenticated uid
      Nothing -> Authenticated <$> insert User
        { userPublic = credsIdent creds
        , userPassword = Nothing
        }
  authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
    where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

  authHttpManager = getHttpManager

isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
