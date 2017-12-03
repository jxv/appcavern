{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AppTavern.App
  ( addApp
  , getApps
  , countApps
  ) where

import Protolude
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text.Conversions
import System.Random
import qualified Data.List as List

import qualified AppTavern.DB as DB
import AppTavern.Import (Handler, runDB, insert_, selectList, count, Entity(..), Filter, SelectOpt(..))
import AppTavern.Api.V0

-- toUTCTime :: Date -> UTCTime
-- toUTCTime Date{dateYear,dateMonth,dateDay} = UTCTime (fromGregorian (fromIntegral dateYear) dateMonth dateDay) 0

toDate :: UTCTime -> Date
toDate utc = let (year, month, day) = toGregorian (utctDay utc) in Date { dateYear = fromIntegral year, dateMonth = month, dateDay = day }

char64 :: [Char]
char64 = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

choice :: [a] -> IO a
choice xs = do
  x <- randomIO
  let idx = x `mod` length xs
  return $ xs List.!! idx

generateText64 :: Int -> IO Text
generateText64 n = fmap toText $ sequence $ replicate n $ choice char64

genAppId :: IO AppId
genAppId = AppId <$> generateText64 16

addApp :: () -> AddApp -> Handler AppId
addApp () AddApp{addAppSpec=spec} = do
  appId <- liftIO genAppId
  now <- liftIO getCurrentTime
  runDB $ insert_ $ DB.App
    { DB.appIdent = toText appId
    , DB.appName = appSpecName spec
    , DB.appSubtitle = appSpecSubtitle spec
    , DB.appInfo = appSpecInfo spec
    , DB.appDevice = dbDevice (appSpecDevice spec)
    , DB.appAuthors = (\(AuthorSpec'Name (AuthorSpec'Name'Members name)) -> name) <$> appSpecAuthors spec
    , DB.appPorters = (\(AuthorSpec'Name (AuthorSpec'Name'Members name)) -> name) <$> appSpecPorters spec
    , DB.appPage = toText <$> appSpecPage spec
    , DB.appCreated = now
    , DB.appReleased = now
    , DB.appImg = toText $ appSpecImg spec
    , DB.appLink = toText $ appSpecLink spec
    }
  return appId

dbDevice :: Device -> DB.Device
dbDevice Device'Gcw0 = DB.Device'Gcw0

apiDevice :: DB.Device -> Device
apiDevice DB.Device'Gcw0 = Device'Gcw0

getApps :: () -> GetApps -> Handler [App]
getApps () GetApps{getAppsStart=start, getAppsSize=size} = do
  apps <-  runDB $ selectList [] [Asc DB.AppName, OffsetBy start, LimitTo size]
  return $ flip map (map entityVal apps) $ \app -> App
    { appId = AppId $ DB.appIdent app
    , appName = DB.appName app
    , appSubtitle = DB.appSubtitle app
    , appDevice = apiDevice $ DB.appDevice app
    , appInfo = DB.appInfo app
    , appAuthors = map (Author'Name . Author'Name'Members) (DB.appAuthors app)
    , appPorters = map (Author'Name . Author'Name'Members) (DB.appPorters app)
    , appPage = fmap Url $ DB.appPage app
    , appImg = Url $ DB.appImg app
    , appLink = Url $ DB.appLink app
    , appReleased = toDate (DB.appReleased app)
    }

countApps :: () -> Handler Int
countApps () = runDB $ count ([] :: [Filter DB.App])
