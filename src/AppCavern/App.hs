{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AppCavern.App
  ( addApp
  , getApps
  ) where

import Protolude
import Import (Handler, runDB, insert_, selectList, Entity(..), SelectOpt(..))
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text.Conversions
import System.Random
import qualified Data.List as List

import qualified Model as DB

import AppCavern.Api.V0

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
  runDB $ insert_ $ DB.A
    { DB.aIdent = toText appId
    , DB.aName = appSpecName spec
    , DB.aSubtitle = appSpecSubtitle spec
    , DB.aInfo = appSpecInfo spec
    , DB.aDevice = toText (appSpecDevice spec)
    , DB.aAuthors = (\(Author'Name (Author'Name'Members name)) -> name) <$> appSpecAuthors spec
    , DB.aPorters = (\(Author'Name (Author'Name'Members name)) -> name) <$> appSpecPorters spec
    , DB.aPage = toText <$> appSpecPage spec
    , DB.aCreated = now
    , DB.aReleased = now
    , DB.aImg = toText $ appSpecImg spec
    , DB.aLink = toText $ appSpecLink spec
    }
  return appId

instance ToText Device where
  toText Device'Gcw0 = "Gcw0"

deviceText :: Text -> Device
deviceText "Gcw0" = Device'Gcw0

getApps :: () -> GetApps -> Handler [App]
getApps () GetApps{getAppsStart=start, getAppsSize=size} = do
  apps <-  runDB $ selectList [] [Desc DB.AReleased, OffsetBy start, LimitTo size]
  return $ flip map (map entityVal apps) $ \app -> App
    { appId = AppId $ DB.aIdent app
    , appName = DB.aName app
    , appSubtitle = DB.aSubtitle app
    , appDevice = deviceText $ DB.aDevice app
    , appInfo = DB.aInfo app
    , appAuthors = map (Author'Name . Author'Name'Members) (DB.aAuthors app)
    , appPorters = map (Author'Name . Author'Name'Members) (DB.aPorters app)
    , appPage = fmap Url $ DB.aPage app
    , appImg = Url $ DB.aImg app
    , appLink = Url $ DB.aLink app
    , appReleased = toDate (DB.aReleased app)
    }
