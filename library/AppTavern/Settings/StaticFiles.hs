{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module AppTavern.Settings.StaticFiles where

import AppTavern.Settings (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFiles)

staticFiles (appStaticDir compileTimeAppSettings)
