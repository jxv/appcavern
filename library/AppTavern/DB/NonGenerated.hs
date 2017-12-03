
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module AppTavern.DB.NonGenerated where

import Protolude
import Data.String (String)
import Data.List (lookup)
import Data.Text.Conversions
import Database.Persist.Sql (PersistField(..), PersistFieldSql(..), PersistValue)

fromPersistValueTagDefault :: (Bounded b, Enum b, ToText b) => PersistValue -> Either Text b
fromPersistValueTagDefault v = case fromPersistValue v of
  Left e -> Left e
  Right s -> case lookupTag (s :: String) of
    Nothing -> Left "Not a value"
    Just u -> Right u

lookupTag :: (ToText b, ToText a, Enum b, Bounded b) => a -> Maybe b
lookupTag tag = lookup (toText tag) [(toText v, v) | v <- [minBound..maxBound]]

data Device
  = Device'Gcw0
  deriving (Show, Eq, Enum, Bounded)

instance ToText Device where
  toText = \case
    Device'Gcw0 -> "Gcw0"

instance PersistField Device where
  toPersistValue x = toPersistValue (fromText (toText x) :: String)
  fromPersistValue = fromPersistValueTagDefault

instance PersistFieldSql Device where
  sqlType _ = sqlType (Proxy :: Proxy String)
