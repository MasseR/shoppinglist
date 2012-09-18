{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Quasi
import Database.Persist.Store (PersistValue(..), SqlType(..))

data Unit = Imperial | Metric deriving Show

instance PersistField Unit where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText (T.stripPrefix "Imperial" -> Just _)) = Right Imperial
  fromPersistValue (PersistText (T.stripPrefix "Metric" -> Just _)) = Right Metric
  fromPersistValue _ = Left "Is not a unit"
  sqlType _ = SqlString

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
