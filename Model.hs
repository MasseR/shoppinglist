{-# Language OverloadedStrings #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Quasi
import Database.Persist.Store (PersistValue(..), SqlType(..))

data Unit = Imperial | Metric deriving (Show, Read)
data Course = Appetizer | Main | Dessert deriving (Show, Read)

instance PersistField Unit where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText value) =
    case reads (T.unpack value) of
         [(unit, _)] -> Right unit
         _ -> Left $ "Could not parse as unit: " `T.append` value
  fromPersistValue _ = Left "Invalid type"
  sqlType _ = SqlString

instance PersistField Course where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText value) =
    case reads (T.unpack value) of
         [(course, _)] -> Right course
         _ -> Left $ "Could not parse as course: " `T.append` value
  fromPersistValue _ = Left "Invalid type"
  sqlType _ = SqlString

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
