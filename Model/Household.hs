module Model.Household where

import Import
import Data.Maybe (listToMaybe)

findHousehold owner = listToMaybe <$> selectList [HouseholdOwner ==. owner] [LimitTo 1]
