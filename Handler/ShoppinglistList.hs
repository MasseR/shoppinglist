module Handler.ShoppinglistList where

import Import
import Model.Household

getShoppinglistListR :: Handler RepHtml
getShoppinglistListR = do
  user <- requireAuth
  household <- runDB $ findHousehold $ entityKey user
  case household of
       Nothing -> setMessage "You don't have household set, create one and then return back" >> redirect HouseholdSettingsR
       Just household' -> do
         lists <- runDB (selectList [ShoppingListHousehold ==. entityKey household'] [])
         defaultLayout $ do
           setTitle "Your shopping lists"
           $(widgetFile "shoppinglistlist")
