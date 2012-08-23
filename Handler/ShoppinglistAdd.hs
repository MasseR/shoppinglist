module Handler.ShoppinglistAdd where

import Import
import Model.Household

newListForm = renderDivs $ areq textField "List name" Nothing

getShoppinglistAddR :: Handler RepHtml
getShoppinglistAddR = do
  (formWidget, enctype) <- generateFormPost newListForm
  defaultLayout $ do
    setTitle "Create a new list"
    $(widgetFile "shoppinglistadd")


postShoppinglistAddR :: Handler RepHtml
postShoppinglistAddR = do
  ((form, _), _) <- runFormPost newListForm
  case form of
       FormSuccess name -> do
         (Entity user _) <- requireAuth
         household <- runDB $ findHousehold user
         case household of
              Just (Entity household' _) -> do
                listId <- runDB $ insert $ ShoppingList household' name
                redirect $ ShoppinglistHandlerR listId
              Nothing -> setMessage "You have no household. Create one first" >> redirect HouseholdSettingsR
       _ -> setMessage "Failed to create the list" >> redirect ShoppinglistAddR
