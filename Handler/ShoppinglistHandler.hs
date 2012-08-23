module Handler.ShoppinglistHandler where

import Import
import Data.Maybe (catMaybes)
import qualified Data.Text as T

{- There is a possible race condition in this handler. We are not exporting the item ids to the outside world, but instead we are regenerating them here in the backend side. This means that if there are two requests close enough the state between the get and post might have changed. A proper fix would be to create a new kind of checkbox field (there is documentation on yesod site), but for now this is ok. (Besides at that point, sqlite isn't good enough :D) -}

checkBoxes :: [(ItemId, Text)] -> AForm App App [ItemId]
checkBoxes = fmap catMaybes . listForm'
  where
    listForm' = foldr (\x xs -> fmap (:) x <*> xs) (pure []) . map checkBox
    checkBox (itemId, str) = maybeStr <$> areq checkBoxField fsettings Nothing
      where maybeStr False = Nothing
            maybeStr True = Just itemId
            fsettings = FieldSettings (SomeMessage str) Nothing Nothing Nothing []

data ShoppingListForm = ShoppingListForm {
      new :: Maybe Text
    , items :: [ItemId]
  } deriving Show

shoppinglistForm :: [(ItemId, Text)] -> AForm App App ShoppingListForm
shoppinglistForm xs = ShoppingListForm <$> aopt textField "New item" Nothing <*> checkBoxes xs

getShoppinglistHandlerR :: ShoppingListId -> Handler RepHtml
getShoppinglistHandlerR shoppingListId = do
  list <- runDB $ get404 shoppingListId
  listItemIds <- map (shoppingListItemItem . entityVal) <$> (runDB $ selectList [ShoppingListItemList ==. shoppingListId, ShoppingListItemCount >. 0] [])
  itemNames <- map (itemName . entityVal) <$> (runDB $ selectList [ItemId <-. listItemIds] [])
  ((_, widget), enctype) <- runFormPost $ renderDivs $ shoppinglistForm $ zip listItemIds itemNames
  defaultLayout $ do
    setTitle $ toHtml $ shoppingListName list
    $(widgetFile "shoppinglisthandler")

postShoppinglistHandlerR :: ShoppingListId -> Handler RepHtml
postShoppinglistHandlerR shoppingListId = do
  _ <- runDB $ get404 shoppingListId
  listItemIds <- map (shoppingListItemItem . entityVal) <$> (runDB $ selectList [ShoppingListItemList ==. shoppingListId, ShoppingListItemCount >. 0] [])
  itemNames <- map (itemName . entityVal) <$> (runDB $ selectList [ItemId <-. listItemIds] [])
  ((form, _), _) <- runFormPost $ renderDivs $ shoppinglistForm $ zip listItemIds itemNames
  case form of
       FormSuccess f -> do
         $(logInfo) $ T.pack $ show f
         -- Add a new item
         case (new f) of
              Just newItem -> do
                maybeItem <- runDB $ getBy $ UniqueName newItem
                itemId <- case maybeItem of
                     Just (Entity key _) -> return key
                     Nothing -> runDB $ insert $ Item newItem Nothing
                maybeListItem <- runDB $ getBy $ UniqueItem shoppingListId itemId
                case maybeListItem of
                     Just (Entity key _) -> runDB $ update key [ShoppingListItemCount +=. 1]
                     Nothing -> (runDB $ insert $ ShoppingListItem shoppingListId itemId 1) >> return ()
              Nothing -> return ()
         -- Remove items
         let itemIds = items f
         runDB $ updateWhere [ShoppingListItemList ==. shoppingListId,
          ShoppingListItemItem <-. itemIds,
          ShoppingListItemCount >. 0] [ShoppingListItemCount -=. 1]
       _ -> return ()
  redirect $ ShoppinglistHandlerR shoppingListId
