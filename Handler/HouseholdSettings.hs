module Handler.HouseholdSettings where

import Import
import Data.Maybe (listToMaybe)
import Model.Household

data HouseholdForm = HouseholdForm Text

getHouseholdSettingsR :: Handler RepHtml
getHouseholdSettingsR = do
  user <- requireAuth
  household <- runDB $ findHousehold $ entityKey user
  form <- case household of
       Nothing -> return $ householdForm Nothing
       Just household' -> do
         return $ householdForm ((Just . householdName . entityVal) household')
  (formWidget, enctype) <- generateFormPost form
  defaultLayout $ do
    setTitle "Household settings"
    $(widgetFile "householdsettings")

householdForm name = renderDivs $ HouseholdForm <$> areq textField "Name for the household" name

postHouseholdSettingsR :: Handler RepHtml
postHouseholdSettingsR = do
  user <- requireAuth
  household <- runDB $ findHousehold $ entityKey user
  ((result, _), _) <- runFormPost $ householdForm Nothing
  case result of
       FormSuccess (HouseholdForm name) ->
         case household of
              Just household' -> do
                setMessage "Household updated"
                _ <- runDB $ update (entityKey household') [HouseholdName =. name]
                return ()
              Nothing -> do
                setMessage "Household created"
                householdId <- runDB $ insert $ Household (entityKey user) name
                _ <- runDB $ insert $ HouseholdMembers householdId (entityKey user)
                return ()
       _ -> setMessage "Error in form"
  redirect HouseholdSettingsR

