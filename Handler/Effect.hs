{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Handler.Effect where

-- libraries
import Prelude hiding (length)
import Data.Text hiding (map)
import qualified Data.Text as Text
import Text.Printf
import Control.Applicative

-- friends
import Foundation


getListEffectsR :: Handler RepHtmlJson
getListEffectsR = do
  -- TODO: For now just return all effects. Pagination to come.
  compilesParam <- lookupGetParam "compiles"
  let effectFilter = maybe [] (\val -> if val == "yes"
                                       then [EffectCompilesEq True] else []) compilesParam
  results <- runDB $ selectList effectFilter [EffectNameAsc] 1000 0
  let effects = map snd results
  (_, form, encType, csrfHtml) <- runFormPost $ createFormlet Nothing
  let newForm = $(widgetFile "effects/new")
      canCancel = False
      info = information ""
  let json = jsonList (map (jsonScalar . unpack . effectName) effects)
  defaultLayoutJson (addWidget $(widgetFile "effects/list")) json

-- A simple form for creating a new effect.
createFormlet :: Maybe Text ->Form s m Text
createFormlet s = fieldsToDivs $ stringField "Add new effect" s

information :: Text -> Html
information infoStr =
  if length infoStr > 0 then [$hamlet| div.info $str$|] else ""

postCreateEffectR :: Handler RepHtml
postCreateEffectR = createEffect

putCreateEffectR :: Handler RepHtml
putCreateEffectR = createEffect

createEffect :: Handler RepHtml
createEffect = do
  maUserId <- maybeUserId
  case maUserId of
    Nothing -> error "not authorized"
    Just userId -> do
      (res, form, encType, csrfHtml) <- runFormPost $ createFormlet Nothing
      result <- case res of
        FormMissing   -> return (Left "Name is blank" :: Either Text Text)
        FormFailure _ -> return (Left "There were some problems with the form")
        FormSuccess name -> return (Right name)
      let canCancel = True
      case result of
        Left infoStr -> do
          let info = information infoStr
          defaultLayout $ addWidget $(widgetFile "effects/new")
        Right name -> do
          mbGet <- runDB $ getBy (UniqueEffect userId name)
          case mbGet of
            Nothing -> do
              -- FIXME: Will only succeed if someone else hasn't inserted a record in the mean time.
              -- Very unlikely but still possible.
              effectKey <- runDB $ insert (Effect name userId defaultEffectCode True)
              -- FIXME: Very small chance it's been deleted in mean time
              (Just effect) <- runDB $ get effectKey
              showEffect effect
            Just _ -> do
              let info = information $ pack $ printf "An effect with name '%s' already exists!"
                            (unpack name)
              defaultLayout $ addWidget $(widgetFile "effects/new")

getShowEffectR :: UserId -> String -> Handler RepHtml
getShowEffectR = error "not implemented"

showEffect :: Effect -> Handler RepHtml
showEffect effect = defaultLayout $ addWidget $(widgetFile "effects/show")

defaultEffectCode :: Text
defaultEffectCode = "this code is broken"

getEditEffectR :: String -> Handler RepHtml
getEditEffectR name = do
  mbUserId <- maybeUserId
  case mbUserId of
    Just userId -> do
      mbResult <- runDB (getBy $ UniqueEffect userId (pack name))
      case mbResult of
        Just (key, effect) -> do
          -- this is the first time we show the form so we don't care about the result type.
          (_, form, encType, csrfHtml) <- runFormPost $ editFormlet effect
          defaultLayout $ do
            addWidget $(widgetFile "effects/edit")
--            addWidget $(widgetFile "effects/preview")
        Nothing            -> effectNotFound name
    Nothing -> error "FIXME"

postUpdateEffectR :: String -> Handler RepHtml
postUpdateEffectR = error "not implemented"

putUpdateEffectR :: String -> Handler RepHtml
putUpdateEffectR = error "not implemented"


deleteDeleteEffectR :: String -> Handler RepHtml
deleteDeleteEffectR = deleteEffect

postDeleteEffectR :: String -> Handler RepHtml
postDeleteEffectR = deleteEffect

deleteEffect :: String -> Handler RepHtml
deleteEffect name = do
  mbUserId <- maybeUserId
  case mbUserId of
    Just userId -> do
      runDB $ deleteBy $ UniqueEffect userId (pack name)
      redirect RedirectSeeOther ListEffectsR
    Nothing -> error "fixme"

getRunEffectR :: String -> Handler RepHtml
getRunEffectR = error "not implemented"

postResultEffectR :: String -> Handler RepHtml
postResultEffectR = error "not implemented"


-------


effectNotFound :: String -> Handler RepHtml
effectNotFound name = defaultLayout $ addWidget $(widgetFile "effects/not-found")

editFormlet :: Effect -> Form s m EditParams
editFormlet effect = do
  -- The unTextArea turns the Textarea back into a String because Effect constructor requires
  -- second argument with that type.
  fieldsToDivs $ EditParams (effectName effect) <$>
     textareaField "Code" (Just $ Textarea $ effectCode effect)

-- A very simple form. The only field you can edit is the code field.
-- TODO: There appears to be no way in Yesod, so far, to set attributes on form fields.
-- e.g. I want to size the textarea in the form below but can't yet.
data EditParams = EditParams { editParamsName :: Text
                             , editParamsCode :: Textarea }
