{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Handler.Effect where

-- libraries
import Prelude hiding (length)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import Control.Applicative
import Data.Maybe

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
  users <- getUsers effects
  let effectsAndUsers = zip effects users
  (_, form, encType, csrfHtml) <- runFormPost $ createFormlet Nothing
  let newForm = $(widgetFile "effects/new")
      canCancel = False
      info = information ""
  let json = jsonList (map (jsonScalar . T.unpack . effectName) effects)
  defaultLayoutJson (addWidget $(widgetFile "effects/list")) json
  where
    getUsers :: [Effect] -> Handler [User]
    getUsers effects = do
       mbUsers <- mapM getUser effects
       return $ catMaybes mbUsers
    getUser :: Effect -> Handler (Maybe User)
    getUser effect = runDB $ get (effectUser effect)



-- A simple form for creating a new effect.
createFormlet :: Maybe Text ->Form s m Text
createFormlet s = fieldsToDivs $ stringField "Add new effect" s

information :: Text -> Html
information infoStr =
  if T.length infoStr > 0 then [$hamlet| <div.info> #{infoStr}|] else ""

postCreateEffectR :: Handler RepHtml
postCreateEffectR = createEffect

putCreateEffectR :: Handler RepHtml
putCreateEffectR = createEffect

createEffect :: Handler RepHtml
createEffect = do
  userId <- requireAuthIdAndDeny
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
          let info = information $ T.pack $ printf "An effect with name '%s' already exists!"
                        (T.unpack name)
          defaultLayout $ addWidget $(widgetFile "effects/new")

getShowEffectR :: UserId -> Text -> Handler RepHtml
getShowEffectR userId name = do
  -- get the effect from the database.
  mbResult <- runDB $ do { getBy $ UniqueEffect userId name }
  case mbResult of
    Just (_,effect) -> showEffect effect
    Nothing         -> effectNotFound name

showEffect :: Effect -> Handler RepHtml
showEffect effect = defaultLayout $ addWidget $(widgetFile "effects/show")

defaultEffectCode :: Text
defaultEffectCode = "this code is broken"

getEditEffectR :: Text -> Handler RepHtml
getEditEffectR name = do
  userId <- requireAuthIdAndDeny
  mbResult <- runDB (getBy $ UniqueEffect userId name)
  case mbResult of
    Just (key, effect) -> do
      -- this is the first time we show the form so we don't care about the result type.
      (_, form, encType, csrfHtml) <- runFormPost $ editFormlet effect
      defaultLayout $ do
        addWidget $(widgetFile "effects/edit")
--          addWidget $(widgetFile "effects/preview")
    Nothing            -> effectNotFound name

postUpdateEffectR :: Text -> Handler RepHtml
postUpdateEffectR = updateEffect

putUpdateEffectR :: Text -> Handler RepHtml
putUpdateEffectR = updateEffect

updateEffect :: Text -> Handler RepHtml
updateEffect name = do
  userId <- requireAuthIdAndDeny
  mbResult <- runDB $ getBy (UniqueEffect userId name)
  case mbResult of
     Just (key, effect') -> do
       (res, form, encType, csrfHtml) <- runFormPost $ editFormlet effect'
       eResult <- case res of
         FormMissing   -> return (Left "Form is blank" :: Either Text EditParams)
         FormFailure _ -> return (Left $ "There were some problems with the form")
         FormSuccess params -> return (Right params)
       case eResult of
         Left info -> do
           defaultLayout $ do
             let effect = effect'
             addWidget $(widgetFile "effects/edit")
             addHtml $ information info
         Right params -> do
           let effect = Effect (editParamsName params) userId
                               (unTextarea $ editParamsCode params) True
           runDB $ replace key effect
           compileRes <- compileEffect effect
           case compileRes of
             (Left err) -> do
               runDB $ replace key (effect {effectCompiles = False})
               defaultLayout $ do
                 addWidget $(widgetFile "effects/edit")
                 addHtml $ information err
--                 addWidget $(widgetFile "effects/preview")
             (Right _) -> do
               runDB $ replace key (effect {effectCompiles = True})
               defaultLayout $ do
                 addWidget $(widgetFile "effects/edit")
--                 addWidget $(widgetFile "effects/preview")
     Nothing -> error "die die die"-- FIXME: Need to handle this gracefully.


deleteDeleteEffectR :: Text -> Handler RepHtml
deleteDeleteEffectR = deleteEffect

postDeleteEffectR :: Text -> Handler RepHtml
postDeleteEffectR = deleteEffect

deleteEffect :: Text -> Handler RepHtml
deleteEffect name = do
  userId <- requireAuthIdAndDeny
  runDB $ deleteBy $ UniqueEffect userId name
  redirect RedirectSeeOther ListEffectsR



-------


effectNotFound :: Text -> Handler RepHtml
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


-- | Compile the effect code. Return either the path to the compiled binary (Right) or
--   the compiler error (Left).
--
compileEffect :: Effect -> Handler (Either Text (Text, FilePath))
compileEffect _ = return $ Left "Compilation of effects is not yet implemented"