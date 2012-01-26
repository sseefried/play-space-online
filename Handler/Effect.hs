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
import Handler.Compile
import Handler.UnsafeText
import Foundation
import Shady.CompileEffect.Json

getListEffectsR :: Handler RepHtmlJson
getListEffectsR = do
  -- TODO: For now just return all effects. Pagination to come.
  effectsAndUsers <- readEffectsAndUsers
  (_, form, encType, csrfHtml) <- runFormPost $ createFormlet Nothing
  let newForm = $(widgetFile "effects/new")
      canCancel = False
      info = information ""
      (width,height) = (150, 150)
  let json = jsonList []
  defaultLayoutJson (do
    addWidget $(widgetFile "effects/list")
    startWebGLScript
    ) json -- FIXME: Find default json

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
          effectKey <- runDB $ insert (Effect name userId defaultEffectCode
                                         Nothing Nothing Nothing True)
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
showEffect effect = do
  eu <- readEffectAndUser effect
  defaultLayout $ do
     addWidget $(widgetFile "effects/show")
     startWebGLScript

defaultEffectCode :: Text
defaultEffectCode = "this code is broken"

getEditEffectR :: Text -> Handler RepHtml
getEditEffectR name = do
  userId <- requireAuthIdAndDeny
  mbResult <- runDB (getBy $ UniqueEffect userId name)
  case mbResult of
    Just (key, effect) -> do
      eu <- readEffectAndUser effect
      -- this is the first time we show the form so we don't care about the result type.
      (_, form, encType, csrfHtml) <- runFormPost $ editFormlet effect
      defaultLayout $ do
        addWidget $(widgetFile "effects/edit")
        startWebGLScript
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
     Just (key, effect) -> do
       (res, form, encType, csrfHtml) <- runFormPost $ editFormlet effect
       eResult <- case res of
         FormMissing   -> return (Left "Form is blank" :: Either Text EditParams)
         FormFailure _ -> return (Left $ "There were some problems with the form")
         FormSuccess params -> return (Right params)
       case eResult of
         Left info -> do
           eu <- readEffectAndUser effect
           defaultLayout $ do
             addWidget $(widgetFile "effects/edit")
             addHtml $ information info
         Right params -> do
           let effect = Effect (editParamsName params) userId
                               (unTextarea $ editParamsCode params)
                               Nothing Nothing Nothing True
           runDB $ replace key effect
           eu <- readEffectAndUser effect
           compileRes <- compileEffect key eu
           case compileRes of
             Left err -> do
               runDB $ replace key (effect {effectCompiles = False})
               eu <- readEffectAndUser effect -- must re-read effect
               defaultLayout $ do
                 addWidget $(widgetFile "effects/edit")
                 addHtml . information $ T.pack err
             Right effect -> do
               eu <- readEffectAndUser effect -- must re-read effect
               defaultLayout $ do
                 addWidget $(widgetFile "effects/edit")
                 startWebGLScript
     Nothing -> defaultLayout $ addWidget $(widgetFile "effects/not_found")


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


--
-- Helpers
--
readEffectsAndUsers :: Handler [(Effect, User)]
readEffectsAndUsers = do
  compilesParam <- lookupGetParam "compiles"
  let effectFilter = maybe [] (\val -> if val == "yes"
                                       then [EffectCompilesEq True] else []) compilesParam
  results <- runDB $ selectList effectFilter [EffectNameAsc] 1000 0
  let effects = map snd results
  users <- getUsers effects
  return $ zip effects users
  where
    getUsers :: [Effect] -> Handler [User]
    getUsers effects = do
       mbUsers <- mapM getUser effects
       return $ catMaybes mbUsers
    getUser :: Effect -> Handler (Maybe User)
    getUser effect = runDB $ get (effectUser effect)

effectThumbnail :: (Effect,User) -> Int -> Bool ->  Widget ()
effectThumbnail (effect,user) size showTheUI = $(widgetFile "effects/thumbnail")
  where
    showUI :: Text
    showUI = if showTheUI then "true" else "false"
    uniquePrefix = effectUnique effect user

startWebGLScript :: Widget ()
startWebGLScript = addJulius [$julius| $(function() {
  /* Auto focus on first canvas */
  $('canvas')[0].focus();
  WebGL.start(); }); |]
--  where
--    uiJsonUnsafeText = UnsafeText . T.pack . show $ uiJson


readEffectAndUser :: Effect -> Handler (Effect,User)
readEffectAndUser effect = do
  mbUser <- runDB $ get (effectUser effect)
  let user = fromMaybe (error "should not happen") mbUser
  return (effect,user)
