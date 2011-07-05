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
  let effectThumbnail (effect,user) = $(widgetFile "effects/thumbnail")
  let json = jsonList (map (jsonScalar . T.unpack . effectName) effects)
  defaultLayoutJson (do
    addWidget $(widgetFile "effects/list")
    addJulius [$julius| $(function() { webGLStart(); }); |]
    ) json
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
          effectKey <- runDB $ insert (Effect name userId defaultEffectCode Nothing Nothing True)
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
                               (unTextarea $ editParamsCode params)
                               (Just defaultFragShaderCode)
                               (Just defaultVertShaderCode) True
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

defaultFragShaderCode :: Text
defaultFragShaderCode = T.pack $ unlines [
    "#define gl_ModelViewProjectionMatrix ModelViewProjectionMatrix"
  , "#define gl_NormalMatrix NormalMatrix"
  , ""
  , "precision highp float;"
  , ""
  , "uniform mat4 gl_ModelViewProjectionMatrix;"
  , "uniform mat3 gl_NormalMatrix;"
  , ""
  , "#define _uniform time"
  , "#define _attribute uv_a"
  , "#define _varying_F uv_v"
  , "#define _varying_S pos_v"
  , ""
  , "uniform   float _uniform;"
  , "varying   vec2 _varying_F;"
  , "varying   vec3 _varying_SF;"
  ,"varying   vec3 _varying_SS;"
  ,"void main () {"
  ,"    vec2  x24 = vec2(0.5,0.5);"
  ,"    vec2  x42 = vec2(1.05,1.05);"
  ,"    float x52 = cos(_uniform);"
  ,"    float x54 = sin(_uniform);"
  ,"    vec2  x69 = vec2(1.0,1.0);"
  ,"    bvec2 x21 = lessThan(x24"
  ,"                        ,mod(1.0 / (sin(vec2(_uniform,_uniform)) + x42) *"
  ,"                             vec2(dot(vec2(x52,x54),_varying_F)"
  ,"                                 ,dot(vec2(x52,- x54),_varying_F.yx))"
  ,"                            ,x69));"
  ,"    bool  x16 = x21.x == x21.y;"
  ,"    vec3  x94 = vec3(-0.37139067,-0.0,-0.9284767);"
  ,"    float x88 = abs(dot(_varying_SS,x94));"
  ,"    vec3  x126 = vec3(0.37139067,0.0,0.9284767);"
  ,"    vec3  x142 = vec3(1.0,1.0,-2.5);"
  ,"    float x97 = exp(log(max(0.0"
  ,"                           ,dot(vec3(2.0 * dot(_varying_SS,x94)) * _varying_SS +"
  ,"                                x126"
  ,"                               ,vec3(1.0 / sqrt(dot(x142 - _varying_SF"
  ,"                                                   ,x142 - _varying_SF))) *"
  ,"                                (x142 - _varying_SF)))) * 15.0);"
  ,"    gl_FragColor = vec4(dot(vec2(x16 ? 0.0 : 1.0,0.5)"
  ,"                           ,vec2(0.2 + 0.4 * x88,x97))"
  ,"                       ,0.5 * x97"
  ,"                       ,dot(vec2(x16 ? 1.0 : 0.0,0.5),vec2(0.2 + 0.4 * x88,x97))"
  ,"                       ,1.0 + x88 + x97);"
  ,"}"]

defaultVertShaderCode :: Text
defaultVertShaderCode = T.pack $ unlines [
    "#define gl_ModelViewProjectionMatrix ModelViewProjectionMatrix"
  , "#define gl_NormalMatrix NormalMatrix"
  , ""
  , "precision highp float;"
  , ""
  , "uniform mat4 gl_ModelViewProjectionMatrix;"
  , "uniform mat3 gl_NormalMatrix;"
  , ""
  , "#define _uniform time"
  , "#define _attribute uv_a"
  , "#define _varying_F uv_v"
  , "#define _varying_S pos_v"
  , ""
  , "uniform   float _uniform;"
  , "attribute vec2 _attribute;"
  , "varying   vec2 _varying_F;"
  , "varying   vec3 _varying_SF;"
  , "varying   vec3 _varying_SS;"
  , "void main () {"
  , "    vec2  x15 = _attribute;"
  , "    float x27 = x15.y;"
  , "    vec2  x39 = vec2(2.0,9.0);"
  , "    float x34 = sin(dot(x39,vec2(_uniform,sqrt(dot(x15,x15)))));"
  , "    float x53 = 1.0 / exp(log(1.0 + sqrt(dot(x15,x15))) * 3.0);"
  , "    float x81 = 0.5 * _uniform;"
  , "    float x79 = cos(x81);"
  , "    float x85 = sin(x81);"
  , "    vec3  x10 = vec3(x15.x"
  , "                    ,dot(vec2(x27,- x34 * x53),vec2(x79,x85))"
  , "                    ,dot(vec2(x34 * x53,x27),vec2(x79,x85)));"
  , "    vec2  x172 = vec2(2.0,9.0);"
  , "    float x178 = dot(x15,x15);"
  , "    float x169 = dot(x172,vec2(_uniform,sqrt(x178)));"
  , "    float x167 = cos(x169);"
  , "    float x194 = 1.0 / (2.0 * sqrt(x178));"
  , "    float x205 = x15.x;"
  , "    float x207 = sin(x169);"
  , "    float x226 = dot(x15,x15);"
  , "    float x221 = 1.0 + sqrt(x226);"
  , "    float x216 = log(x221) * 3.0;"
  , "    float x214 = exp(x216);"
  , "    float x212 = 1.0 / x214;"
  , "    float x238 = 1.0 / (x214 * x214);"
  , "    float x248 = exp(x216);"
  , "    float x258 = 1.0 / x221;"
  , "    float x265 = 1.0 / (2.0 * sqrt(x226));"
  , "    float x156 = dot(vec2(x167 * (9.0 * (x194 * (x205 + x205))),x207)"
  , "                    ,vec2(x212"
  , "                         ,x238 * (x248 * (x258 * (x265 * (x205 + x205)) *"
  , "                                          3.0))));"
  , "    float x309 = x15.y;"
  , "    float x287 = dot(vec2(x167 * (9.0 * (x194 * (x309 + x309))),x207)"
  , "                    ,vec2(x212"
  , "                         ,x238 * (x248 * (x258 * (x265 * (x309 + x309)) *"
  , "                                          3.0))));"
  , "    float x343 = - x287 * x85;"
  , "    float x145 = dot(- (vec2(x156,x156)) * vec2(x85,x79)"
  , "                    ,vec2(x287 * x79,x79) + vec2(x85,x343));"
  , "    float x351 = - x287 * x79;"
  , "    float x356 = - x85;"
  , "    float x348 = x351 + x356;"
  , "    float x358 = x79 + x343;"
  , "    float x132 = 1.0 / sqrt(dot(vec3(vec2(x145,x348),x358)"
  , "                               ,vec3(vec2(x145,x348),x358)));"
  , "    vec3  x120 = gl_NormalMatrix * (vec3(x132,x132,x132) * vec3(x145"
  , "                                                               ,vec2(x351,x79) +"
  , "                                                                vec2(x356"
  , "                                                                    ,x343)));"
  , "    gl_Position = gl_ModelViewProjectionMatrix * vec4(x10,1.0);"
  , "    _varying_F = _attribute;"
  , "    _varying_SF = x10;"
  , "    _varying_SS = vec3(1.0 / sqrt(dot(x120,x120))) * x120;"
  , "}" ]

dasherize :: Text -> Text
dasherize = T.toLower . (T.replace " " "-")

effectUnique :: Effect -> User -> Text
effectUnique effect user = dasherize $ effectName effect `T.append` "-" `T.append` userIdent user