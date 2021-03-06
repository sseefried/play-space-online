{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation
    ( Foundation (..)
    , FoundationRoute (..)
    , resourcesFoundation
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , requireAuthId
    , requireAuthIdAndDeny
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Email
import Yesod.Helpers.Auth.Dummy
import Yesod.Helpers.Auth.HashDB hiding (UserId)
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import Model
import StaticFiles
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import qualified Data.Text as T
import Data.Text (Text)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Foundation = Foundation
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool  :: Settings.ConnectionPool -- ^ Database connection pool.
    , cacheDir  :: FilePath
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler Foundation Foundation

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget Foundation Foundation

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype FoundationRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Foundation = FoundationRoute
-- * Creates the value resourcesFoundation which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Foundation. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the FoundationRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Foundation" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Foundation where
    approot _ = Settings.approot

    defaultLayout widget = do
        mmsg <- getMessage
        mu <- maybeAuth
        pc <- widgetToPageContent $ do
            setTitle "PlaySpace Online"
            addCassius $(Settings.cassiusFile "default-layout")
            addLucius $(Settings.luciusFile "master")
            -- The order of the julius (javascript) inclusions matters

            let julii = [ $(Settings.juliusFile "glMatrix")
                        , $(Settings.juliusFile "enumerations")
                        , $(Settings.juliusFile "shady-ui")
                        , $(Settings.juliusFile "input-handler")
                        , $(Settings.juliusFile "frame-handler")
                        , $(Settings.juliusFile "webgl")
                        , $(Settings.juliusFile "canvas-resize")]
            mapM_ addJulius julii
            widget
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])

-- How to run database actions.
instance YesodPersist Foundation where
    type YesodDB Foundation = SqlPersist
    runDB db = liftIOHandler
             $ fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodAuth Foundation where
    type AuthId Foundation = UserId

    -- Where to send a user after successful login
    loginDest _ = ListEffectsR
    -- Where to send a user after logout
    logoutDest _ = ListEffectsR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    authPlugins = [ -- authOpenId
--                  , authEmail
                     authDummy
--                  , authHashDB
                  ]

instance YesodAuthEmail Foundation where
    type AuthEmailId Foundation = EmailId

    addUnverified email verkey =
        runDB $ insert $ Email email Nothing $ Just verkey
    sendVerifyEmail email _ verurl = liftIO $ renderSendMail Mail
        { mailHeaders =
            [ ("From", "noreply")
            , ("To", email)
            , ("Subject", "Verify your email address")
            ]
        , mailParts = [[textPart, htmlPart]]
        }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                          $ Data.Text.Lazy.unlines
                [ "Please confirm your email address by clicking on the link below."
                , ""
                , Data.Text.Lazy.fromChunks [verurl]
                , ""
                , "Thank you"
                ]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [$hamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey eid key = runDB $ update eid [EmailVerkey $ Just key]
    verifyAccount eid = runDB $ do
        me <- get eid
        case me of
            Nothing -> return Nothing
            Just e -> do
                let email = emailEmail e
                case emailUser e of
                    Just uid -> return $ Just uid
                    Nothing -> do
                        uid <- insert $ User email Nothing
                        update eid [EmailUser $ Just uid, EmailVerkey Nothing]
                        return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword $ Just pass]
    getEmailCreds email = runDB $ do
        me <- getBy $ UniqueEmail email
        case me of
            Nothing -> return Nothing
            Just (eid, e) -> return $ Just EmailCreds
                { emailCredsId = eid
                , emailCredsAuthId = emailUser e
                , emailCredsStatus = isJust $ emailUser e
                , emailCredsVerkey = emailVerkey e
                }
    getEmail = runDB . fmap (fmap emailEmail) . get

requireAuthIdAndDeny :: Handler UserId
requireAuthIdAndDeny = maybeAuthId >>= maybe (permissionDenied "You need to be logged in") return

isAboutR = False