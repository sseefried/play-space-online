{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withFoundation
    , withDevelApp
    ) where

import Foundation
import Settings
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)
import System.Directory ( getAppUserDataDirectory, doesDirectoryExist, createDirectory
                        , removeDirectoryRecursive)
import Control.Monad (when)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Effect
import Handler.About

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Foundation" resourcesFoundation

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withFoundation :: (Application -> IO a) -> IO a
withFoundation f = Settings.withConnectionPool $ \p -> do
    runConnectionPool (runMigration migrateAll) p
    cacheDir <- createCache
    let h = Foundation { getStatic = s
                       , connPool = p
                       , cacheDir = cacheDir }
    toWaiApp h >>= f
  where
    s = static Settings.staticdir
    createCache = do
      cache <- getAppUserDataDirectory "play-space-cache"
      exists <- doesDirectoryExist cache
      when exists (removeDirectoryRecursive cache)
      createDirectory cache
      return cache

withDevelApp :: Dynamic
withDevelApp = toDyn (withFoundation :: (Application -> IO ()) -> IO ())
