        {-# LANGUAGE OverloadedStrings #-}
module Handler.Compile where

-- standard libraries
import System.Plugins as Plugins
import System.Plugins.Utils (mkTemp)
import System.Directory
import System.FilePath
import System.IO
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C
import Yesod.Helpers.Static (base64md5)

import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)

import Shady.CompileEffect
import qualified Shady.CompileEffect as S

-- friends
import Foundation
import Handler.UnsafeText

-- | Compile the effect code. Return either the @Effect@ or a compiler error (Left).
--
compileEffect :: Key Effect -> (Effect,User) -> Handler (Either String Effect)
compileEffect key (effect,user) = do
  path        <- writeEffectFileToTemp uniquePrefix $ effectCode effect
  (res, name) <- makeEffect path

  case res of
     MakeSuccess _  objectFile -> loadAndUpdateEffect objectFile name
     MakeFailure errors        -> return (Left $ formatErrors errors)
  where
    formatErrors errors = concat $ intersperse "\n" errors
    uniquePrefix = effectUnique effect user
    --
    -- Load the plugin, get the code, update the effect, unload the effect object.
    --
    loadAndUpdateEffect :: String -> String -> Handler (Either String Effect)
    loadAndUpdateEffect objectFile name = do
      mbStatus <- liftIO $ Plugins.load objectFile [] [] name
      case mbStatus of
        LoadSuccess modul effect -> do
          effect' <- updateAndReturnEffect effect
          liftIO $ Plugins.unload modul
          return (Right effect')
        LoadFailure errors -> return (Left $ formatErrors errors)

    --
    -- Given a GLSL object, updates the effect in the database and returns it.
    --
    updateAndReturnEffect :: WebGLEffect -> Handler Effect
    updateAndReturnEffect e = do
      let newEffect =
            effect { effectCompiles       = True
                   , effectFragShaderCode = Just . T.pack $ fragmentShader e
                   , effectVertShaderCode = Just . T.pack $ vertexShader e
                   , effectUiJson         = Just . T.pack $ jsonUI e }
      runDB $ replace key newEffect
      return newEffect

--
-- Given the path to a temporary file which contains the effect code,
-- compiles it and returns @(makeStatus, name)@ where
-- @name@ is the unique name of the effect.
--
-- The @name@ is as unique as the temporary file's name (which is assumed
-- to be globally unique to the file system).
--
makeEffect :: String -> Handler (MakeStatus, String)
makeEffect path = do
  let (modulExt, modulName, name) = getNames path
  res <- liftIO $ Plugins.make path [ "-DMODULE_NAME=" ++ modulName
                                    , "-DEFFECT_NAME=" ++ name ]

  return (res, name)
  where
    getNames :: String -> (String, String, String)
    getNames path =
      let modulExt = takeBaseName path
          modulName = modulExt
          name = map toLower modulExt
      in (modulExt, modulName, name)

--
-- Takes some effect code, wraps it in some boilerplate, and writes it out to a handle.
--
writeEffectFileToTemp :: Text -> Text -> Handler FilePath
writeEffectFileToTemp uniquePrefix text = do
  (path, h) <- liftIO mkTemp
  liftIO $ ((hPutStrLn h) . T.unpack . effectCodeWrapper uniquePrefix $ text) >> hClose h
  return path

--
-- This Haskell code needs to be run through CPP to be valid.
--
effectCodeWrapper :: Text -> Text -> Text
effectCodeWrapper uniquePrefix codeStr = T.unlines [
    "{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators #-}"
  , "module MODULE_NAME where"
  , ""
  , "import Data.Boolean"
  , "import Shady.Lighting"
  , "import Data.Derivative"
  , "import Data.VectorSpace"
  , "import Shady.ParamSurf"
  , "import Shady.CompileImage"
  , "import Shady.CompileSurface"
  , "import Shady.Color"
  , "import Shady.Language.Exp"
  , "import Shady.Complex"
  , "import Shady.Misc"
  , "import Shady.CompileE"
  , "import Shady.Image"
  , "import qualified Shady.Vec as V"
  , "import Shady.CompileEffect"
  , "import Control.Applicative hiding ((<*))"
  , ""
  , ""
  , "EFFECT_NAME :: WebGLEffect"
  , "EFFECT_NAME = compileEffect \"" `T.append` uniquePrefix `T.append` "\" effect"
  , "{-# LINE 1 \"Code.hs\" #-}" ] `T.append` codeStr

defaultFragShaderCode, defaultVertShaderCode :: Text -> UnsafeText
(defaultFragShaderCode, defaultVertShaderCode) =
  (\p -> fromString . fragmentShader $ effectForPrefix p,
   \p -> fromString . vertexShader $ effectForPrefix p)
  where effectForPrefix p = S.compileEffect (T.unpack p) . shadyEffect . return $ shadyGeometry

-- Utilities


underscoreize :: Text -> Text
underscoreize = T.toLower . (T.replace " " "_")

--
-- FIXME: This needs to be a valid javascript and GLSL identifier. It is not necessarily at the
--        moment.
--
effectUnique :: Effect -> User -> Text
effectUnique effect user = underscoreize $ effectName effect `T.append`
                           "_" `T.append` userIdent user