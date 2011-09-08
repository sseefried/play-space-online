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

import Shady.CompileE

-- friends
import Foundation
import Handler.UnsafeText

-- | Compile the effect code. Return either the @Effect@ or a compiler error (Left).
--
compileEffect :: Key Effect -> Effect -> Handler (Either String Effect)
compileEffect key effect = do
  path              <- writeEffectFileToTemp $ effectCode effect
  (res, name) <- makeEffect path
  case res of
     MakeSuccess _  objectFile -> loadAndUpdateEffect objectFile name
     MakeFailure errors        -> return (Left $ formatErrors errors)
  where
    formatErrors errors = concat $ intersperse "\n" errors

    --
    -- Load the plugin, get the code, update the effect, unload the effect object.
    --
    loadAndUpdateEffect :: String -> String -> Handler (Either String Effect)
    loadAndUpdateEffect objectFile name = do
      mbStatus <- liftIO $ Plugins.load objectFile [] [] name
      case mbStatus of
        LoadSuccess modul glsl -> do
          effect' <- updateAndReturnEffect glsl
          liftIO $ Plugins.unload modul
          return (Right effect')
        LoadFailure errors -> return (Left $ formatErrors errors)

    --
    -- Given a GLSL object updates the effect in the database and returns it.
    --
    updateAndReturnEffect :: GLSL a b -> Handler Effect
    updateAndReturnEffect (GLSL vertexShader fragmentShader _ _) = do
      let newEffect =
            effect { effectCompiles = True
                   , effectFragShaderCode = Just . addShaderHeaders $ fragmentShader
                   , effectVertShaderCode = Just . addShaderHeaders $ vertexShader }
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
writeEffectFileToTemp :: Text -> Handler FilePath
writeEffectFileToTemp text = do
  (path, h) <- liftIO mkTemp
  liftIO $ ((hPutStrLn h) . T.unpack . effectCodeWrapper . indent $ text) >> hClose h
  return path
  where
    indent :: Text -> Text
    indent = T.concat . intersperse "\n" . map ("    " `T.append`) . T.lines


defaultFragShaderCode :: UnsafeText
defaultFragShaderCode = UnsafeText . T.unlines $ [
    "precision highp float;"
  , ""
  , "uniform float time;"
  , "void main() {"
  , "  gl_FragColor = vec4(0.5,0.5,0.5,1.0);"
  , "}" ]

defaultVertShaderCode :: UnsafeText
defaultVertShaderCode = UnsafeText . T.unlines $ [
    "uniform mat4 ModelViewProjectionMatrix;"
  , "uniform mat3 NormalMatrix;"
  , ""
  , "attribute vec2 uv_a;"
  , "void main() {"
  , "  gl_Position = ModelViewProjectionMatrix * vec4(uv_a.x, uv_a.y, 1.0, 1.0);"
  , "}" ]

--
-- This Haskell code needs to be run through CPP to be valid.
--
effectCodeWrapper :: Text -> Text
effectCodeWrapper codeStr = T.unlines [
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
  , ""
  , "makeFullSurf :: HasColor c => (T -> SurfD) -> ImageB c -> SurfB"
  , "makeFullSurf surf im t = (basicStd,view1,surf t, toColor . im (powVal t))"
  , ""
  , "EFFECT_NAME :: GLSL R1 R2"
  , "EFFECT_NAME = surfBProg eyePosE $ makeFullSurf surface image"
  , "  where"
  , "    eyePosE :: EyePosE"
  , "    eyePosE = pureE (V.vec3 ex ey ez) where (ex,ey,ez) = eyePos"
  , "{-# LINE 1 \"Code.hs\" #-}" ] `T.append` codeStr

addShaderHeaders :: String -> Text
addShaderHeaders shaderStr =  T.unlines [
    "#define gl_ModelViewProjectionMatrix ModelViewProjectionMatrix"
  , "#define gl_NormalMatrix NormalMatrix"
  , ""
  , "precision highp float;"
  , ""
  , "uniform mat4 gl_ModelViewProjectionMatrix;"
  , "uniform mat3 gl_NormalMatrix;"
  , ""
  , "#define False false"
  , "#define _uniform time"
  , "#define _attribute uv_a"
  , "#define _varying_F uv_v"
  , "#define _varying_S pos_v" ] `T.append` (T.pack shaderStr)
