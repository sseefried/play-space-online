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

-- | Compile the effect code. Return either the path to the compiled GLSL program (Right) or
--   the compiler error (Left).
--
compileEffect :: Key Effect -> Effect -> Handler (Either String ())
compileEffect key effect = do
  foundation <- getYesod
  (path, h) <- liftIO mkTemp
  liftIO $ ((hPutStrLn h) . T.unpack . effectCodeWrapper . indent $ effectCode effect) >> hClose h
  -- We need a unique name for the effect that we will temporarily load into memory.
  -- This is, after all, a web server serving multiple clients.
  let moduleExt = takeBaseName path
      moduleName = moduleExt
      effectName = map toLower moduleExt
  res <- liftIO $ Plugins.make path [ "-DMODULE_NAME=" ++ moduleName
                                    , "-DEFFECT_NAME=" ++ effectName ]
  case res of
     MakeSuccess _  objectFile -> do
       mbStatus <- liftIO $ Plugins.load objectFile [] [] effectName
       case mbStatus of
         LoadSuccess modul (GLSL vertexShader fragmentShader _ _) -> do
            runDB $ replace key (effect { effectCompiles = True
                                , effectFragShaderCode = Just . addShaderHeaders $ fragmentShader
                                , effectVertShaderCode = Just . addShaderHeaders $ vertexShader })
            liftIO $ Plugins.unload modul
            return (Right ())
         LoadFailure msg -> return (Left . concat $ intersperse "\n" msg)
       -- Load the plugin, get the code, update the effect, unload the effect object.
     MakeFailure errors        -> return (Left (concat $ intersperse "\n" errors))
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
