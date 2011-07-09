module Handler.Compile where

-- standard libraries
import System.Plugins as Plugins
import System.Directory
import System.FilePath
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C
import Yesod.Helpers.Static (base64md5)

import qualified Data.Text as T

import Shady.CompileE

-- friends
import Foundation

-- | Compile the effect code. Return either the path to the compiled GLSL program (Right) or
--   the compiler error (Left).
--
compileEffect :: Effect -> Handler (Either String (String, FilePath))
compileEffect effect = do
  foundation <- getYesod
  let hash          = codeHash effect
      effectSrcFile = (cacheDir foundation) </> hash <.> "hs"
      effectObjectFile = (cacheDir foundation) </> hash <.> "o"

  exists <- liftIO $ doesFileExist effectObjectFile
  case exists of
    True  -> return (Right (hash, effectObjectFile))
    False -> do
      liftIO . (writeFile effectSrcFile) $ effectCodeWrapper
               ++ (indent . T.unpack $ effectCode effect) ++ "\n"
      res <- liftIO $ Plugins.make effectSrcFile ["-DMODULE_NAME=" ++ hash, "-DJOB=job" ++ hash]
      case res of
        MakeSuccess _  objectFile -> return (Right (hash, objectFile))
        MakeFailure errors        -> return (Left (concat $ intersperse "\n" errors))

  where
    indent = concat . intersperse "\n" . map ("    " ++) . lines

-- | Produce a hash of an effect's code string:
--   The returned hash value will be a valid Haskell module name. That is, the first
--   character will be a capital letter, and all other characters will either be
--   alphanumeric or an underscore.
--
codeHash :: Effect -> String
codeHash effect = tidy . base64md5 . C.pack . T.unpack $ effectCode effect
  where
    tidy s = "E" ++ (map validChars s)
    validChars c | isAlphaNum c = c
    validChars _                = '_'

effectCodeWrapper = unlines $ [
    "module Effect where"
  , "import Data.Boolean"
  , "import Shady.Lighting"
  , "import Data.Derivative"
  , "import Data.VectorSpace"
  , "import Shady.ParamSurf"
  , "import Shady.CompileSurface"
  , "import Shady.Color"
  , "import Shady.Language.Exp hiding (get)"
  , "import Shady.Complex"
  , "import Shady.Misc"
  , "import Shady.CompileE"
  , "import qualified Shady.Vec as V"
  , "" ]