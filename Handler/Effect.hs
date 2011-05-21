{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Handler.Effect where

-- libraries
import Data.Text hiding (map, length)
import qualified Data.Text as Text

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
createFormlet :: Maybe Text -> Form s m Text
createFormlet s = fieldsToDivs $ stringField "Add new effect" s

information :: String -> Html
information infoStr =
  if length infoStr > 0
  then {-$(Foundation.hamletFile "info")-} [$hamlet| div.info $str$|]
  else ""



postCreateEffectR :: Handler RepHtml
postCreateEffectR = error "not implemented"

putCreateEffectR :: Handler RepHtml
putCreateEffectR = error "not implemented"

getShowEffectR :: String -> String -> Handler RepHtml
getShowEffectR = error "not implemented"

getEditEffectR :: String -> Handler RepHtml
getEditEffectR = error "not implemented"

postUpdateEffectR :: String -> Handler RepHtml
postUpdateEffectR = error "not implemented"

putUpdateEffectR :: String -> Handler RepHtml
putUpdateEffectR = error "not implemented"

deleteDeleteEffectR :: String -> Handler RepHtml
deleteDeleteEffectR = error "not implemented"

postDeleteEffectR :: String -> Handler RepHtml
postDeleteEffectR = error "not impelemented"

getRunEffectR :: String -> Handler RepHtml
getRunEffectR = error "not implemented"

postResultEffectR :: String -> Handler RepHtml
postResultEffectR = error "not implemented"
