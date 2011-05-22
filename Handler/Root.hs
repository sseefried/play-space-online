{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Foundation

getRootR :: Handler RepHtml
getRootR = redirect RedirectTemporary $ ListEffectsR

    -- mu <- maybeAuth
    -- defaultLayout $ do
    --     h2id <- lift newIdent
    --     setTitle "play-space-online homepage"
    --     addWidget $(widgetFile "homepage")
