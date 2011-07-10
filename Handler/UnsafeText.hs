module Handler.UnsafeText where

import Data.String
import Data.Text (pack)
import Text.Blaze

--
-- UnsafeText will not be escaped when interpolating in Hamlet templates.
--
newtype UnsafeText = UnsafeText Text

instance IsString UnsafeText where
  fromString = UnsafeText . pack

instance ToHtml UnsafeText where
  toHtml (UnsafeText text) = preEscapedText text
