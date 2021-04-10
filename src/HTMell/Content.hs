module HTMell.Content (HTContent(..), html, page) where

import HTMell.Path (HTPath)
import Data.Map (Map)
import Data.Text (pack)
import Data.ByteString.Lazy (ByteString)
import Cheapskate (markdown, def)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data HTContent = HTContent {
    path        :: HTPath,
    metadata    :: Map String String,
    rawContent  :: String
}

html :: HTContent -> Html
html htc = toHtml . markdown def . pack $ rawContent htc

page :: HTContent -> ByteString
page = renderHtml . html
