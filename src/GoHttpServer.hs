module GoHttpServer where

import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

server :: ServerPart Response
server = path $ \(msg :: String) ->
   ok $ template "Haskell go" $ do
      H.h1 "Haskell GO"
      H.p $ a ! href "/datajotain"