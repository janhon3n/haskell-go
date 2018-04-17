module GoHttpServer where

{- import Happstack.Server
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

conf :: Conf
conf = Conf { port = 3008
      , validator = Nothing
      , logAccess = Just logMAccess
      }

startServer :: IO ()
startServer = simpleHTTP conf $ msum [ dir "menu" $ menu
      , dir "play" $ play
      ]

menu :: ServerPart String
menu = path $ \s -> ok $ "Hello, " ++ s -}