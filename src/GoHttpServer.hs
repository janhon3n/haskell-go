module GoHttpServer where
import Control.Monad
import Happstack.Server

conf :: Conf
conf = Conf { port = 3008
      , validator = Nothing
      , logAccess = Just logMAccess
      }

startServer :: IO ()
startServer = simpleHTTP conf $ msum [ dir "static" $ serveDirectory EnableBrowsing ["client.js", "index.html"] "react-client/build" ]
