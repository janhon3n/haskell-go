{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module GoHttpServer where
import Go
import Board
import Player

import Data.Maybe
import Data.Aeson
import Data.Data (Data, Typeable)
import Data.ByteString.Lazy.Char8 as L
import Control.Monad
import Happstack.Server
import Happstack.Server.Types
import Control.Monad.IO.Class (liftIO)
import GHC.Generics

instance FromJSON Side
instance ToJSON Side
instance FromJSON PlaceData
instance ToJSON PlaceData
instance FromJSON PlayerType
instance ToJSON PlayerType
instance FromJSON Player
instance ToJSON Player
instance FromJSON GameState
instance ToJSON GameState

data JSONMove = JSONMove { gameState :: GameState
      , place :: Place
} deriving (Show, Eq, Generic)

instance FromJSON JSONMove
instance ToJSON JSONMove

conf :: Conf
conf = Conf { port = 3008
      , validator = Nothing
      , logAccess = Just logMAccess
      , timeout = 60
      }

bodyPolicy :: BodyPolicy
bodyPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

startServer :: IO ()
startServer = simpleHTTP nullConf $ handlers

handlers :: ServerPart Response
handlers = do
      decodeBody bodyPolicy
      msum [ dir "game" $ do
                  method GET
                  ok $ toResponse $ encode (initialState (9,9) (Human, Human))
            , dir "game" $ do
                  method POST
                  handleGameTurn
            , serveDirectory EnableBrowsing [] "./react-client/build" ]

handleGameTurn :: ServerPart Response
handleGameTurn = do
      body <- getBody
      let move = fromJust $ decode body :: JSONMove
      if moveIsValid (gameState move) (place move)
            then do
                  let newGameState = executeMove (gameState move) (place move)
                  ok $ toResponse $ encode newGameState
            else noContent $ toResponse ("Invalid move" :: String)

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 
