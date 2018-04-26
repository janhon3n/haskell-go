{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module GoHttpServer where
import Board
import GameState
import Move
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
instance FromJSON MoveType
instance ToJSON MoveType
instance FromJSON Move
instance ToJSON Move
instance FromJSON PlaceData
instance ToJSON PlaceData
instance FromJSON Player
instance ToJSON Player
instance FromJSON GameState
instance ToJSON GameState

data JSONMove = JSONMove { gameState :: GameState
      , move :: Move
} deriving (Show, Eq, Generic)

instance FromJSON JSONMove
instance ToJSON JSONMove

data JSONNewGame = JSONNewGame { boardSize :: Int } deriving (Show, Eq, Generic)


instance FromJSON JSONNewGame
instance ToJSON JSONNewGame

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
                  ok $ toResponse $ encode (initialState (9,9))
            , dir "game" $ do
                  method POST
                  handleGameTurn
            , dir "newgame" $ do
                  method POST
                  handleNewGame
            , do
                  nullDir
                  movedPermanently ("/index.html"::String) (toResponse ("Redirected to /index.html"::String))
            , serveDirectory EnableBrowsing [] "./react-client/build" ]

handleNewGame :: ServerPart Response
handleNewGame = do
      body <- getBody
      let newGameData = fromJust $ decode body :: JSONNewGame
      let boardDimensions = ((boardSize newGameData), (boardSize newGameData))
      ok $ toResponse $ encode (initialState boardDimensions)


handleGameTurn :: ServerPart Response
handleGameTurn = do
      body <- getBody
      let gameData = fromJust $ decode body :: JSONMove
      if moveIsValid (gameState gameData) (move gameData)
            then do
                  let newGameState = executeMove (gameState gameData) (move gameData)
                  ok $ toResponse $ encode newGameState
            else noContent $ toResponse ("Invalid move" :: String)

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 
