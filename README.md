# haskell-go

haskell-go is the Go board game created in haskell. It uses the Happstack HTTP server to allow gameplay over HTTP. There is no state in the server. The client needs to send the whole game state and the next action to get a new gamestate in the response.

There is also a web interface created with the React framework that the Happstack server serves.

## Go structure

The following files are responsible for the game functionality.

File | Responsibility
-----|----------------------------------------------------
Board.hs | Declare data types for Side and PlaceData and types for Place, Board etc. Add functions for manipulating the board state.
Region.hs | Declare type for Region as a list of Places. Add functions for finding uniform regions from a board and manipulating regions. Used for analysing the board for allowed moves, capturing stones and score calculation.




## Happstack config