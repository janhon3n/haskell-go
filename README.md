# haskell-go

haskell-go is the Go board game created in haskell. It uses the Happstack HTTP server to allow gameplay over HTTP. There is no state in the server. The client needs to send the whole game state and the next action to get a new gamestate in the response.

There is also a web interface created with the React framework that the Happstack server serves.

## How to install
A built executable is included in the root folder, named "haskell-go.exe". You can launch the program simply by executing it.

To build, make sure you have Stack installed (> 1.7.1) and run
```
stack build
```

To launch the server after being built run
```
stack exec haskell-go-exe
```

The react client is already built in /react-client/build/. To build again you need to first install the dependencies and then run the build command. Make sure first that you are in the /react-client folder.
```
npm install
npm run build
```
