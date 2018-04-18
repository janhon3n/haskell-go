import React, { Component } from "react";
import "./App.css";

class App extends Component {
  constructor(props) {
    super();
    this.state = {
      gameState: null,
      postInProgress: false
    };
  }

  updateGameState(newGameState) {
    console.log(newGameState);
    if (newGameState.board !== undefined) {
      this.setState({ gameState: newGameState });
    }
  }

  async componentDidMount() {
    let response = await fetch("/game", { method: "get" });
    this.updateGameState(await response.json());
  }

  async sendMove(place) {
    try{
      this.setState({ postInProgress: true });
      let response = await fetch("/game", {
        method: "post",
        body: JSON.stringify({
          gameState: this.state.gameState,
          place: place
        })
      });
      this.updateGameState(await response.json());
      this.setState({ postInProgress: false });
    } catch(err) {
      console.log(err)
    }
  }

  render() {
    if (this.state.gameState === null) {
      return <div>Loading...</div>;
    }
    return (
      <div>
        <h1>Haskell Go!</h1>
        {<h2>{this.state.gameState.playerInTurn[1]} players turn</h2>}
        <div className="board">
          {this.state.gameState !== null &&
            this.state.gameState.board.map((row, i) => {
              return (
                <div className="row">
                  {row.map((place, j) => {
                    if (place.tag === "Empty") {
                      return (
                        <div
                          className={"place " + place.tag.toLowerCase()}
                          onClick={() => {
                            if (place.tag === "Empty") this.sendMove([i, j]);
                          }}
                        />
                      );
                    } else {
                      return (
                        <div
                          className={"place " + place.contents.toLowerCase()}
                        />
                      );
                    }
                  })}
                </div>
              );
            })}
        </div>
        {this.state.postInProgress && <h3>Posting your move...</h3>}
      </div>
    );
  }
}

export default App;
