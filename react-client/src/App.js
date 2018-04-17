import React, { Component } from "react";
import "./App.css";

class App extends Component {
  constructor(props) {
    super();
    this.state = {
      gameState: null
    };
  }

  async componentDidMount() {
    let response = await fetch("/game", { method: "get" });
    let gameState = await response.json();
    console.log(gameState);
    if (gameState.board !== undefined) {
      this.setState({ gameState: gameState });
    }
  }

  render() {
    return (
      <div>
        <h1>Haskell Go!</h1>
        <div className="board">
          {this.state.gameState !== null &&
            this.state.gameState.board.map(row => {
              return (
                <div className="row">
                  {row.map(d => {
                    return <div className="place">{d.tag}</div>;
                  })}
                </div>
              );
            })}
        </div>
      </div>
    );
  }
}

export default App;
