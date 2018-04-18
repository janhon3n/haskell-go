import React, { Component } from "react"
import "./App.css"
import Board from "./Board"
import Bowl from "./Bowl"

class App extends Component {
  constructor(props) {
    super();
    this.sendMove = this.sendMove.bind(this)
    this.state = {
      gameState: null,
      postInProgress: false,
      screenSize: {
        width:0,
        height:0,
      }
    };
  }

  updateScreenSize() {
    this.setState({screenSize: {
      width: window.innerWidth,
      height: window.innerHeight,
    }})
  }

  updateGameState(newGameState) {
    console.log(newGameState);
    if (newGameState.board !== undefined) {
      this.setState({ gameState: newGameState });
    }
  }
  async componentDidMount() {
    this.updateScreenSize()
    window.addEventListener("resize", this.updateScreenSize.bind(this))

    let response = await fetch("/game", { method: "get" });
    this.updateGameState(await response.json());
  }
  
  componentWillUnmount() {
    window.removeEventListener("resize", this.updateScreenSize.bind(this))
  }
  
  async sendMove(place) {
    try {
      this.setState({ postInProgress: true });
      let response = await fetch("/game", {
        method: "post",
        body: JSON.stringify({
          gameState: this.state.gameState,
          place: place
        })
      });
      this.updateGameState(await response.json());
    } catch (err) {
      console.log(err);
    } finally {
      this.setState({ postInProgress: false });
    }
  }

  render() {
    if (this.state.gameState === null) {
      return <div>Loading...</div>;
    }
    let boardSize = this.state.screenSize.height * 0.8
    let bowlSize = boardSize / 3
    if (this.state.screenSize.width/5 < this.state.screenSize.height/3) {
      boardSize = this.state.screenSize.width * 0.8 * 3 / 5
      bowlSize = boardSize / 3
    }

    let blackPlayer = this.state.gameState.playerInTurn[1] === "Black" ?
      this.state.gameState.playerInTurn : this.state.gameState.otherPlayer
    let whitePlayer = this.state.gameState.playerInTurn[1] === "White" ?
      this.state.gameState.playerInTurn : this.state.gameState.otherPlayer

    return (
      <div className="App">
        <main>
          <Bowl side="black" stoneCount={blackPlayer[2]} size={bowlSize}/>
          <Board board={this.state.gameState.board} size={boardSize} onMove={this.sendMove}/>
          <Bowl side="white" stoneCount={whitePlayer[2]} size={bowlSize} align="end"/>
        </main>
      </div>
    );
  }
}

export default App;
