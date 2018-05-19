import React, { Component } from "react";
import "./Game.css";
import Board from "./Board";
import Bowl from "./Bowl";
import ActionMenu from "./ActionMenu";
import EndMenu from "./EndMenu";

class Game extends Component {
  constructor(props) {
    super();
    this.sendMove = this.sendMove.bind(this);
    this.placeStone = this.placeStone.bind(this);
    this.passTurn = this.passTurn.bind(this);
    this.declareFinished = this.declareFinished.bind(this);
    this.state = {
      gameState: null,
      postInProgress: false,
      screenSize: {
        width: 0,
        height: 0
      }
    };
  }

  updateScreenSize() {
    this.setState({
      screenSize: {
        width: window.innerWidth,
        height: window.innerHeight
      }
    });
  }

  updateGameState(newGameState) {
    console.log(newGameState);
    if (newGameState.board !== undefined) {
      this.setState({ gameState: newGameState });
    }
  }
  async componentDidMount() {
    this.updateScreenSize();
    window.addEventListener("resize", this.updateScreenSize.bind(this));

    let response = await fetch("/newgame", {
      method: "post",
      body: JSON.stringify({
        boardSize: this.props.boardSize
      })
    });
    this.updateGameState(await response.json());
  }

  componentWillUnmount() {
    window.removeEventListener("resize", this.updateScreenSize.bind(this));
  }

  async sendMove(move) {
    try {
      console.log(move)
      this.setState({ postInProgress: true });
      let response = await fetch("/game", {
        method: "post",
        body: JSON.stringify({
          gameState: this.state.gameState,
          move: move
        })
      });
      console.log(response);
      this.updateGameState(await response.json());
    } catch (err) {
      console.log(err);
    } finally {
      this.setState({ postInProgress: false });
    }
  }

  placeStone(place) {
    this.sendMove({
      moveType: "StonePlacing",
      place: place
    });
  }

  passTurn() {
    this.sendMove({
      moveType: "Passing",
      place: [0, 0]
    });
  }

  declareFinished() {
    this.sendMove({
      moveType: "Finishing",
      place: [0, 0]
    });
  }

  render() {
    if (this.state.gameState === null) {
      return <div>Loading...</div>;
    }

    let boardSize = this.state.screenSize.height * 0.8;
    if (this.state.screenSize.width / 5 < this.state.screenSize.height / 3) {
      boardSize = this.state.screenSize.width * 0.8 * 3 / 5;
    }
    let bowlSize = boardSize / 3;
    let textSize = bowlSize / 9;

    let blackPlayer =
      this.state.gameState.playerInTurn.playerSide === "Black"
        ? this.state.gameState.playerInTurn
        : this.state.gameState.otherPlayer;

    let whitePlayer =
      this.state.gameState.playerInTurn.playerSide === "White"
        ? this.state.gameState.playerInTurn
        : this.state.gameState.otherPlayer;

    blackPlayer.passAvailable = !(
      (blackPlayer.hasPassed && whitePlayer.hasPassed) ||
      whitePlayer.hasFinished
    );

    whitePlayer.passAvailable = !(
      (blackPlayer.hasPassed && whitePlayer.hasPassed) ||
      blackPlayer.hasFinished
    );

    return (
      <div className="Game">
        <main>
          <div className="sideMenu">
            <Bowl
              side="black"
              color="white"
              stoneCount={whitePlayer.captured}
              size={bowlSize}
            />
            {this.state.gameState.playerInTurn.playerSide == "White" &&
              !this.state.gameState.gameOver && (
              <h2 style={{
                fontSize:textSize*1.5
              }}>White's turn</h2>
            )}
            {!this.state.gameState.gameOver ? (
              <ActionMenu
                passAvailable={whitePlayer.passAvailable}
                onPassing={this.passTurn}
                onDeclareFinished={this.declareFinished}
                active={
                  this.state.gameState.playerInTurn.playerSide === "White"
                }
                color='white'
                size={textSize}
              />
            ) : (
              <EndMenu
                color='white'
                size={textSize}
                score={whitePlayer.finalScore}
                onExit={this.props.onEnd}
              />
            )}
          </div>
          <Board
            board={this.state.gameState.board}
            size={boardSize}
            onMove={this.placeStone}
          />
          <div className="sideMenu">
            {!this.state.gameState.gameOver ? (
              <ActionMenu
                passAvailable={blackPlayer.passAvailable}
                onPassing={this.passTurn}
                onDeclareFinished={this.declareFinished}
                active={
                  this.state.gameState.playerInTurn.playerSide === "Black"
                }
                color='black'
                size={textSize}
              />
            ) : (
              <EndMenu
                color='black'
                size={textSize}
                score={blackPlayer.finalScore}
                onExit={this.props.onEnd}
              />
            )}
            {this.state.gameState.playerInTurn.playerSide == "Black" &&
              !this.state.gameState.gameOver && (
              <h2 className='blackText' style={{
                fontSize:textSize*1.5,
              }}>Black's turn</h2>
            )}
            <Bowl
              side="white"
              color="black"
              stoneCount={blackPlayer.captured}
              size={bowlSize}
              align="end"
            />
          </div>
        </main>
      </div>
    );
  }
}

export default Game;
