import React, { Component } from "react";
import Game from "./Game";
import MainMenu from "./MainMenu";

export default class App extends Component {
  constructor(props) {
    super(props);
    this.setPlayerTypes = this.setPlayerTypes.bind(this);
    this.setBoardSize = this.setBoardSize.bind(this);
    this.startNewGame = this.startNewGame.bind(this);
    this.state = {
      view: "menu",
      playerTypes: ["Human", "Human"],
      boardSize: 9
    };
  }

  setPlayerTypes(types) {
    this.setState({ playerTypes: types });
  }

  setBoardSize(size) {
    this.setState({ boardSize: size });
  }

  startNewGame() {
    this.setState({ view: "game" });
  }

  render() {
    switch (this.state.view) {
      case "menu":
        return (
          <MainMenu
            boardSize={this.state.boardSize}
            playerTypes={this.state.playerTypes}
            onPlayerTypesChange={this.setPlayerTypes}
            onBoardSizeChange={this.setBoardSize}
            onStart={this.startNewGame}
          />
        );
        break;
      case "game":
        return (
          <Game
            boardSize={this.state.boardSize}
            playerTypes={this.state.playerTypes}
          />
        );
        break;
    }
  }
}
