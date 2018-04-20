import React, { Component } from "react";
import "./MainMenu.css";

export default class MainMenu extends Component {
  boardSizes = [9, 13, 19]
  possiblePlayerTypes = [["Human", "Human"], ["Human", "RandomAI"], ["Human", "TreeAI"]]

  render() {
    return (
      <div className="MainMenu">
        <h1>haskell-go</h1>
        <ul className="select">
          {this.boardSizes.map(bs => {
            return <li
              className={this.props.boardSize === bs && "selected"}
              onClick={() => {
                this.props.onBoardSizeChange(bs);
              }}
            >
              {bs + "x" + bs}
            </li>;
          })}
        </ul>
        <ul className="select">
          {this.possiblePlayerTypes.map(pts => {
            return <li
              className={
                this.props.playerTypes[0] === pts[0] &&
                this.props.playerTypes[1] === pts[1] &&
                "selected"
              }
              onClick={() => {
                this.props.onPlayerTypesChange(pts);
              }}
            >
              {pts[0] + " vs " + pts[1]}
            </li>;
          })}
        </ul>
        <button onClick={this.props.onStart}>Start</button>
      </div>
    );
  }
}
