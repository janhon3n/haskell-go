import React, { Component } from "react";
import "./Board.css";

export default class Board extends Component {
  render() {
    let gridSize = this.props.size / this.props.board.length;
    return (
      <div className="boardWrapper">
        <div
          className="board"
          style={{
            width: this.props.size + "px",
            height: this.props.size + "px",
            left: gridSize / 2 + "px",
            top: gridSize / 2 + "px",
          }}
        >
          {this.props.board.map((row, i) => {
            return (
              <div className="row">
                {row.map((place, j) => {
                  return (
                    <div className="placeBox">
                      <div
                        className={
                          place.tag === "Empty"
                            ? "place " + place.tag.toLowerCase()
                            : "place " + place.contents.toLowerCase()
                        }
                        onClick={() => {
                          if (place.tag === "Empty") this.props.onMove([i, j]);
                        }}
                      />
                    </div>
                  );
                })}
              </div>
            );
          })}
        </div>
      </div>
    );
  }
}
