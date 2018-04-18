import React, { Component } from "react";
import * as seed from "seed-random";

export default class Bowl extends Component {
  render() {
    let random = seed(this.props.side)
    let stoneSize = this.props.size / 5
    return (
      <div style={{
        textAlign:'center',
        alignSelf: this.props.align === "end" ? "flex-end" : "flex-start"
      }}>
        <div
          style={{
            position: "relative",
            width: this.props.size + "px",
            height: this.props.size + "px",
            backgroundImage: 'url("/bowl.svg")',
            backgroundSize: "100%",
            backgroundRepeat: "no-repeat",
          }}
        >
          {[...Array(this.props.stoneCount)].map((_, i) => {
            let angle = random() * 2 * Math.PI;
            let step = random() * this.props.size / 2.5;
            let offsetX = step * Math.cos(angle);
            let offsetY = step * Math.sin(angle);
            return (
              <img
                style={{
                  width: stoneSize + "px",
                  height: stoneSize + "px",
                  position: "absolute",
                  left: this.props.size / 2 + offsetX - (stoneSize/2) + "px",
                  top: this.props.size / 2 + offsetY - (stoneSize/2) + "px"
                }}
                src={"/stone_" + this.props.side + ".svg"}
              />
            );
          })}
        </div>
        <div>{this.props.stoneCount}</div>
      </div>
    );
  }
}
