import React, { Component } from "react";

export default class ActionMenu extends Component {
  render() {
    if (!this.props.active) return <div />;
    return (
      <div className="ActionMenu"
        style={{
          fontSize: this.props.size + "px"
        }}
        >
        <button>Pass</button>
        <button>Declare finished</button>
      </div>
    );
  }
}
