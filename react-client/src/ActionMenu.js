import React, { Component } from "react";

export default class ActionMenu extends Component {
  render() {
    if (!this.props.active) return <div />;
    return (
      <div className={"ActionMenu "+this.props.color+"Text"}
        style={{
          fontSize: this.props.size + "px"
        }}
        >
        {this.props.passAvailable && (<button onClick={this.props.onPassing} >Pass</button>)}
        <button onClick={this.props.onDeclareFinished}>Declare finished</button>
      </div>
    );
  }
}
