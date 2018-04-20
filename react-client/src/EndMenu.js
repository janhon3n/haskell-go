import React, { Component } from 'react';

export default class EndMenu extends Component {
   render() {
      return (
         <div className="EndMenu"
           style={{
             fontSize: this.props.size + "px"
           }}
           >
           <div>{"Score: " + this.props.score}</div>
           <button onClick={this.props.onExit}>Back to menu</button>
         </div>
       );
   }
}