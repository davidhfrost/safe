import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';

class App extends Component {
  constructor(props) {
    super(props);
    this.yourname = "ammy";
    this.state = {};
  }

  sayhello(name) {
    return "Hello " + name;
  }

  render() {
    const myName = "sammy";
    return /*#__PURE__*/React.createElement("div", {
      className: "App"
    }, /*#__PURE__*/React.createElement("h2", null, "Just some sample data: ", this.yourname));
  }

}

export default App;

