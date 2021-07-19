import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import Header from './Header';
import Intro from './Intro';

class App extends Component {
  render() {
    return /*#__PURE__*/React.createElement("div", {
      className: "App"
    }, /*#__PURE__*/React.createElement(Header, null), /*#__PURE__*/React.createElement("p", null, " Hey "), /*#__PURE__*/React.createElement(Intro, null));
  }

}

export default App;

