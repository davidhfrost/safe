import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';

class App extends Component {
  render() {
    return /*#__PURE__*/React.createElement("div", {
      className: "App"
    }, /*#__PURE__*/React.createElement("div", {
      className: "App-header"
    }, /*#__PURE__*/React.createElement("img", {
      src: logo,
      className: "App-logo",
      alt: "logo"
    }), /*#__PURE__*/React.createElement("h2", null, "Components life cycle")), /*#__PURE__*/React.createElement(Body, null));
  }

}

class Body extends Component {
  constructor(props) {
    super(props);
    this.state = {
      r: 0
    };
    this.getRandomNumber = this.getRandomNumber.bind(this);
  }

  getRandomNumber() {
    // console.log("randome number called");
    this.setState({
      r: Math.floor(Math.random() * 10)
    });
  }

  render() {
    return /*#__PURE__*/React.createElement("div", null, /*#__PURE__*/React.createElement("p", {
      className: "App-intro"
    }, "To get started, edit ", /*#__PURE__*/React.createElement("code", null, "src/App.js"), " and save to reload."), /*#__PURE__*/React.createElement("button", {
      onClick: this.getRandomNumber
    }, " Random Number"), /*#__PURE__*/React.createElement(Numbers, {
      myNumber: this.state.r
    }));
  }

}

class Numbers extends Component {
  componentDidMount() {
    console.log("componentDidMount called here");
  }

  componentWillMount() {
    console.log("componentWillMount called here");
  }

  componentWillReceiveProps(newProps) {
    console.log("componentWillReceiveProps called");
  }

  shouldComponentUpdate(newProps, nextState) {
    console.log('Called should component Update');
    return true;
  }

  componentWillUpdate(newProps, nextState) {
    console.log('Called component Will Update');
  }

  componentDidUpdate(newProps, nextState) {
    console.log('Called component Did Update');
  }

  componentWillUnmount() {
    console.log('Called componentWill un mount');
  }

  render() {
    return /*#__PURE__*/React.createElement("div", null, /*#__PURE__*/React.createElement("br", null), this.props.myNumber);
  }

}

export default App;

