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
    }), /*#__PURE__*/React.createElement("h2", null, "Welcome to React")), /*#__PURE__*/React.createElement("p", {
      className: "App-intro"
    }, "To get started, edit ", /*#__PURE__*/React.createElement("code", null, "src/App.js"), " and save to reload."), /*#__PURE__*/React.createElement("h3", null, "prop number is : ", this.props.propNumber), /*#__PURE__*/React.createElement("h3", null, "prop number is : ", this.props.propString), /*#__PURE__*/React.createElement("h3", null, "prop number is : ", this.props.propObject.obj1), /*#__PURE__*/React.createElement(Parent, null));
  }

}

App.propTypes = {
  propObject: React.PropTypes.object,
  propString: React.PropTypes.string,
  propNumber: React.PropTypes.number
};
App.defaultProps = {
  propNumber: 3,
  propString: "THis is prop string",
  propObject: {
    obj1: "I am obj 1",
    obj2: "I am obj 2",
    obj3: "I am obj 3"
  }
};

class Parent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      cars: ['s-BMW', 's-MERC', 's-City', 's-Audi']
    };
    this.handleClick = this.handleClick.bind(this);
  }

  handleClick() {
    this.setState({
      cars: this.state.cars.reverse()
    });
  }

  render() {
    return /*#__PURE__*/React.createElement("div", null, /*#__PURE__*/React.createElement("h2", {
      onClick: this.handleClick
    }, "Just some info"), /*#__PURE__*/React.createElement(Cars, {
      msg: "cars are cool",
      model: "34765",
      coolCars: this.state.cars
    }));
  }

}

Parent.defaultProps = {
  cars: ['BMW', 'MERC', 'City', 'Audi']
};

class Cars extends Component {
  render() {
    return /*#__PURE__*/React.createElement("div", null, /*#__PURE__*/React.createElement("h3", null, " I am from cars component"), /*#__PURE__*/React.createElement("p", null, this.props.msg), /*#__PURE__*/React.createElement("p", null, this.props.model), /*#__PURE__*/React.createElement("div", null, this.props.coolCars.map((item, i) => {
      return /*#__PURE__*/React.createElement("p", {
        key: i
      }, item);
    })));
  }

}

export default App;

