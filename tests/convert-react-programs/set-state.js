import React, { Component } from 'react'

class App extends Component {
  constructor() {
    this.state = {};
  }

  testMethod() {
    var stateRef = this.state
    stateRef.x = 3;
  }
}

var app = new App();

app.testMethod();