var React = {
  createElement: function(type, props, children) {
    if (props === null) props = {};

    var childArray = [];

    for (var i = 2; i < arguments.length; i++) {
      var child = arguments[i];
      if (child === null) continue;
      if (typeof child === 'string') {
        child = { type: 'plaintext', props: { text: child, children: [] } };
      }

      if (Array.isArray(child)) {
        for (var j = 0; j < child.length; j++) {
          childArray.push(child[j]);
        }
      } else {
        childArray.push(child);
      }
    }

    props.children = childArray;

    return { type: type, props: props };
  }
};


React.Component = function() {};
React.Component.prototype.__class__ = true;
React.Component.prototype.setState = function(partialState) {
  for (var prop in partialState) {
    this.state[prop] = partialState[prop];
  }
};
React.Component.prototype.componentDidMount = function() {};


export default React;
var Component = React.Component;
export { Component as Component };

