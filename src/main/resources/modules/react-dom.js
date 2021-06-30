var ReactDOM = {
  render: function(element, container) {
    // HTML element component
    if (typeof element.type === "string") {
      // recursively render children into an array
      var children = [];

      for (var i = 0; i < element.props.children.length; i++) {
        var child = element.props.children[i];
        if (typeof child === 'string') {
          children[i] = child;
        } else {
          children[i] = ReactDOM.render(child);
        }
      }

      return {
        type: element.type,
        props: { children: children },
      };
    }
    // function component.
    // this is detected using the `React.Component.prototype.__class__` flag,
    // which will be `true` for any component extending `React.Component`.
    else if (!element.type.prototype.__class__) {
      return ReactDOM.render(element.type(element.props));
    }
    // class component
    else {
      var instance = new element.type();
      // ideally, this would create a shallow copy of `element.props`
      instance.props = element.props;
      var rendered = ReactDOM.render(instance.render());
      rendered.instance = instance;
      return rendered;
    }
  }
};

export default ReactDOM;