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
    // function component
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

function ReactState() {
  this.heap = {};
  this.shapeMap = {};
  this.stateMap = {};
  this.nextLoc = 0;
}

ReactState.prototype.mountElement = function mountElement(elt) {
  var desc = this.extractCompDesc(elt);
  this.mount(desc);
};

ReactState.prototype.mount = function mount(comp) {
  var loc = this.nextLoc++;
  var mountedComp = new MountedCompDesc(comp, loc);
  var isClassComponent = typeof comp.type === 'function' &&
      comp.type.prototype.__class__;

  this.stateMap[loc] = {};
  this.heap[loc] = { props: comp.props };

  // TODO: add component's listeners to listener map

  // inject a version of setState into class component
  // instances that is compatible with ReactState.
  if (isClassComponent) {
    var instance = comp.props.$$instance;
    instance.setState = this.setState.bind(this, loc);
    instance.props = comp.props;
  }

  var result = this.mounted(
    mountedComp,
    this.mountSeq(this.render(comp))
  );

  // call `componentDidMount` after rendering the component
  if (isClassComponent) {
    comp.props.$$instance.componentDidMount();
  }

  return result;
};

ReactState.prototype.mountSeq =   function mountSeq(comps) {
  if (comps.length === 0) return [];
  var head = this.mount(comps.shift());
  var tail = this.mountSeq(comps);
  tail.unshift(head);
  return tail;
}

ReactState.prototype.mounted = function mounted(mountedComp, childLocs) {
  this.shapeMap[mountedComp.loc] = {
    mountedComp: mountedComp,
    childLocs: childLocs,
  };

  return mountedComp.loc;
};

ReactState.prototype.unmount = function unmount(loc) {
  var mountedComp = this.shapeMap[loc];

  if (mountedComp === undefined) {
    return null;
  } else {
    delete this.shapeMap[loc];
    this.unmountSeq(mountedComp.childLocs);
    this.unmounted(loc);
  }
};

ReactState.prototype.unmountSeq = function unmountSeq(locs) {
  if (locs.length > 0) {
    var loc = locs.shift();
    this.unmount(loc);
    this.unmountSeq(locs);
  }

  return [];
}

ReactState.prototype.unmounted = function unmounted(loc) {
  // remove `loc` from listener map
}

ReactState.prototype.render = function(comp) {
  // sync the props of the component instance with
  // the props of the component descriptor.
  if (comp.props.$$instance !== undefined) {
    comp.props.$$instance.props = comp.props;
  }

  if (comp.props.render !== undefined) {
    var elt = comp.props.render(comp.props);
    var desc = this.extractCompDesc(elt);
    return desc.children;
  } else {
    return comp.children.slice(0);
  }
}

ReactState.prototype.rerender = function(loc, defaultComp) {
  var props = this.heap[loc].props;
  var comp = this.shapeMap[loc].mountedComp.comp;

  if (props.render !== undefined) {
    return this.extractCompDesc(comp.props.render(props));
  } else {
    return defaultComp;
  }
};

ReactState.prototype.reconcileSeq = function(comps, locs) {
  if (comps.length === 0 && locs.length === 0) return [];
  if (comps.length === 0) return this.unmountSeq(locs);
  if (locs.length === 0) return this.mountSeq(comps);

  var comp = comps[0];
  var loc = locs[0];
  var mountLoc = this.reconcile(comp, loc);

  var tailMountLocs = this.reconcileSeq(comps.slice(1), locs.slice(1));
  return [mountLoc].concat(tailMountLocs);
};

ReactState.prototype.reconcile = function(comp, loc) {
  var shape = this.shapeMap[loc];
  var mountedComp = shape.mountedComp.comp;

  if (comp.type !== mountedComp.type) {
    this.unmount(loc);
    return this.mount(comp);
  } else {
    var nextProps = comp.props;
    this.heap[loc].props = nextProps;

    var nextComp = this.rerender(loc, comp);
    var nextChildComps = nextComp.children;
    var nextChildLocs = this.reconcileSeq(nextChildComps, shape.childLocs)

    var nextMountedComp = new MountedCompDesc(comp, loc);

    return this.reconciled(nextMountedComp, nextChildLocs);
  }
};

ReactState.prototype.reconciled = function(mountedComp, childLocs) {
  var loc = mountedComp.loc;

  this.shapeMap[loc] = {
    mountedComp: mountedComp,
    childLocs: childLocs,
  };

  return loc;
};

ReactState.prototype.setState = function(loc, newState) {
  var nextState = this.stateMap[loc];
  var shape = this.shapeMap[loc];

  var instance = shape.mountedComp.comp.props.$$instance;
  if (instance !== undefined) {
    var compState = instance.state;
    for (var key in nextState) compState[key] = nextState[key];
    for (var key in newState) compState[key] = newState[key];
    this.stateMap[loc] = compState;
    instance.state = compState;
  }

  var nextChildComps = this.render(shape.mountedComp.comp);
  var nextChildLocs = this.reconcileSeq(nextChildComps, shape.childLocs);
  return this.reconciled(shape.mountedComp, nextChildLocs);
};

ReactState.prototype.extractCompDesc = function(element) {
  // plain text
  if (typeof element === 'undefined') {
    return new CompDesc('plaintext', { text: '' }, []);
  }
  else if (typeof element === 'string') {
    return new CompDesc('plaintext', { text: element }, []);
  }
  // html element
  else if (typeof element.type === 'string') {
    var children = [];
    var childElts = element.props.children;
    for (var i = 0; i < childElts.length; i++) {
      children[i] = this.extractCompDesc(childElts[i]);
    }

    return new CompDesc(element.type, element.props, children);
  }
  // function component
  else if (!element.type.prototype.__class__) {
    element.props.render = element.type;
    return new CompDesc(element.type, element.props, []);
  }
  // class component
  else {
    var instance = new element.type();
    element.props.$$instance = instance;
    element.props.render = instance.render.bind(instance);
    return new CompDesc(element.type, element.props, []);
  }
};

ReactState.prototype.printState = function() {
  var result = '\n====== Shape Map ======\n\n';
  for (var key in this.shapeMap) {
    var shape = this.shapeMap[key];
    result += key + ': '
            + shape.mountedComp.toString(this.heap, shape.childLocs)
            + '\n';
  }

  return result;
};

ReactState.prototype.printHtml = function(loc) {
  return this.printHtmlRec(loc, '');
};

ReactState.prototype.printHtmlRec = function(loc, indent) {
  var shape = this.shapeMap[loc];
  var comp = shape.mountedComp.comp;
  var props = comp.props;
  var result = '';
  var tag;

  if (typeof props.render === 'function') {
    // this should be somewhere else
    props.$$instance.props = props;
    var elt = props.render(props);
    tag = elt.type + ':' + comp.type.name;
  } else {
    tag = shape.mountedComp.comp.type;
  }

  if (tag !== 'plaintext') {
    result += indent + '<' + tag + '@' + loc;
    for (var key in props) {
      if (key === 'children' || key === 'render' || key === '$$instance') continue;
      result += ' ' + key + '=' + '"' + props[key] + '"';
    }
    result += '>\n';

    for (var i = 0; i < shape.childLocs.length; i++) {
      result += this.printHtmlRec(shape.childLocs[i], indent + '  ');
    }

    result += indent + '</' + tag + '>\n';
  } else {
    result += indent + shape.mountedComp.comp.props.text + '\n';
  }

  return result;
};

function CompDesc(type, props, children) {
  this.type = type;
  this.props = props;
  this.children = children;
}

CompDesc.prototype.toString = function(heap, loc, childLocs) {
  if (this.type === 'plaintext') {
    return this.props.text;
  }

  var type;
  if (typeof this.type === 'string') {
    type = this.type;
  } else {
    type = this.type.name;
  }

  var result = '<' + type + ' @' + loc + '';
  for (var key in this.props) {
    if (key === 'children' || key === 'render' || key === '$$instance') continue;
    result += ' ' + key + '=' + '"' + this.props[key] + '"';
  }
  result += '>';

  for (var i = 0; i < childLocs.length; i++) {
    result += '@' + childLocs[i];
    if (i < childLocs.length - 1) result += ', ';
  }
  result += '</' + type + '>';
  return result;
}

function MountedCompDesc(comp, loc) {
  this.comp = comp;
  this.loc = loc;
}

MountedCompDesc.prototype.toString = function(heap, childLocs) {
  return this.comp.toString(heap, this.loc, childLocs);
}

export default React;
var Component = React.Component;
export { Component as Component, ReactState };

