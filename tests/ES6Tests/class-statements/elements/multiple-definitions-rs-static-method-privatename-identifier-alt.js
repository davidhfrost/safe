// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Collection of assertion functions used throughout test262
defines: [assert]
---*/


function assert(mustBeTrue, message) {
    if (mustBeTrue === true) {
        return;
    }

    if (message === undefined) {
        message = 'Expected true but got ' + assert._toString(mustBeTrue);
    }
    throw (message);
}

assert._isSameValue = function (a, b) {
    if (a === b) {
        // Handle +/-0 vs. -/+0
        return a !== 0 || 1 / a === 1 / b;
    }

    // Handle NaN vs. NaN
    return a !== a && b !== b;
};

assert.sameValue = function (actual, expected, message) {
    try {
        if (assert._isSameValue(actual, expected)) {
            return;
        }
    } catch (error) {
        throw (message + ' (_isSameValue operation threw) ' + error);
    }

    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    message += 'Expected SameValue(«' + assert._toString(actual) + '», «' + assert._toString(expected) + '») to be true';

    throw (message);
};

assert.notSameValue = function (actual, unexpected, message) {
    if (!assert._isSameValue(actual, unexpected)) {
        return;
    }

    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    message += 'Expected SameValue(«' + assert._toString(actual) + '», «' + assert._toString(unexpected) + '») to be false';

    throw (message);
};

assert.throws = function (expectedErrorConstructor, func, message) {
    if (typeof func !== "function") {
        throw ('assert.throws requires two arguments: the error constructor ' +
            'and a function to run');
    }
    if (message === undefined) {
        message = '';
    } else {
        message += ' ';
    }

    try {
        func();
    } catch (thrown) {
        if (typeof thrown !== 'object' || thrown === null) {
            message += 'Thrown value was not an object!';
            throw (message);
        } else if (thrown.constructor !== expectedErrorConstructor) {
            message += 'Expected a ' + expectedErrorConstructor.name + ' but got a ' + thrown.constructor.name;
            throw (message);
        }
        return;
    }

    message += 'Expected a ' + expectedErrorConstructor.name + ' to be thrown but no exception was thrown at all';
    throw (message);
};

assert._toString = function (value) {
    try {
        if (value === 0 && 1 / value === -Infinity) {
            return '-0';
        }

        return String(value);
    } catch (err) {
        if (err.name === 'TypeError') {
            return Object.prototype.toString.call(value);
        }

        throw err;
    }
};

// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Collection of functions used to safely verify the correctness of
    property descriptors.
defines:
  - verifyProperty
  - verifyEqualTo
  - verifyWritable
  - verifyNotWritable
  - verifyEnumerable
  - verifyNotEnumerable
  - verifyConfigurable
  - verifyNotConfigurable
---*/

// @ts-check

/**
 * @param {object} obj
 * @param {string|symbol} name
 * @param {PropertyDescriptor|undefined} desc
 * @param {object} [options]
 * @param {boolean} [options.restore]
 */
function verifyProperty(obj, name, desc, options) {
    assert(
        arguments.length > 2,
        'verifyProperty should receive at least 3 arguments: obj, name, and descriptor'
    );

    var originalDesc = Object.getOwnPropertyDescriptor(obj, name);
    var nameStr = String(name);

    // Allows checking for undefined descriptor if it's explicitly given.
    if (desc === undefined) {
        assert.sameValue(
            originalDesc,
            undefined,
            "obj['" + nameStr + "'] descriptor should be undefined"
        );

        // desc and originalDesc are both undefined, problem solved;
        return true;
    }

    assert(
        Object.prototype.hasOwnProperty.call(obj, name),
        "obj should have an own property " + nameStr
    );

    assert.notSameValue(
        desc,
        null,
        "The desc argument should be an object or undefined, null"
    );

    assert.sameValue(
        typeof desc,
        "object",
        "The desc argument should be an object or undefined, " + String(desc)
    );

    var failures = [];

    if (Object.prototype.hasOwnProperty.call(desc, 'value')) {
        if (!isSameValue(desc.value, originalDesc.value)) {
            failures.push("descriptor value should be " + desc.value);
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'enumerable')) {
        if (desc.enumerable !== originalDesc.enumerable ||
            desc.enumerable !== isEnumerable(obj, name)) {
            failures.push('descriptor should ' + (desc.enumerable ? '' : 'not ') + 'be enumerable');
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'writable')) {
        if (desc.writable !== originalDesc.writable ||
            desc.writable !== isWritable(obj, name)) {
            failures.push('descriptor should ' + (desc.writable ? '' : 'not ') + 'be writable');
        }
    }

    if (Object.prototype.hasOwnProperty.call(desc, 'configurable')) {
        if (desc.configurable !== originalDesc.configurable ||
            desc.configurable !== isConfigurable(obj, name)) {
            failures.push('descriptor should ' + (desc.configurable ? '' : 'not ') + 'be configurable');
        }
    }

    assert(!failures.length, failures.join('; '));

    if (options && options.restore) {
        Object.defineProperty(obj, name, originalDesc);
    }

    return true;
}

function isConfigurable(obj, name) {
    var hasOwnProperty = Object.prototype.hasOwnProperty;
    try {
        delete obj[name];
    } catch (e) {
        if (!(e instanceof TypeError)) {
            throw ("Expected TypeError, got " + e)
        }
    }
    return !hasOwnProperty.call(obj, name);
}

function isEnumerable(obj, name) {
    var stringCheck = false;

    if (typeof name === "string") {
        for (var x in obj) {
            if (x === name) {
                stringCheck = true;
                break;
            }
        }
    } else {
        // skip it if name is not string, works for Symbol names.
        stringCheck = true;
    }

    return stringCheck &&
        Object.prototype.hasOwnProperty.call(obj, name) &&
        Object.prototype.propertyIsEnumerable.call(obj, name);
}

function isSameValue(a, b) {
    if (a === 0 && b === 0) return 1 / a === 1 / b;
    if (a !== a && b !== b) return true;

    return a === b;
}

var __isArray = Array.isArray;
function isWritable(obj, name, verifyProp, value) {
    var unlikelyValue = __isArray(obj) && name === "length" ?
        Math.pow(2, 32) - 1 :
        "unlikelyValue";
    var newValue = value || unlikelyValue;
    var hadValue = Object.prototype.hasOwnProperty.call(obj, name);
    var oldValue = obj[name];
    var writeSucceeded;

    try {
        obj[name] = newValue;
    } catch (e) {
        if (!(e instanceof TypeError)) {
            throw ("Expected TypeError, got " + e);
        }
    }

    writeSucceeded = isSameValue(obj[verifyProp || name], newValue);

    // Revert the change only if it was successful (in other cases, reverting
    // is unnecessary and may trigger exceptions for certain property
    // configurations)
    if (writeSucceeded) {
        if (hadValue) {
            obj[name] = oldValue;
        } else {
            delete obj[name];
        }
    }

    return writeSucceeded;
}

function verifyEqualTo(obj, name, value) {
    if (!isSameValue(obj[name], value)) {
        throw ("Expected obj[" + String(name) + "] to equal " + value +
            ", actually " + obj[name]);
    }
}

function verifyWritable(obj, name, verifyProp, value) {
    if (!verifyProp) {
        assert(Object.getOwnPropertyDescriptor(obj, name).writable,
            "Expected obj[" + String(name) + "] to have writable:true.");
    }
    if (!isWritable(obj, name, verifyProp, value)) {
        throw ("Expected obj[" + String(name) + "] to be writable, but was not.");
    }
}

function verifyNotWritable(obj, name, verifyProp, value) {
    if (!verifyProp) {
        assert(!Object.getOwnPropertyDescriptor(obj, name).writable,
            "Expected obj[" + String(name) + "] to have writable:false.");
    }
    if (isWritable(obj, name, verifyProp)) {
        throw ("Expected obj[" + String(name) + "] NOT to be writable, but was.");
    }
}

function verifyEnumerable(obj, name) {
    assert(Object.getOwnPropertyDescriptor(obj, name).enumerable,
        "Expected obj[" + String(name) + "] to have enumerable:true.");
    if (!isEnumerable(obj, name)) {
        throw ("Expected obj[" + String(name) + "] to be enumerable, but was not.");
    }
}

function verifyNotEnumerable(obj, name) {
    assert(!Object.getOwnPropertyDescriptor(obj, name).enumerable,
        "Expected obj[" + String(name) + "] to have enumerable:false.");
    if (isEnumerable(obj, name)) {
        throw ("Expected obj[" + String(name) + "] NOT to be enumerable, but was.");
    }
}

function verifyConfigurable(obj, name) {
    assert(Object.getOwnPropertyDescriptor(obj, name).configurable,
        "Expected obj[" + String(name) + "] to have configurable:true.");
    if (!isConfigurable(obj, name)) {
        throw ("Expected obj[" + String(name) + "] to be configurable, but was not.");
    }
}

function verifyNotConfigurable(obj, name) {
    assert(!Object.getOwnPropertyDescriptor(obj, name).configurable,
        "Expected obj[" + String(name) + "] to have configurable:false.");
    if (isConfigurable(obj, name)) {
        throw ("Expected obj[" + String(name) + "] NOT to be configurable, but was.");
    }
}

function Test262Error(message) {
    this.message = message || "";
}

Test262Error.prototype.toString = function () {
    return "Test262Error: " + this.message;
};

// Copyright (C) 2017 Ecma International.  All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
description: |
    Compare the contents of two arrays
defines: [compareArray]
---*/

function compareArray(a, b) {
    if (b.length !== a.length) {
        return false;
    }

    for (var i = 0; i < a.length; i++) {
        if (!compareArray.isSameValue(b[i], a[i])) {
            return false;
        }
    }
    return true;
}

compareArray.isSameValue = function (a, b) {
    if (a === 0 && b === 0) return 1 / a === 1 / b;
    if (a !== a && b !== b) return true;

    return a === b;
};

compareArray.format = function (array) {
    return "[" + array.map(String).join(', ') + "]";
};

assert.compareArray = function (actual, expected, message) {
    assert(actual != null, "First argument shouldn't be nullish. " + message);
    assert(expected != null, "Second argument shouldn't be nullish. " + message);
    var format = compareArray.format;
    assert(
        compareArray(actual, expected),
        "Expected " + format(actual) + " and " + format(expected) + " to have the same contents. " + message
    );
};
// This file was procedurally generated from the following sources:
// - src/class-elements/rs-static-method-privatename-identifier-alt.case
// - src/class-elements/productions/cls-decl-multiple-definitions.template
/*---
description: Valid Static Method PrivateName (multiple fields definitions)
esid: prod-FieldDefinition
features: [class-static-methods-private, class, class-fields-public]
flags: [generated]
includes: [propertyHelper.js]
info: |
    ClassElement :
      MethodDefinition
      static MethodDefinition
      FieldDefinition ;
      static FieldDefinition ;
      ;

    MethodDefinition :
      ClassElementName ( UniqueFormalParameters ){ FunctionBody }

    ClassElementName :
      PropertyName
      PrivateName

    PrivateName ::
      # IdentifierName

    IdentifierName ::
      IdentifierStart
      IdentifierName IdentifierPart

    IdentifierStart ::
      UnicodeIDStart
      $
      _
      \ UnicodeEscapeSequence

    IdentifierPart::
      UnicodeIDContinue
      $
      \ UnicodeEscapeSequence
      <ZWNJ> <ZWJ>

    UnicodeIDStart::
      any Unicode code point with the Unicode property "ID_Start"

    UnicodeIDContinue::
      any Unicode code point with the Unicode property "ID_Continue"


    NOTE 3
    The sets of code points with Unicode properties "ID_Start" and
    "ID_Continue" include, respectively, the code points with Unicode
    properties "Other_ID_Start" and "Other_ID_Continue".

---*/


class C {
  foo = "foobar";
  m() { return 42 }
  static #$(value) {
    return value;
  }
  static #_(value) {
    return value;
  }
  static #o(value) {
    return value;
  }
  static #℘(value) {
    return value;
  }
  static #ZW_‌_NJ(value) {
    return value;
  }
  static #ZW_‍_J(value) {
    return value;
  }
  m2() { return 39 }
  bar = "barbaz";
  static $(value) {
    return this.#$(value);
  }
  static _(value) {
    return this.#_(value);
  }
  static o(value) {
    return this.#o(value);
  }
  static ℘(value) { // DO NOT CHANGE THE NAME OF THIS FIELD
    return this.#℘(value);
  }
  static ZW_‌_NJ(value) { // DO NOT CHANGE THE NAME OF THIS FIELD
    return this.#ZW_‌_NJ(value);
  }
  static ZW_‍_J(value) { // DO NOT CHANGE THE NAME OF THIS FIELD
    return this.#ZW_‍_J(value);
  }

}

var c = new C();

assert.sameValue(c.m(), 42);
assert.sameValue(Object.hasOwnProperty.call(c, "m"), false);
assert.sameValue(c.m, C.prototype.m);

verifyProperty(C.prototype, "m", {
  enumerable: false,
  configurable: true,
  writable: true,
});

assert.sameValue(c.m2(), 39);
assert.sameValue(Object.hasOwnProperty.call(c, "m2"), false);
assert.sameValue(c.m2, C.prototype.m2);

verifyProperty(C.prototype, "m2", {
  enumerable: false,
  configurable: true,
  writable: true,
});

assert.sameValue(c.foo, "foobar");
assert.sameValue(Object.hasOwnProperty.call(C, "foo"), false);
assert.sameValue(Object.hasOwnProperty.call(C.prototype, "foo"), false);

verifyProperty(c, "foo", {
  value: "foobar",
  enumerable: true,
  configurable: true,
  writable: true,
});

assert.sameValue(c.bar, "barbaz");
assert.sameValue(Object.hasOwnProperty.call(C, "bar"), false);
assert.sameValue(Object.hasOwnProperty.call(C.prototype, "bar"), false);

verifyProperty(c, "bar", {
  value: "barbaz",
  enumerable: true,
  configurable: true,
  writable: true,
});

assert.sameValue(C.$(1), 1);
assert.sameValue(C._(1), 1);
assert.sameValue(C.o(1), 1);
assert.sameValue(C.℘(1), 1); // DO NOT CHANGE THE NAME OF THIS FIELD
assert.sameValue(C.ZW_‌_NJ(1), 1); // DO NOT CHANGE THE NAME OF THIS FIELD
assert.sameValue(C.ZW_‍_J(1), 1); // DO NOT CHANGE THE NAME OF THIS FIELD

