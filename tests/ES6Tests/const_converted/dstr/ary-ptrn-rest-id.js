function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
assert.notSameValue = function (actual, expected, message) { return actual !== expected; }
// This file was procedurally generated from the following sources:
// - src/dstr-binding/ary-ptrn-rest-id.case
// - src/dstr-binding/default/const-stmt.template
/*---
description: Lone rest element (`const` statement)
esid: sec-let-and-const-declarations-runtime-semantics-evaluation
features: [destructuring-binding]
flags: [generated]
info: |
    LexicalBinding : BindingPattern Initializer

    1. Let rhs be the result of evaluating Initializer.
    2. Let value be GetValue(rhs).
    3. ReturnIfAbrupt(value).
    4. Let env be the running execution context's LexicalEnvironment.
    5. Return the result of performing BindingInitialization for BindingPattern
       using value and env as the arguments.

    13.3.3.6 Runtime Semantics: IteratorBindingInitialization
    BindingRestElement : ... BindingIdentifier
    [...] 3. Let A be ArrayCreate(0). [...] 5. Repeat
       [...]
       f. Let status be CreateDataProperty(A, ToString (n), nextValue).
       [...]
---*/
var values = [1, 2, 3];

const [...x] = values;

var __result1 = assert(Array.isArray(x));
var __result2 = assert.sameValue(x.length, 3);
var __result3 = assert.sameValue(x[0], 1);
var __result4 = assert.sameValue(x[1], 2);
var __result5 = assert.sameValue(x[2], 3);
var __result6 = assert.notSameValue(x, values);
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;
var __expect4 = true;
var __expect5 = true;
var __expect6 = true;
