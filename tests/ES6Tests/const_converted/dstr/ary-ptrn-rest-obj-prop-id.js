function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
// This file was procedurally generated from the following sources:
// - src/dstr-binding/ary-ptrn-rest-obj-prop-id.case
// - src/dstr-binding/default/const-stmt.template
/*---
description: Rest element containing an object binding pattern (`const` statement)
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

    BindingRestElement : ... BindingPattern

    1. Let A be ArrayCreate(0).
    [...]
    3. Repeat
       [...]
       b. If iteratorRecord.[[done]] is true, then
          i. Return the result of performing BindingInitialization of
             BindingPattern with A and environment as the arguments.
       [...]
---*/
let length = "outer";

const [...{ 0: v, 1: w, 2: x, 3: y, length: z }] = [7, 8, 9];

var __result1 = assert.sameValue(v, 7);
var __result2 = assert.sameValue(w, 8);
var __result3 = assert.sameValue(x, 9);
var __result4 = assert.sameValue(y, undefined);
var __result5 = assert.sameValue(z, 3);

var __result6 = assert.sameValue(length, "outer", "the length prop is not set as a binding name");
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;
var __expect4 = true;
var __expect5 = true;
var __expect6 = true;
