function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
// This file was procedurally generated from the following sources:
// - src/dstr-binding/ary-ptrn-elem-obj-id-init.case
// - src/dstr-binding/default/const-stmt.template
/*---
description: BindingElement with object binding pattern and initializer is used (`const` statement)
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

    BindingElement : BindingPatternInitializer opt

    [...]
    2. If iteratorRecord.[[done]] is true, let v be undefined.
    3. If Initializer is present and v is undefined, then
       a. Let defaultValue be the result of evaluating Initializer.
       b. Let v be ? GetValue(defaultValue).
    4. Return the result of performing BindingInitialization of BindingPattern
       with v and environment as the arguments.
---*/

const [{ x, y, z } = { x: 44, y: 55, z: 66 }] = [];

var __result1 = assert.sameValue(x, 44);
var __result2 = assert.sameValue(y, 55);
var __result3 = assert.sameValue(z, 66);
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;