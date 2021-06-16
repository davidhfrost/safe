function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
assert.throws = function (error, func, message) { try{ func(); return false; } catch(e){ return e instanceof error;}}
// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-prop-id-init-skipped.case
// - src/dstr-binding/default/const-stmt.template
/*---
description: Destructuring initializer is not evaluated when value is not `undefined` (`const` statement)
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

    13.3.3.7 Runtime Semantics: KeyedBindingInitialization

    BindingElement : BindingPattern Initializeropt

    [...]
    3. If Initializer is present and v is undefined, then
    [...]
---*/
var initCount = 0;
function counter() {
  initCount += 1;
}

const { s: t = counter(), u: v = counter(), w: x = counter(), y: z = counter() } = { s: null, u: 0, w: false, y: '' };

var __result1 = assert.sameValue(t, null);
var __result2 = assert.sameValue(v, 0);
var __result3 = assert.sameValue(x, false);
var __result4 = assert.sameValue(z, '');
var __result5 = assert.sameValue(initCount, 0);

var __result6 = assert.throws(ReferenceError, function() {
  s;
});
var __result7 = assert.throws(ReferenceError, function() {
  u;
});
var __result8 = assert.throws(ReferenceError, function() {
  w;
});
var __result9 = assert.throws(ReferenceError, function() {
  y;
});
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;
var __expect4 = true;
var __expect5 = true;
var __expect6 = true;
var __expect7 = true;
var __expect8 = true;
var __expect9 = true;
