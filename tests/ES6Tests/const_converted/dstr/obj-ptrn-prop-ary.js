function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
assert.throws = function (error, func, message) { try{ func(); return false; } catch(e){ return e instanceof error;}}
// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-prop-ary.case
// - src/dstr-binding/default/const-stmt.template
/*---
description: Object binding pattern with "nested" array binding pattern not using initializer (`const` statement)
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

    [...]
    3. If Initializer is present and v is undefined, then
       [...]
    4. Return the result of performing BindingInitialization for BindingPattern
       passing v and environment as arguments.
---*/

const { w: [x, y, z] = [4, 5, 6] } = { w: [7, undefined, ] };

var __result1 = assert.sameValue(x, 7);
var __result2 = assert.sameValue(y, undefined);
var __result3 = assert.sameValue(z, undefined);

var __result4 = assert.throws(ReferenceError, function() {
  w;
});
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;
var __expect4 = true;
