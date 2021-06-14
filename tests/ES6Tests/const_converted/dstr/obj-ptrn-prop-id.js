function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
assert.throws = function (error, func, message) { try{ func(); return false; } catch(e){ return e instanceof error;}}
// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-prop-id.case
// - src/dstr-binding/default/const-stmt.template
/*---
description: Binding as specified via property name and identifier (`const` statement)
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

    SingleNameBinding : BindingIdentifier Initializeropt

    [...]
    8. Return InitializeReferencedBinding(lhs, v).
---*/

const { x: y } = { x: 23 };

var __result1 = assert.sameValue(y, 23);
var __result2 = assert.throws(ReferenceError, function() {
  x;
});
var __expect1 = true;
var __expect2 = true;
