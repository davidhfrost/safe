function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
// This file was procedurally generated from the following sources:
// - src/dstr-binding/ary-ptrn-elem-id-init-skipped.case
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

    13.3.3.6 Runtime Semantics: IteratorBindingInitialization

    SingleNameBinding : BindingIdentifier Initializeropt

    [...]
    6. If Initializer is present and v is undefined, then
       [...]
    7. If environment is undefined, return PutValue(lhs, v).
    8. Return InitializeReferencedBinding(lhs, v).
---*/
var initCount = 0;
function counter() {
  initCount += 1;
}

const [w = counter(), x = counter(), y = counter(), z = counter()] = [null, 0, false, ''];

var __result1 = assert.sameValue(w, null);
var __result2 = assert.sameValue(x, 0);
var __result3 = assert.sameValue(y, false);
var __result4 = assert.sameValue(z, '');
var __result5 = assert.sameValue(initCount, 0);
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;
var __expect4 = true;
var __expect5 = true;
