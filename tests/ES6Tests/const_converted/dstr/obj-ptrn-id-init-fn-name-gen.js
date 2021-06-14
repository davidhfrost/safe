function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
assert.notSameValue = function (actual, expected, message) { return actual !== expected; }
// This file was procedurally generated from the following sources:
// - src/dstr-binding/obj-ptrn-id-init-fn-name-gen.case
// - src/dstr-binding/default/const-stmt.template
/*---
description: SingleNameBinding assigns name to "anonymous" generator functions (`const` statement)
esid: sec-let-and-const-declarations-runtime-semantics-evaluation
features: [generators, destructuring-binding]
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
    6. If Initializer is present and v is undefined, then
       [...]
       d. If IsAnonymousFunctionDefinition(Initializer) is true, then
          i. Let hasNameProperty be HasOwnProperty(v, "name").
          ii. ReturnIfAbrupt(hasNameProperty).
          iii. If hasNameProperty is false, perform SetFunctionName(v,
               bindingId).

---*/

const { gen = function* () {}, xGen = function* x() {} } = {};

var __result1 = assert.sameValue(gen.name, 'gen');
var __result2 = assert.notSameValue(xGen.name, 'xGen');
var __expect1 = true;
var __expect2 = true;
