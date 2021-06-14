function assert(param) { return Boolean(param)}
assert.sameValue = function (actual, expected, message) { return actual === expected; }
// Copyright (C) 2016 the V8 project authors. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.
/*---
esid: sec-let-and-const-declarations-runtime-semantics-evaluation
es6id: 13.3.1.4
description: Returns an empty completion
info: |
  LexicalDeclaration : LetOrConst BindingList ;

  1. Let next be the result of evaluating BindingList.
  2. ReturnIfAbrupt(next).
  3. Return NormalCompletion(empty).
---*/

var __result1 = assert.sameValue(
  eval('const test262id1 = 1;'), undefined, 'Single declaration'
);
var __result2 = assert.sameValue(
  eval('const test262id2 = 2, test262id3 = 3;'),
  undefined,
  'Multiple declarations'
);

var __result3 = assert.sameValue(eval('4; const test262id5 = 5;'), 4);
var __result4 = assert.sameValue(eval('6; let test262id7 = 7, test262id8 = 8;'), 6);
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;
var __expect4 = true;
