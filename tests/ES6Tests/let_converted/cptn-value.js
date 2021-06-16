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
  eval('let test262id1;'), undefined, 'Single declaration without initializer'
);
var __result2 = assert.sameValue(
  eval('let test262id2 = 2;'),
  undefined,
  'Single declaration bearing initializer'
);
var __result3 = assert.sameValue(
  eval('let test262id3 = 3, test262id4;'),
  undefined,
  'Multiple declarations, final without initializer'
);
var __result4 = assert.sameValue(
  eval('let test262id5, test262id6 = 6;'),
  undefined,
  'Multiple declarations, final bearing initializer'
);

var __result5 = assert.sameValue(eval('7; let test262id8;'), 7);
var __result6 = assert.sameValue(eval('9; let test262id10 = 10;'), 9);
var __result7 = assert.sameValue(eval('11; let test262id12 = 12, test262id13;'), 11);
var __result8 = assert.sameValue(eval('14; let test262id15, test262id16 = 16;'), 14);
var __expect1 = true;
var __expect2 = true;
var __expect3 = true;
var __expect4 = true;
var __expect5 = true;
var __expect6 = true;
var __expect7 = true;
var __expect8 = true;
