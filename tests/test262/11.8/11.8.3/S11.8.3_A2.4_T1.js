  var x = 0;
  {
    var __result1 = (x = 1) <= x !== true;
    var __expect1 = false;
  }
  var x = 1;
  {
    var __result2 = x <= (x = 0) !== false;
    var __expect2 = false;
  }
  