  function testcase() 
  {
    function callbackfn(val, idx, obj) 
    {
      return val > 10;
    }
    var obj = {
      0 : 11,
      length : 0
    };
    var newArr = Array.prototype.map.call(obj, callbackfn);
    return newArr.length === 0;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  