  function testcase() 
  {
    var accessed = false;
    function callbackfn(val, idx, obj) 
    {
      accessed = true;
      return null;
    }
    var obj = {
      0 : 11,
      length : 1
    };
    var newArr = Array.prototype.filter.call(obj, callbackfn);
    return newArr.length === 0 && accessed;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  