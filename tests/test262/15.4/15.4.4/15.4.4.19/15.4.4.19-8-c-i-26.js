  function testcase() 
  {
    function callbackfn(val, idx, obj) 
    {
      if (idx === 0)
      {
        return val === 9;
      }
      else
        if (idx === 1)
        {
          return val === 11;
        }
        else
        {
          return false;
        }
    }
    var func = (function (a, b) 
    {
      return Array.prototype.map.call(arguments, callbackfn);
    });
    var testResult = func(9, 11);
    return testResult[0] === true && testResult[1] === true;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  