  function testcase() 
  {
    function callbackfn(val, idx, obj) 
    {
      return obj instanceof Number;
    }
    try
{      Number.prototype[0] = 1;
      Number.prototype.length = 1;
      var testResult = Array.prototype.map.call(2.5, callbackfn);
      return testResult[0] === true;}
    finally
{      delete Number.prototype[0];
      delete Number.prototype.length;}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  