  function testcase() 
  {
    var result = false;
    function callbackfn(val, idx, obj) 
    {
      result = obj instanceof Number;
    }
    try
{      Number.prototype[0] = 1;
      Number.prototype.length = 1;
      Array.prototype.forEach.call(2.5, callbackfn);
      return result;}
    finally
{      delete Number.prototype[0];
      delete Number.prototype.length;}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  