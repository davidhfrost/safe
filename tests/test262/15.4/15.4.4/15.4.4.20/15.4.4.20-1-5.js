  function testcase() 
  {
    function callbackfn(val, idx, obj) 
    {
      return obj instanceof Number;
    }
    try
{      Number.prototype[0] = 1;
      Number.prototype.length = 1;
      var newArr = Array.prototype.filter.call(2.5, callbackfn);
      return newArr[0] === 1;}
    finally
{      delete Number.prototype[0];
      delete Number.prototype.length;}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  