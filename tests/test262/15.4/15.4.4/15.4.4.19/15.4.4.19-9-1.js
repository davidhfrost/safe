  function testcase() 
  {
    function callbackfn(val, idx, obj) 
    {
      return true;
    }
    var srcArr = [1, 2, 3, 4, 5, ];
    srcArr.map(callbackfn);
    if (srcArr[0] === 1 && srcArr[1] === 2 && srcArr[2] === 3 && srcArr[3] === 4 && srcArr[4] === 5)
    {
      return true;
    }
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  