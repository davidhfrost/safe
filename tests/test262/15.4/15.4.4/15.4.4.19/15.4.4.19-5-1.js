var __globalObject = __Global;
function fnGlobalObject() {
	return __globalObject;
}
  function testcase() 
  {
    try
{      fnGlobalObject()._15_4_4_19_5_1 = true;
      var _15_4_4_19_5_1 = false;
      function callbackfn(val, idx, obj) 
      {
        return this._15_4_4_19_5_1;
      }
      var srcArr = [1, ];
      var resArr = srcArr.map(callbackfn);
      if (resArr[0] === true)
        return true;
      return false;}
    finally
{      delete fnGlobalObject()._15_4_4_19_5_1;}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
