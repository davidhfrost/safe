//   TODO dataPropertyAttributesAreCorrect
//   function testcase() 
//   {
//     var arg;
//     (function fun(a, b, c) 
//     {
//       arg = arguments;
//     })(0, 1, 2);
//     Object.defineProperty(arg, "0", {
//       value : 0,
//       writable : false,
//       configurable : false
//     });
//     try
// {      Object.defineProperties(arg, {
//         "0" : {
//           value : 10
//         }
//       });
//       return false;}
//     catch (e)
// {      return (e instanceof TypeError) && dataPropertyAttributesAreCorrect(arg, "0", 0, false, true, false);}
// 
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   
