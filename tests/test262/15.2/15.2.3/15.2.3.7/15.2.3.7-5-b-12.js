//   TODO getter/setter
//   function testcase() 
//   {
//     var obj = {
//       
//     };
//     var accessed = false;
//     var descObj = {
//       
//     };
//     Object.defineProperty(descObj, "enumerable", {
//       get : (function () 
//       {
//         return true;
//       })
//     });
//     Object.defineProperties(obj, {
//       prop : descObj
//     });
//     for(var property in obj)
//     {
//       if (property === "prop")
//       {
//         accessed = true;
//       }
//     }
//     return accessed;
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   
