//   TODO getter/setter
//   function testcase() 
//   {
//     var obj = {
//       
//     };
//     var proto = {
//       
//     };
//     Object.defineProperty(proto, "configurable", {
//       set : (function () 
//       {
//         
//       })
//     });
//     var Con = (function () 
//     {
//       
//     });
//     Con.prototype = proto;
//     var descObj = new Con();
//     Object.defineProperties(obj, {
//       prop : descObj
//     });
//     var result1 = obj.hasOwnProperty("prop");
//     delete obj.prop;
//     var result2 = obj.hasOwnProperty("prop");
//     return result1 === true && result2 === true;
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   
