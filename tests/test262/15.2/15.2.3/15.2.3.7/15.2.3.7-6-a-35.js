//   TODO getter/setter
//   function testcase() 
//   {
//     var obj = {
//       
//     };
//     var getFun = (function () 
//     {
//       return 10;
//     });
//     var setFun = (function (value) 
//     {
//       obj.setVerifyHelpProp = value;
//     });
//     Object.defineProperties(obj, {
//       prop : {
//         set : setFun,
//         get : getFun,
//         configurable : true
//       }
//     });
//     return accessorPropertyAttributesAreCorrect(obj, "prop", getFun, setFun, "setVerifyHelpProp", false, 
//     true);
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   
