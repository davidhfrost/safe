//   TODO getter/setter
//   function testcase() 
//   {
//     var arr = [];
//     function set_fun(value) 
//     {
//       arr.setVerifyHelpProp = value;
//     }
//     Object.defineProperty(arr, "1", {
//       set : set_fun
//     });
//     try
// {      Object.defineProperties(arr, {
//         "1" : {
//           set : undefined
//         }
//       });
//       return false;}
//     catch (ex)
// {      return (ex instanceof TypeError) && accessorPropertyAttributesAreCorrect(arr, "1", undefined, set_fun, "setVerifyHelpProp", false, 
//       false);}
// 
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   
