  function foo() 
  {
    ['z', ].forEach((function () 
    {
      Object.freeze(Array.prototype.forEach);
    }));
  }
  foo();
  