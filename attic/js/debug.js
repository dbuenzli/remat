/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** @define {boolean} @ignore */
var DEBUG = true;

/** Debugging tools. */

var Debug = function () 
{
  var console = null; 
  var logf = null;
  switch (typeof window.console.log)
  {
  case "function": console = window.console; logf = window.console.log; break;
  case "object": // IE
    console = null;
    if (Function.prototype.bind) 
      logf = Function.prototype.bind.call(window.console.log, window.console);
    break;
  };
  var log =
    (DEBUG && logf) ?
    function () { logf.apply(console, arguments); } : function () {}; 
  
  return { 
    /** `log (m)` logs `m` in the console of browsers that support it. */
    log : log 
  };  
} ();
