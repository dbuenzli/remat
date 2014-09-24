/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** Server request module. */ 

var Request = function () 
{

  function http ()
  {
    if (window.XMLHttpRequest) return new XMLHttpRequest ();
    if (window.ActiveXObject) return new ActiveXObject("Microsoft.XMLHTTP");
    Debug.log ("No HttpRequest object found");
    return null;
  }
  
  function get (uri, k) 
  {
    var r = http (); if (!r) return null;
    function kontinue () 
    { if (r.readyState == 4 /* DONE */) k(r.status, r.responseText); }
    r.onreadystatechange = kontinue;
    r.open ("GET", uri, true);
    r.send ();
    return function () { r.abort (); }
  }

  return { 
    /** 
     * `get (uri, k)` makes a GET on `uri` and returns the result to `[k]`.
     * Returns a closure that if invoked aborts the request or `null` if
     * HTTP requests cannot be made. 
     */
    get : get 
  };
} ();
