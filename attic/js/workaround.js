/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** The Workaround module handles browser compatibility workarounds., */

var Workaround = function () 
{  
  function opera_history () 
  {
//    document.getElementsByTagName("base")[0].href =
//      document.getElementsByTagName("base")[0].href;
  }
  
  var transitionend = function () 
  {
    var props = 
      { 'transition':'transitionend',
        'OTransition':'oTransitionEnd',
        'MSTransition':'msTransitionEnd',
        'MozTransition':'transitionend',
        'WebkitTransition':'webkitTransitionEnd' }; 
    var e = document.createElement ("dummy");  
    for (var p in props) if (e.style[p] !== undefined) return props[p];
    return null;
  } (); 

  return { 
    /** 
     * Hack for HTML5 history relative link bug present in Opera < 11.60.
     * Invoke after pushState.
     * http://my.opera.com/community/forums/topic.dml?id=1185462
     */
    opera_history : opera_history,
    
    /** 
     * `transitionend` is the browser specific CSS3 `transitionend`
     *  event name.
     */
    transitionend : transitionend  }; 
} ();
