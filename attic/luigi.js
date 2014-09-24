/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** Luigi main. */

var Luigi = function () 
{
  var init = false;
  var luigi_conf = 
    {
      base : "/luigi/",
      langs : { "en": "en", "fr" : "fr" },
      library : { "en" : "Digital CIRA", "fr" : "CIRA numérique" }
    };
  
  function main () 
  { 
    if (!init) 
    {
      init = true;
      var ui = Ui.create (luigi_conf, window.location); 
      if (DEBUG)
      {
        // Convenient global environment pollution.
        window.luigi_conf = luigi_conf;
        window.ui = ui; 
        window.J = J;
        window.Workaround = Workaround;
        window.Debug = Debug;
        window.Request = Request;
        window.Doc = Doc;
        window.Ui = Ui;
      }
    }
  }
  document.addEventListener("DOMContentLoaded", main, false);
} (); 



