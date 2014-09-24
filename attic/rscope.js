/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

function rescale (pdi, pli, technique)
{
  function abort () { return false; }
  var de = J.dom ("canvas"); 
  de.width = 555;
  de.height = 275; 
  var dx = de.getContext ("2d"); 
  var di = dx.createImageData (de.width, de.height); 
  Rescale.image (technique.filter, pdi, di, abort, function (abort, di)
  {
    dx.putImageData (di, 0, 0); 
  });
  J.ev (de, "click", function (e){ window.open (de.toDataURL("image/png"));});
  var le = J.dom ("canvas"); 
  le.width = 555; 
  le.height = 15; 
  var lx = le.getContext ("2d"); 
  var li = lx.createImageData (le.width, le.height);
  J.ev (le, "click", function (e){ window.open (le.toDataURL("image/png"));});
  Rescale.image (technique.filter, pli, li, abort, function (abort, li)
  { lx.putImageData (li, 0, 0); }); 
  return J.dom ("div", J.dom ("h1", technique.title), de, le);
}

function image_data (i)
{
  var se = J.dom ("canvas"); 
  se.width = i.width; 
  se.height = i.height; 
  var sx = se.getContext ("2d");
  sx.drawImage (i, 0, 0); 
  return sx.getImageData (0, 0, i.width, i.height); 
}

function ui (techniques) 
{
  var pdi = new Image (); pdi.src = "images/pd.png";
  var pli = new Image (); pli.src = "images/pl.png";

  
  pdi.onload = function () 
  {
    pli.onload = function () 
    {
      document.body.appendChild (
      J.dom ("div", J.map (J.app (rescale, 
                                  image_data (pdi), 
                                  image_data (pli)), techniques))); 
    }
  }
}

function main () 
{
  var techniques = [ 
    { title : "Box (nearest neighbor)",
      filter : Rescale.Filter.Box },
    { title : "Linear",
      filter : Rescale.Filter.Linear },
    { title : "B-spline (cubic)",
      filter : Rescale.Filter.B_spline },
    { title : "Catmull-Rom (cubic)",
      filter : Rescale.Filter.Catmull_rom },
    { title : "Lanczos 3", 
      filter : Rescale.Filter.Lanczos_3 }];
  
  (ui (techniques)); 
}

window.onload = main;
window.J = J

