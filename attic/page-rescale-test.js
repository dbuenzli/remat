/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/



/* Image and screen resolution. */

var csspx_per_cm = 38; /* 1cm = 38 css px  */
function css_px_dim (i) 
{ 
  return { w : Math.round ((i.width / i.ppcm) * csspx_per_cm), 
           h : Math.round ((i.height / i.ppcm) * csspx_per_cm) }
}

function ui_screen_info ()
{
  var r = window.devicePixelRatio || 1;
  var txt = (r * csspx_per_cm) + " ppcm";
  txt += (window.devicePixelRatio) ? "" : " (CSS pixel fallback)";
  return txt;
}

function device_pixel_ratio ()
{
  if (!window.devicePixelRatio) 
  { window.console.log 
    ("Warning. window.devicePixelRatio is undefined, assuming 1");
  }
  return (window.devicePixelRatio || 1); 
}

/* Remember ui state */

function get_prefs ()
{
  var selected = window.localStorage.selected_filter;
  var scale = window.localStorage.scale;
  var srgb = window.localStorage.srgb;
  return { selected_filter : (selected) ? window.JSON.parse (selected) : 0, 
           scale : (scale) ? window.JSON.parse (scale) : 1.0,
           srgb : (srgb) ? window.JSON.parse (srgb) : false };
}

function save_prefs (selected, scale, srgb)
{
  window.localStorage.selected_filter = window.JSON.stringify (selected); 
  window.localStorage.scale = window.JSON.stringify (scale);
  window.localStorage.srgb = window.JSON.stringify (srgb);
}

/* UI */

function now () { return new Date().getTime (); }

function ui_rescale_spec (filters, prefs, on_spec_change)
{
  function filter_opt (j, filter)
  { return J.dom ("option", { value : j }, filter.name); }

  var opts = J.mapi (filter_opt, filters);
  var filter_sel = J.dom ("select", 
                          { selectedIndex : prefs.selected_filter }, opts);
  var scale = J.dom ("input", 
                     { type : "number", 
                       min : 1,
                       max : 1600,
                       value : (prefs.scale * 100 | 0) }); 

  var srgb = J.dom ("input", { type : "checkbox", checked : prefs.srgb }); 
  var form = J.dom ("form", 
                    J.dom ("label", "Filter: ", filter_sel), " ",
                    J.dom ("label", "Scale: ", scale),
                    J.dom ("label", srgb, " sRGB conversion"));
                    
  var last = null; 
  function on_change (e) 
  { 
    var now = { abort : false }; 
    function abort () { return now.abort; }
    if (last) last.abort = true; 
    last = now; 
    var sv = scale.value / 100;
    save_prefs (filter_sel.selectedIndex, sv, srgb.checked);
    on_spec_change (filters [filter_sel.selectedIndex], sv, srgb.checked, 
                    abort);
  }
  J.ev (form, "change", on_change);
  J.ev (form, "submit", function (e) { e.preventDefault(); });
  window.setTimeout(on_change, 0, null);
  return form;
}

function ui_rescale (old, img, filter, scale, srgb, abort)
{
  if (abort ()) return; 
  var dim = css_px_dim (img); 
  var w = Math.round (dim.w * scale); var h = Math.round (dim.h * scale);
  var de = J.dom ("canvas"); 
  J.set_css_dim (de, w, h); 
  de.width = Math.round (w * device_pixel_ratio ()); 
  de.height = Math.round (h * device_pixel_ratio ()); 
  var dx = de.getContext ("2d"); 
  var di = dx.createImageData (de.width, de.height); 
  var start = now ();
  var stats = "";

  function finish ()
  {
    dx.putImageData (di, 0, 0);
    var fresh = J.dom ("div", J.dom ("div", { "class" : "stats"}, stats), de); 
    fresh.id = old.id;
    old.parentElement.replaceChild (fresh, old);
  }

  Rescale.image (filter.filter, img, di, abort, function (abort, di)
  {
    if (abort ()) { Debug.log ("rescale aborted"); return};
    var rescale_stamp = now ();
    stats = 
      "source: " + img.width + "x" + img.height + " @ " + img.ppcm + 
      " ppcm  rendered at " + de.width + "x" + de.height + " @ " + 
      ui_screen_info () + 
      " in " + (rescale_stamp - start) + "ms"
    if (!srgb) finish (); 
    else
    {
      Rescale.linear_to_sRGB (di, abort, function (abort, di)
     {
       var linear_stamp = now ();
       stats += "; sRGB conversion in " + (linear_stamp - rescale_stamp) + 
         "ms";
       finish ();
      }); 
    }
  });
}

function inject_rescale (id, i, filter, scale, srgb, abort)
{
  if (abort ()) return; 
  Debug.log ("id: %s", id);
  var old = J.$(id); if (!old) Debug.log ("no old");
  ui_rescale (old, i, filter, scale, srgb, abort); 
}

function image_data (i, k)
{
  var img = new Image (); img.src = i.uri; img.ppcm = i.ppcm;
  img.onload = function ()
  {
    var se = J.dom ("canvas"); 
    se.width = img.width; 
    se.height = img.height; 
    var sx = se.getContext("2d"); 
    sx.drawImage (img, 0, 0); 
    var si = sx.getImageData (0, 0, img.width, img.height); 
    si.ppcm = img.ppcm;
    k (si);
  }
}

function main (techniques) 
{
  var prefs = get_prefs (); 
  var img = { uri : "images/lra_1925_04_25_0002.png", ppcm : 118 }
  var filters = [ 
    { name : "Binary pixel mixing", 
      filter : Rescale.Filter.Binary_pixel_mixing },
    { name : "Binary subpixel mixing (3-tap filter)", 
      filter : Rescale.Filter.Binary_subpixel_mixing3 },
    { name : "Binary supixel mixing (5-tap filter)", 
      filter : Rescale.Filter.Binary_subpixel_mixing5 },
    { name : "Linear",
      filter : Rescale.Filter.Linear },
    { name : "Box (nearest neighbor)",
      filter : Rescale.Filter.Box },
    { name : "B-spline (cubic)",
      filter : Rescale.Filter.B_spline },
    { name : "Catmull-Rom (cubic)",
      filter : Rescale.Filter.Catmull_rom },
    { name : "Lanczos 3", 
      filter : Rescale.Filter.Lanczos_3 }];
  
  if (prefs.selected_filter > filters.length - 1) prefs.selected_filter = 0; 

  image_data (img, function (i) 
  {
    
    var result = J.dom ("div", { id : "result" });
    var on_change = J.app (inject_rescale, result.id, i);
    var ui = 
      J.dom ("div", ui_rescale_spec (filters, prefs, on_change), result); 
    document.body.appendChild (ui); 
  });
}

window.onload = main;
window.J = J

